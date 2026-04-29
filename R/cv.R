#' Spatial k-means folds for cross-validation
#'
#' Partitions stations into `k` spatially compact folds by running
#' k-means on their `(X, Y)` coordinates. Each fold is then used in
#' turn as a held-out test set. Compared with random k-fold CV, this
#' isolates whole spatial neighbourhoods, so prediction error reflects
#' genuine spatial extrapolation rather than autocorrelated leakage.
#'
#' This is the most defensible CV scheme for the planned R Journal
#' article on Seoul air-quality interpolation, where the monitoring
#' network is dense in the city centre and sparse in surrounding
#' districts; random k-fold would mask that imbalance.
#'
#' @param stations An `sf` POINT layer (typically the training
#'   monitors) or a `data.frame` with columns `X` and `Y`.
#' @param k Number of folds. Defaults to 5; set lower for small
#'   networks (<= 30 stations).
#' @param seed Optional integer for reproducibility.
#'
#' @return A list of length `k`. Each element is a list with integer
#'   vectors `train` (row indices used to fit) and `test` (row
#'   indices held out).
#' @export
#'
#' @examples
#' \dontrun{
#'   data("stations_demo", package = "aqsurface")
#'   folds <- spatial_kmeans_folds(stations_demo, k = 5, seed = 1)
#' }
spatial_kmeans_folds <- function(stations, k = 5, seed = NULL) {
  xy <- station_xy(stations)
  if (k < 2L) cli::cli_abort("`k` must be >= 2.")
  if (k > nrow(xy)) {
    cli::cli_abort("`k` ({k}) cannot exceed number of stations ({nrow(xy)}).")
  }
  if (!is.null(seed)) set.seed(seed)
  cl <- stats::kmeans(xy, centers = k, nstart = 25)
  idx <- seq_len(nrow(xy))
  lapply(seq_len(k), function(i) {
    list(
      train = idx[cl$cluster != i],
      test  = idx[cl$cluster == i]
    )
  })
}

#' Spatial block folds based on a regular grid
#'
#' Divides the bounding box of `stations` into a regular grid of
#' `block_size_m` metre cells, assigns each station to its cell, and
#' then groups cells into `k` roughly balanced folds via cyclic
#' assignment. Useful when the user wants tight control over the
#' physical scale of the held-out blocks.
#'
#' @inheritParams spatial_kmeans_folds
#' @param block_size_m Block edge length in metres. Default 5000.
#'
#' @return A list of `k` folds, same structure as
#'   [spatial_kmeans_folds()].
#' @export
spatial_block_folds <- function(stations, k = 5, block_size_m = 5000,
                                seed = NULL) {
  xy <- station_xy(stations)
  if (!is.null(seed)) set.seed(seed)
  bx <- floor((xy[, "X"] - min(xy[, "X"])) / block_size_m)
  by <- floor((xy[, "Y"] - min(xy[, "Y"])) / block_size_m)
  cell <- paste(bx, by, sep = "_")
  cells <- unique(cell)
  cells <- sample(cells)
  fold_of_cell <- stats::setNames(
    rep(seq_len(k), length.out = length(cells)),
    cells
  )
  station_fold <- fold_of_cell[cell]
  idx <- seq_len(nrow(xy))
  lapply(seq_len(k), function(i) {
    list(
      train = idx[station_fold != i],
      test  = idx[station_fold == i]
    )
  })
}

#' Run cross-validated predictions for a single target column
#'
#' For each fold, fits the requested algorithm on the training
#' subset, predicts at the held-out monitoring locations, and
#' collects the (observation, prediction) pairs into a long-format
#' tibble suitable for [compute_metrics()].
#'
#' Predictions are evaluated **directly at the test station
#' locations** (`gstat::krige` and `predict.gam` both accept point
#' inputs), so no raster is built per fold; this is dramatically
#' faster than the surface-then-extract path.
#'
#' @param data An `sf` POINT layer with `X`, `Y` columns and the
#'   response column.
#' @param target Character; response column name.
#' @param folds Output of [spatial_kmeans_folds()] or
#'   [spatial_block_folds()].
#' @param algorithm `"uk"` or `"gam"`.
#' @param ... Extra arguments passed to [fit_uk()] or [fit_gam()].
#' @param station_id Identifier column on `data`. Auto-detected if
#'   `NULL` (see [extract_at_stations()]).
#' @param quiet Suppress per-fit warnings (recommended in benchmarks).
#'
#' @return Tibble with columns `station`, `target`, `obs`, `pred`,
#'   `fold`.
#' @export
cv_predict <- function(data, target, folds,
                       algorithm = c("uk", "gam"),
                       ...,
                       station_id = NULL,
                       quiet = TRUE) {
  algorithm <- match.arg(algorithm)
  if (!inherits(data, "sf")) {
    cli::cli_abort("`data` must be an sf POINT layer.")
  }
  station_id <- resolve_station_id(data, station_id)

  out <- vector("list", length(folds))
  for (i in seq_along(folds)) {
    fold <- folds[[i]]
    train <- data[fold$train, , drop = FALSE]
    test  <- data[fold$test,  , drop = FALSE]
    test  <- test[!is.na(test[[target]]), , drop = FALSE]
    if (nrow(test) == 0L) next

    pred <- tryCatch(
      cv_one_fit(train, test, target, algorithm, quiet, ...),
      error = function(e) {
        if (!quiet) cli::cli_warn("Fold {i} failed: {conditionMessage(e)}")
        rep(NA_real_, nrow(test))
      }
    )

    # NB: pre-extract obs before the tibble() call. Inside tibble(),
    # later arguments see earlier-named columns, so writing
    # `target = target, obs = test[[target]]` in one call would mask
    # the function parameter `target` with the just-created column.
    target_name <- target
    obs_vec     <- test[[target_name]]
    station_vec <- sf::st_drop_geometry(test)[[station_id]]
    out[[i]] <- tibble::tibble(
      station = station_vec,
      target  = target_name,
      obs     = obs_vec,
      pred    = pred,
      fold    = i
    )
  }
  dplyr::bind_rows(out)
}

# Internal: fit + predict at test points for a single fold.
cv_one_fit <- function(train, test, target, algorithm, quiet, ...) {
  if (algorithm == "uk") {
    args <- list(data = train, target = target, ...)
    args <- args[intersect(names(args),
                           c("data", "target", "trend", "cutoff",
                             "width", "model", "psill", "nugget",
                             "range", "fit_kappa", "fit_method",
                             "crs"))]
    fit <- if (quiet) {
      suppressWarnings(suppressMessages(do.call(fit_uk, args)))
    } else {
      do.call(fit_uk, args)
    }
    pr <- if (quiet) {
      suppressWarnings(suppressMessages(predict_uk(fit, test)))
    } else {
      predict_uk(fit, test)
    }
    pr$pred
  } else {
    args <- list(data = train, target = target, ...)
    args <- args[intersect(names(args),
                           c("data", "target", "smooth", "k",
                             "method", "extra_terms"))]
    fit <- if (quiet) {
      suppressWarnings(suppressMessages(do.call(fit_gam, args)))
    } else {
      do.call(fit_gam, args)
    }
    pr <- if (quiet) {
      suppressWarnings(suppressMessages(predict_gam(fit, test)))
    } else {
      predict_gam(fit, test)
    }
    pr$pred
  }
}

# Internal: extract X / Y matrix from sf or data.frame.
station_xy <- function(stations) {
  if (inherits(stations, "sf")) {
    coords <- sf::st_coordinates(stations)
    return(matrix(c(coords[, 1], coords[, 2]),
                  ncol = 2, dimnames = list(NULL, c("X", "Y"))))
  }
  if (!all(c("X", "Y") %in% names(stations))) {
    cli::cli_abort("`stations` must have X and Y columns or be sf.")
  }
  as.matrix(stations[, c("X", "Y")])
}
