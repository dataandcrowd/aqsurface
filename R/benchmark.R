#' Benchmark interpolation methods across validation strategies
#'
#' One-call wrapper that runs every requested algorithm against
#' three validation strategies and returns a long-format tibble of
#' metrics ready for plotting:
#' \describe{
#'   \item{`in-sample`}{Fit on `train`, predict at the same training
#'     monitors. Reproduces the legacy RMSE figure but is biased
#'     downward.}
#'   \item{`spatial-cv`}{`k`-fold spatial cross-validation on `train`
#'     using [spatial_kmeans_folds()]. Genuine spatial extrapolation
#'     error.}
#'   \item{`held-out`}{Fit on `train`, predict at every station in
#'     `holdout`. Captures background-to-roadside generalisation.}
#' }
#'
#' Results across all three strategies are returned in one place so
#' the R Journal article's headline figure (RMSE, bias, r broken
#' down by algorithm and strategy) can be reproduced from a single
#' object.
#'
#' @param train An `sf` POINT layer of training monitors carrying
#'   the response columns.
#' @param holdout An `sf` POINT layer of held-out monitors (typically
#'   road-side). Pass `NULL` to skip the held-out strategy.
#' @param targets Character vector of response columns (typically
#'   the output of [decade_columns()]).
#' @param algorithms Character vector; subset of `c("uk", "gam", "rf")`.
#' @param covariates Optional character vector of covariate columns
#'   that the user wants every algorithm to use. When supplied, this
#'   overrides each algorithm's default predictor handling:
#'   * UK: `trend = ~ <covariates>` (kriging with external drift),
#'   * GAM: `extra_terms = covariates`,
#'   * RF: `predictors = c("X", "Y", covariates)`.
#'   Setting `covariates = "station_type"` together with [combine_stations()]
#'   trains all three methods on Fixed + Road monitors with the type
#'   factor as a covariate.
#' @param uk_args A named list of extra arguments forwarded to
#'   [fit_uk()] (e.g. `list(trend = "const", cutoff = 30000)`).
#' @param gam_args A named list of extra arguments forwarded to
#'   [fit_gam()] (e.g. `list(smooth = "tp", method = "REML")`).
#' @param rf_args A named list of extra arguments forwarded to
#'   [fit_rf()] (e.g. `list(num_trees = 500)`).
#' @param cv_folds Number of spatial folds.
#' @param fold_method Either `"kmeans"` (default; recommended) or
#'   `"block"`. Forwarded to [spatial_kmeans_folds()] /
#'   [spatial_block_folds()].
#' @param block_size_m Block size when `fold_method = "block"`.
#' @param stratify_by Optional `train` column (typically
#'   `"station_type"`) ensuring every fold's training set contains
#'   every level of the stratum. Forwarded to
#'   [spatial_kmeans_folds()] / [spatial_block_folds()]. Use this
#'   when `covariates` includes a categorical column with few levels,
#'   to prevent UK with external drift from receiving rank-deficient
#'   designs in some folds.
#' @param seed Optional integer; sets the seed before fold creation
#'   so the benchmark is reproducible.
#' @param progress Show a progress bar.
#'
#' @return A tibble with one row per (`algorithm`, `strategy`,
#'   `target`) combination, plus a final `overall` row per
#'   (`algorithm`, `strategy`) collapsing across targets. Columns:
#'   `algorithm`, `strategy`, `target`, `n`, `rmse`, `mae`, `bias`,
#'   `r`.
#' @export
benchmark_methods <- function(train,
                              holdout = NULL,
                              targets,
                              algorithms = c("uk", "gam"),
                              covariates = NULL,
                              uk_args = list(trend = "const",
                                             cutoff = 30000,
                                             width = 3000,
                                             model = "Ste"),
                              gam_args = list(smooth = "tp",
                                              method = "REML"),
                              rf_args  = list(num_trees = 500),
                              cv_folds = 5,
                              fold_method = c("kmeans", "block"),
                              block_size_m = 5000,
                              stratify_by = NULL,
                              seed = 1L,
                              progress = TRUE) {
  fold_method <- match.arg(fold_method)
  algorithms <- match.arg(algorithms, choices = c("uk", "gam", "rf"),
                          several.ok = TRUE)
  if (!is.null(covariates)) {
    miss_cv <- setdiff(covariates, names(train))
    if (length(miss_cv) > 0L) {
      cli::cli_abort("`train` is missing covariate columns: {.val {miss_cv}}")
    }
    uk_args  <- utils::modifyList(uk_args,
                                  list(trend = stats::as.formula(
                                    paste("~", paste(covariates, collapse = " + ")))))
    gam_args <- utils::modifyList(gam_args,
                                  list(extra_terms = covariates))
    rf_args  <- utils::modifyList(rf_args,
                                  list(predictors = c("X", "Y", covariates)))
  }
  if (!inherits(train, "sf")) {
    cli::cli_abort("`train` must be an sf POINT layer.")
  }
  station_id_train   <- resolve_station_id(train, NULL)
  station_id_holdout <- if (!is.null(holdout))
    resolve_station_id(holdout, NULL) else NULL

  folds <- if (fold_method == "kmeans") {
    spatial_kmeans_folds(train, k = cv_folds, seed = seed,
                         stratify_by = stratify_by)
  } else {
    spatial_block_folds(train, k = cv_folds,
                        block_size_m = block_size_m, seed = seed,
                        stratify_by = stratify_by)
  }

  total <- length(algorithms) * length(targets)
  if (progress) {
    cli::cli_progress_bar("Benchmarking", total = total)
  }

  rows <- list()

  for (alg in algorithms) {
    extra <- switch(alg,
                    uk  = uk_args,
                    gam = gam_args,
                    rf  = rf_args)
    for (tgt in targets) {
      train_t <- train[!is.na(train[[tgt]]), , drop = FALSE]

      # in-sample
      fit_args <- c(list(data = train_t, target = tgt), extra)
      fit_args <- fit_args[!duplicated(names(fit_args))]
      fit <- safe_fit(alg, fit_args)
      if (!is.null(fit)) {
        pred_in <- safe_pred(alg, fit, train_t)
        in_sample <- tibble::tibble(
          algorithm = alg, strategy = "in-sample", target = tgt,
          obs = train_t[[tgt]], pred = pred_in
        )
        rows[[length(rows) + 1L]] <- in_sample
      }

      # spatial CV
      cv_args <- c(list(data = train_t, target = tgt, folds = folds,
                        algorithm = alg,
                        station_id = station_id_train,
                        quiet = TRUE),
                   extra)
      cv_args <- cv_args[!duplicated(names(cv_args))]
      cv_long <- do.call(cv_predict, cv_args)
      if (nrow(cv_long) > 0L) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          algorithm = alg, strategy = "spatial-cv", target = tgt,
          obs = cv_long$obs, pred = cv_long$pred
        )
      }

      # held-out
      if (!is.null(holdout) && !is.null(fit)) {
        ho_t <- holdout[!is.na(holdout[[tgt]]), , drop = FALSE]
        if (nrow(ho_t) > 0L) {
          pred_ho <- safe_pred(alg, fit, ho_t)
          rows[[length(rows) + 1L]] <- tibble::tibble(
            algorithm = alg, strategy = "held-out", target = tgt,
            obs = ho_t[[tgt]], pred = pred_ho
          )
        }
      }

      if (progress) cli::cli_progress_update()
    }
  }
  if (progress) cli::cli_progress_done()

  long <- dplyr::bind_rows(rows)
  if (nrow(long) == 0L) return(long)

  per_target <- compute_metrics(long, by = c("algorithm", "strategy", "target"))
  overall <- compute_metrics(long, by = c("algorithm", "strategy")) |>
    dplyr::mutate(target = "ALL", .before = "n")
  dplyr::bind_rows(per_target, overall) |>
    dplyr::arrange(.data$algorithm, .data$strategy, .data$target)
}

# Internal: tolerant fit dispatch used by benchmark_methods.
safe_fit <- function(algorithm, args) {
  bits <- alg_dispatch(algorithm)
  args <- args[intersect(names(args), bits$keep)]
  tryCatch(
    suppressWarnings(suppressMessages(do.call(bits$fit, args))),
    error = function(e) NULL
  )
}

safe_pred <- function(algorithm, fit, newdata) {
  bits <- alg_dispatch(algorithm)
  pr <- tryCatch(
    suppressWarnings(suppressMessages(bits$pred(fit, newdata))),
    error = function(e) NULL
  )
  if (is.null(pr)) rep(NA_real_, nrow(newdata)) else pr$pred
}

alg_dispatch <- function(algorithm) {
  switch(algorithm,
    uk = list(
      fit  = fit_uk,
      pred = predict_uk,
      keep = c("data", "target", "trend", "cutoff", "width", "model",
               "psill", "nugget", "range", "fit_kappa", "fit_method",
               "crs")
    ),
    gam = list(
      fit  = fit_gam,
      pred = predict_gam,
      keep = c("data", "target", "smooth", "k", "method", "extra_terms")
    ),
    rf = list(
      fit  = fit_rf,
      pred = predict_rf,
      keep = c("data", "target", "predictors", "num_trees",
               "respect_unordered_factors")
    ),
    cli::cli_abort("Unknown algorithm: {.val {algorithm}}")
  )
}
