#' Create a calibration curve
#'
#' Creates ordinary factual calibration when `treats` is `NULL`, delegating
#' directly to [rtichoke::create_calibration_curve()]. For predictions under an
#' intervention, `treats` contains the observed treatment assignments and
#' `intervention` identifies the treatment level whose predicted risks are in
#' `probs`.
#'
#' Intervention calibration reproduces the subgroup coordinates used by
#' `ipeval::ip_score(..., metrics = "calplot")`: predictions are sorted and
#' split into equally sized rank bins using `cut(seq_len(n), breaks = n_bins)`;
#' x is the unweighted mean prediction among all subjects in a bin, while y is
#' the weighted mean outcome among subjects observed under `intervention`.
#'
#' @param probs Named list of predicted probabilities. Names identify models or
#'   prediction series. In intervention mode every series must predict risk
#'   under the same `intervention`.
#' @param reals Binary observed outcomes.
#' @param treats Optional observed treatment assignments.
#' @param intervention Optional treatment level whose counterfactual predictions
#'   are being evaluated. Required when `treats` is supplied.
#' @param weights Optional non-negative observation weights. Used only in
#'   intervention mode. If omitted, observations assigned to `intervention`
#'   receive weight 1.
#' @param n_bins Number of rank-based calibration bins in intervention mode.
#'   Defaults to 10 to match rtichoke's discrete calibration behavior. A
#'   different value, such as 8 for ipeval parity, can be requested explicitly.
#'   The actual number is capped at the number of unique predictions for each
#'   series.
#' @param interactive Passed to rtichoke's calibration renderer.
#' @param type Calibration type. Intervention calibration currently supports
#'   only `"discrete"`.
#' @param ... Additional arguments passed to rtichoke calibration preparation.
#'
#' @return A calibration plot produced by rtichoke.
#' @export
#'
#' @examples
#' set.seed(1)
#' n <- 1000
#'
#' data <- data.frame(L = rnorm(n), P = rnorm(n))
#' data$A <- rbinom(n, 1, plogis(data$L))
#' data$Y <- rbinom(n, 1, plogis(0.1 + 0.5 * data$L + 0.7 * data$P - 2 * data$A))
#'
#' # Predict each subject's risk if treatment were set to A = 0.
#' outcome_model <- glm(Y ~ A + P, data = data, family = "binomial")
#' intervention_data <- transform(data, A = 0)
#' probs <- predict(outcome_model, newdata = intervention_data, type = "response")
#'
#' # Supply inverse-probability weights; rticausal does not estimate them.
#' treatment_model <- glm(A ~ L, data = data, family = "binomial")
#' propensity <- predict(treatment_model, type = "response")
#' weights <- 1 / (1 - propensity)
#'
#' create_calibration_curve(
#'   probs = list("Model" = probs),
#'   reals = data$Y,
#'   treats = data$A,
#'   intervention = 0,
#'   weights = weights
#' )
create_calibration_curve <- function(
  probs,
  reals,
  treats = NULL,
  intervention = NULL,
  weights = NULL,
  n_bins = 10L,
  interactive = TRUE,
  type = "discrete",
  ...
) {
  if (is.null(treats)) {
    if (!is.null(intervention) || !is.null(weights)) {
      stop("intervention and weights require treats.")
    }
    factual_reals <- if (is.list(reals)) reals else list(reals)
    return(rtichoke::create_calibration_curve(
      probs = probs,
      reals = factual_reals,
      interactive = interactive,
      type = type,
      ...
    ))
  }

  if (is.null(intervention) || length(intervention) != 1L || is.na(intervention)) {
    stop("intervention must be a single observed treatment level when treats is supplied.")
  }
  if (!identical(type, "discrete")) {
    stop("Intervention calibration currently supports only type = 'discrete'.")
  }

  prepared <- .prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = intervention,
    weights = weights,
    n_bins = n_bins,
    ...
  )

  if (interactive) {
    rtichoke:::create_plotly_curve_from_calibration_curve_list(prepared, type = type)
  } else {
    rtichoke:::create_ggplot_curve_from_calibration_curve_list(prepared, type = type)
  }
}

.ipeval_calplot_rows <- function(
  probs,
  reals,
  pseudo_i,
  weights,
  n_bins = 10L,
  reference_group = "model"
) {
  n_breaks <- min(n_bins, length(unique(probs)))
  cal <- data.frame(
    obs_outcome = reals,
    pseudo_i = pseudo_i,
    cf_pred = probs,
    ipw = weights
  )
  cal <- cal[order(cal$cf_pred), , drop = FALSE]

  if (n_breaks >= 2L) {
    cal$bin <- cut(seq_len(nrow(cal)), breaks = n_breaks, labels = FALSE)
  } else {
    cal$bin <- 1L
  }
  cal$bin <- factor(cal$bin, levels = seq_len(n_breaks))

  mean_preds <- tapply(cal$cf_pred, cal$bin, mean)
  cal_pseudo <- cal[cal$pseudo_i, , drop = FALSE]
  pseudo_bins <- split(cal_pseudo, cal_pseudo$bin, drop = FALSE)
  mean_obs <- vapply(
    pseudo_bins,
    function(x) {
      if (nrow(x) == 0L) {
        return(NA_real_)
      }
      stats::weighted.mean(x$obs_outcome, x$ipw)
    },
    numeric(1)
  )

  data.frame(
    reference_group = reference_group,
    bin = seq_len(n_breaks),
    x = unname(mean_preds),
    y = unname(mean_obs)
  )
}

.prepare_intervention_calibration <- function(
  probs,
  reals,
  treats,
  intervention,
  weights = NULL,
  n_bins = 10L,
  ...
) {
  if (!is.list(probs) || length(probs) == 0L) {
    stop("probs must be a non-empty list of prediction series.")
  }
  if (is.null(names(probs))) {
    names(probs) <- if (length(probs) == 1L) "model" else paste0("model_", seq_along(probs))
  }
  if (any(names(probs) == "")) {
    stop("probs must not contain empty series names.")
  }

  n <- length(reals)
  if (length(treats) != n || any(vapply(probs, length, integer(1)) != n)) {
    stop("probs, reals, and treats must describe the same observations.")
  }
  if (!all(reals %in% c(0, 1))) {
    stop("reals must be binary (0/1).")
  }
  if (is.null(weights)) {
    weights <- rep(1, n)
  }
  if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) {
    stop("weights must be finite, non-negative, and the same length as reals.")
  }
  if (length(n_bins) != 1L || !is.finite(n_bins) || n_bins < 1 || n_bins != as.integer(n_bins)) {
    stop("n_bins must be a positive integer.")
  }
  n_bins <- as.integer(n_bins)

  treatment_labels <- as.character(treats)
  intervention_label <- as.character(intervention)
  if (!intervention_label %in% unique(treatment_labels)) {
    stop("intervention must match an observed treatment level in treats.")
  }
  pseudo_i <- treatment_labels == intervention_label

  prepared <- rtichoke::create_calibration_curve_list(
    probs = probs,
    reals = list(reals),
    ...
  )

  calibration_rows <- lapply(names(probs), function(series) {
    .ipeval_calplot_rows(
      probs = probs[[series]],
      reals = reals,
      pseudo_i = pseudo_i,
      weights = weights,
      n_bins = n_bins,
      reference_group = series
    )
  }) |>
    dplyr::bind_rows()

  label_prefix <- if (prepared$performance_type == "one model") {
    rep("", nrow(calibration_rows))
  } else {
    paste0("<b>", calibration_rows$reference_group, "</b><br>")
  }
  calibration_rows$text <- paste0(
    label_prefix,
    "Predicted: ", round(calibration_rows$x, 3),
    "<br>Observed: ", round(calibration_rows$y, 3)
  )
  prepared$deciles_dat <- calibration_rows

  finite_values <- c(calibration_rows$x, calibration_rows$y)
  finite_values <- finite_values[is.finite(finite_values)]
  if (length(finite_values) == 0L) {
    limits <- c(0, 1)
  } else {
    l <- max(0, min(finite_values))
    u <- max(finite_values)
    if (u == l) {
      limits <- c(max(0, l - 0.05), min(1, u + 0.05))
    } else {
      limits <- c(l - (u - l) * 0.05, u + (u - l) * 0.05)
    }
  }
  prepared$axes_ranges <- list(xaxis = limits, yaxis = limits)
  prepared
}
