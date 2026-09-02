#' Create a calibration curve
#'
#' Creates ordinary factual calibration when `treats` is `NULL`, delegating
#' directly to [rtichoke::create_calibration_curve()]. For predictions under an
#' intervention, `treats` contains the observed treatment assignments and
#' `intervention` identifies the treatment level whose predicted risks are in
#' `probs`. The observed risk in each rtichoke prediction bin is replaced by
#' the treatment-specific weighted outcome mean.
#'
#' This mirrors the separation used by `ipeval`: prediction/model identity is
#' distinct from the treatment of interest. `rticausal` consumes caller-supplied
#' weights but does not estimate a treatment model.
#'
#' @param probs Named list of predicted probabilities. Names identify models or
#'   prediction series, as in rtichoke. In intervention mode every series must
#'   predict risk under the same `intervention`.
#' @param reals Binary observed outcomes.
#' @param treats Optional observed treatment assignments.
#' @param intervention Optional treatment level whose counterfactual predictions
#'   are being evaluated. Required when `treats` is supplied.
#' @param weights Optional non-negative observation weights. Used only in
#'   intervention mode. If omitted, observations assigned to `intervention`
#'   receive weight 1.
#' @param interactive Passed to rtichoke's calibration renderer.
#' @param type Calibration type. Intervention calibration currently supports
#'   only `"discrete"`.
#' @param ... Additional arguments passed to rtichoke calibration preparation.
#'
#' @return A calibration plot produced by rtichoke.
#' @export
create_calibration_curve <- function(
  probs,
  reals,
  treats = NULL,
  intervention = NULL,
  weights = NULL,
  interactive = TRUE,
  type = "discrete",
  ...
) {
  if (is.null(treats)) {
    if (!is.null(intervention) || !is.null(weights)) {
      stop("intervention and weights require treats.")
    }
    return(rtichoke::create_calibration_curve(
      probs = probs,
      reals = reals,
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
    ...
  )

  if (interactive) {
    rtichoke:::create_plotly_curve_from_calibration_curve_list(prepared, type = type)
  } else {
    rtichoke:::create_ggplot_curve_from_calibration_curve_list(prepared, type = type)
  }
}

.prepare_intervention_calibration <- function(
  probs,
  reals,
  treats,
  intervention,
  weights = NULL,
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
  if (is.null(weights)) {
    weights <- rep(1, n)
  }
  if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) {
    stop("weights must be finite, non-negative, and the same length as reals.")
  }

  treatment_labels <- as.character(treats)
  intervention_label <- as.character(intervention)
  if (!intervention_label %in% unique(treatment_labels)) {
    stop("intervention must match an observed treatment level in treats.")
  }

  prepared <- rtichoke::create_calibration_curve_list(
    probs = probs,
    reals = list(reals),
    ...
  )

  selected_weight <- weights * (treatment_labels == intervention_label)
  adjusted <- lapply(names(probs), function(series) {
    p <- probs[[series]]
    bin <- if (length(unique(p)) == 1L) {
      rep(1L, n)
    } else {
      dplyr::ntile(p, 10L)
    }

    data.frame(reference_group = series, .rticausal_bin = bin) |>
      dplyr::mutate(
        weighted_event = selected_weight * reals,
        selected_weight = selected_weight
      ) |>
      dplyr::group_by(reference_group, .rticausal_bin) |>
      dplyr::summarise(
        y = sum(weighted_event) / sum(selected_weight),
        .groups = "drop"
      )
  }) |>
    dplyr::bind_rows()

  if (any(!is.finite(adjusted$y))) {
    stop("Each prediction bin must contain positive treatment weight for the intervention.")
  }

  deciles <- prepared$deciles_dat
  if ("quintile" %in% names(deciles)) {
    deciles$.rticausal_bin <- deciles$quintile
  } else {
    deciles$.rticausal_bin <- ave(
      seq_len(nrow(deciles)),
      deciles$reference_group,
      FUN = seq_along
    )
  }
  deciles <- dplyr::left_join(
    dplyr::select(deciles, -y),
    adjusted,
    by = c("reference_group", ".rticausal_bin")
  )
  deciles$text <- paste0(
    ifelse(
      prepared$performance_type == "one model",
      "",
      paste0("<b>", deciles$reference_group, "</b><br>")
    ),
    "Predicted: ", round(deciles$x, 3),
    "<br>Observed: ", round(deciles$y, 3)
  )
  prepared$deciles_dat <- dplyr::select(deciles, -.rticausal_bin)
  limits <- rtichoke:::define_limits_for_calibration_plot(prepared$deciles_dat)
  prepared$axes_ranges <- list(xaxis = limits, yaxis = limits)
  prepared
}
