#' Create a calibration curve
#'
#' Creates ordinary factual calibration when `treats` is `NULL`, delegating
#' directly to [rtichoke::create_calibration_curve()]. When `treats` is
#' supplied, `probs` must be a named list containing one predicted probability
#' vector for each intervention. The observed risk in each rtichoke prediction
#' bin is replaced by the treatment-specific weighted outcome mean.
#'
#' @param probs Named list of predicted probabilities. For intervention
#'   calibration, names identify intervention levels.
#' @param reals Binary observed outcomes.
#' @param treats Optional observed treatment assignments.
#' @param weights Optional non-negative observation weights. Used only when
#'   `treats` is supplied. If omitted, assigned observations receive weight 1.
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
  weights = NULL,
  interactive = TRUE,
  type = "discrete",
  ...
) {
  if (is.null(treats)) {
    return(rtichoke::create_calibration_curve(
      probs = probs,
      reals = reals,
      interactive = interactive,
      type = type,
      ...
    ))
  }

  if (!identical(type, "discrete")) {
    stop("Intervention calibration currently supports only type = 'discrete'.")
  }

  prepared <- .prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
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
  weights = NULL,
  ...
) {
  if (!is.list(probs) || is.null(names(probs)) || any(names(probs) == "")) {
    stop("For intervention calibration, probs must be a named list.")
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
  if (!all(names(probs) %in% unique(treatment_labels))) {
    stop("Every name in probs must match an observed treatment level in treats.")
  }

  prepared <- rtichoke::create_calibration_curve_list(
    probs = probs,
    reals = list(reals),
    ...
  )

  adjusted <- lapply(names(probs), function(level) {
    p <- probs[[level]]
    bin <- if (length(unique(p)) == 1L) {
      rep(1L, n)
    } else {
      dplyr::ntile(p, 10L)
    }
    selected_weight <- weights * (treatment_labels == level)

    data.frame(reference_group = level, .rticausal_bin = bin) |>
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
    stop("Each prediction bin must contain positive treatment weight for its intervention.")
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
