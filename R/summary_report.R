#' Create a calibration-only summary report
#'
#' @inheritParams create_calibration_curve
#' @param output_file Optional HTML output path. If omitted, the calibration
#'   widget is returned without writing a file.
#'
#' @return The calibration widget, invisibly when written to `output_file`.
#' @export
create_summary_report <- function(
  probs,
  reals,
  treats = NULL,
  intervention = NULL,
  weights = NULL,
  output_file = NULL,
  ...
) {
  calibration <- create_calibration_curve(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = intervention,
    weights = weights,
    interactive = TRUE,
    ...
  )

  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(calibration, output_file, selfcontained = TRUE)
    return(invisible(calibration))
  }

  calibration
}
