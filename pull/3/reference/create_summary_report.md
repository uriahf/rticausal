# Create a calibration-only summary report

Create a calibration-only summary report.

## Usage

``` r
create_summary_report(
  probs,
  reals,
  treats = NULL,
  intervention = NULL,
  weights = NULL,
  output_file = NULL,
  ...
)
```

## Arguments

- probs:

  Named list of predicted probabilities. Names identify models or
  prediction series. In intervention mode every series must predict risk
  under the same `intervention`.

- reals:

  Binary observed outcomes.

- treats:

  Optional observed treatment assignments.

- intervention:

  Optional treatment level whose counterfactual predictions are being
  evaluated. Required when `treats` is supplied.

- weights:

  Optional non-negative observation weights. Used only in intervention
  mode. If omitted, observations assigned to `intervention` receive
  weight 1.

- output_file:

  Optional HTML output path. If omitted, the calibration widget is
  returned without writing a file.

- ...:

  Additional arguments passed to
  [`create_calibration_curve()`](https://uriahf.github.io/rticausal/pull/3/reference/create_calibration_curve.md).

## Value

The calibration widget, invisibly when written to `output_file`.
