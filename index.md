# rticausal

External validation and evaluation of predictions under interventions in
R.

Initial public API:

``` r

create_calibration_curve()
create_summary_report()
```

For factual calibration, `rticausal` delegates directly to `rtichoke`.

For intervention-specific calibration, `probs` contains one or more
model/prediction series for the same intervention, `treats` contains
observed treatment assignments, `intervention` identifies the treatment
level being evaluated, and `weights` optionally supplies
identification/design weights. `rticausal` does not fit propensity
models.

The intervention calibration coordinates follow `ipeval` calibration
semantics: predictions are sorted and divided into 8 rank groups by
default (or fewer when there are fewer unique prediction values); the
plotted predicted value is the unweighted mean prediction among all
subjects in each group, and the observed value is the weighted mean
outcome among subjects observed under the intervention of interest. A
different group count can be supplied with `groups`.

The initial scope is static binary discrete calibration.
[`create_summary_report()`](https://uriahf.github.io/rticausal/reference/create_summary_report.md)
contains calibration only. Existing `rtichoke` rendering machinery is
reused after the calibration coordinates are prepared.
