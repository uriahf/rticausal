# rticausal

External validation and evaluation of predictions under interventions in R.

Initial public API:

```r
create_calibration_curve()
create_summary_report()
```

`rticausal` reuses the existing `rtichoke` calibration rendering machinery. For intervention-specific calibration, the caller supplies predicted risks under each intervention, observed treatment assignments, observed outcomes, and optional identification/design weights. `rticausal` does not fit propensity models.

The initial scope is static binary discrete calibration. `create_summary_report()` contains calibration only.
