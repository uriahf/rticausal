test_that("factual calibration delegates to rtichoke", {
  probs <- list(model = c(0.1, 0.2, 0.8, 0.9))
  reals <- c(0, 0, 1, 1)

  expect_s3_class(
    create_calibration_curve(probs, reals, interactive = TRUE),
    "plotly"
  )
})

test_that("intervention calibration separates treatment from model identity", {
  probs <- list(
    model_a = seq(0.05, 0.95, length.out = 40),
    model_b = seq(0.10, 0.90, length.out = 40)
  )
  reals <- rep(c(0, 1), 20)
  treats <- rep(c(0, 0, 1, 1), 10)

  prepared <- rticausal:::.prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1
  )

  expect_setequal(unique(prepared$deciles_dat$reference_group), c("model_a", "model_b"))
  expect_equal(nrow(prepared$deciles_dat), 20)
  expect_true(all(grepl("<b>model_a</b>", prepared$deciles_dat$text[prepared$deciles_dat$reference_group == "model_a"], fixed = TRUE)))
  expect_true(all(grepl("<b>model_b</b>", prepared$deciles_dat$text[prepared$deciles_dat$reference_group == "model_b"], fixed = TRUE)))
})

test_that("calibration coordinates match ipeval cf_calplot", {
  skip_if_not_installed("ipeval")

  probs <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80,
             0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85)
  reals <- c(0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1)
  treats <- c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1)
  weights <- c(1.0, 1.2, 0.8, 1.5, 1.1, 0.9, 1.3, 1.0,
               1.4, 0.7, 1.6, 1.0, 0.6, 1.8, 1.2, 0.5)

  ours <- rticausal:::.ipeval_calplot_rows(
    probs = probs,
    reals = reals,
    pseudo_i = treats == 1,
    weights = weights,
    n_bins = 8L
  )
  reference <- ipeval:::cf_calplot(
    obs_outcome = reals,
    cf_pred = probs,
    pseudo_i = treats == 1,
    ipw = weights,
    n = 8L
  )

  expect_equal(ours$x, as.numeric(reference$pred))
  expect_equal(ours$y, as.numeric(reference$obs))
})

test_that("bin count follows ipeval semantics", {
  probs <- rep(c(0.1, 0.5, 0.9), each = 4)
  rows <- rticausal:::.ipeval_calplot_rows(
    probs = probs,
    reals = rep(c(0, 1), 6),
    pseudo_i = rep(TRUE, 12),
    weights = rep(1, 12),
    n_bins = 8L
  )

  expect_equal(nrow(rows), 3)
  expect_equal(rows$x, c(0.1, 0.5, 0.9))
  expect_equal(rows$bin, 1:3)
})

test_that("intervention must match an observed treatment level", {
  expect_error(
    rticausal:::.prepare_intervention_calibration(
      probs = list(model = seq(0.1, 0.9, length.out = 20)),
      reals = rep(c(0, 1), 10),
      treats = rep(c(0, 1), 10),
      intervention = 2
    ),
    "observed treatment level"
  )
})

test_that("weights must be valid", {
  probs <- list(model = c(0.1, 0.2))
  expect_error(
    rticausal:::.prepare_intervention_calibration(
      probs = probs,
      reals = c(0, 1),
      treats = c(0, 1),
      intervention = 1,
      weights = c(1, -1)
    ),
    "finite, non-negative"
  )
})
