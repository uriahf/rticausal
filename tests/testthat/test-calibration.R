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
  expect_true(all(prepared$deciles_dat$y >= 0))
  expect_true(all(prepared$deciles_dat$y <= 1))
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
