test_that("factual calibration delegates to rtichoke", {
  probs <- list(model = c(0.1, 0.2, 0.8, 0.9))
  reals <- c(0, 0, 1, 1)

  expect_s3_class(
    create_calibration_curve(probs, reals, interactive = TRUE),
    "plotly"
  )
})

test_that("intervention calibration uses assigned treatment outcomes", {
  probs <- list(
    "0" = seq(0.05, 0.95, length.out = 20),
    "1" = seq(0.95, 0.05, length.out = 20)
  )
  reals <- rep(c(0, 1), 10)
  treats <- rep(c(0, 1), 10)

  prepared <- rticausal:::.prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats
  )

  expect_true(all(prepared$deciles_dat$y >= 0))
  expect_true(all(prepared$deciles_dat$y <= 1))
  expect_equal(nrow(prepared$deciles_dat), 20)
})

test_that("weights must be valid", {
  probs <- list("0" = c(0.1, 0.2), "1" = c(0.8, 0.9))
  expect_error(
    rticausal:::.prepare_intervention_calibration(
      probs = probs,
      reals = c(0, 1),
      treats = c(0, 1),
      weights = c(1, -1)
    ),
    "finite, non-negative"
  )
})
