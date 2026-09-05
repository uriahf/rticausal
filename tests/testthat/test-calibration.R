test_that("factual calibration delegates to rtichoke", {
  probs <- list(model = c(0.1, 0.2, 0.8, 0.9))
  reals <- c(0, 0, 1, 1)

  expect_s3_class(
    create_calibration_curve(probs, reals, interactive = TRUE),
    "plotly"
  )
})

test_that("factual n_bins behavior: omitted delegates without n_bins, explicit passes n_bins", {
  probs <- list(model = seq(0.01, 0.99, length.out = 100))
  reals <- rep(c(0, 1), 50)

  # When n_bins is omitted, rtichoke default (10 bins) is used.
  curve_default <- create_calibration_curve(probs, reals, interactive = FALSE)
  # ggplot object produced by rtichoke
  expect_s3_class(curve_default, "ggplot")

  # When n_bins is explicitly provided, e.g. 5 bins
  curve_5 <- create_calibration_curve(probs, reals, n_bins = 5, interactive = FALSE)
  expect_s3_class(curve_5, "ggplot")
})

test_that("intervention calibration default and explicit n_bins behavior", {
  probs <- list(
    model_a = seq(0.05, 0.95, length.out = 40),
    model_b = seq(0.10, 0.90, length.out = 40)
  )
  reals <- rep(c(0, 1), 20)
  treats <- rep(c(0, 0, 1, 1), 10)

  # Omitted n_bins -> effective_n_bins = 8
  prepared_default <- rticausal:::.prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1
  )
  expect_setequal(unique(prepared_default$calibration_bins_dat$reference_group), c("model_a", "model_b"))
  expect_equal(nrow(prepared_default$calibration_bins_dat), 16) # 8 bins * 2 models
  expect_null(prepared_default$deciles_dat)

  # Explicit n_bins = 10 -> 10 bins per model
  prepared_10 <- rticausal:::.prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1,
    n_bins = 10
  )
  expect_equal(nrow(prepared_10$calibration_bins_dat), 20) # 10 bins * 2 models
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
  expect_equal(ours$bin, as.integer(reference$quintile))
})

test_that("bin count follows ipeval unique prediction capping semantics", {
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
})

test_that("cut vs ntile behavior: N=12, B=10 demonstrates cut behavior preserved", {
  probs <- seq(0.1, 0.9, length.out = 12)
  reals <- rep(c(0, 1), 6)
  pseudo_i <- rep(TRUE, 12)
  weights <- rep(1, 12)

  rows <- rticausal:::.ipeval_calplot_rows(
    probs = probs,
    reals = reals,
    pseudo_i = pseudo_i,
    weights = weights,
    n_bins = 10L
  )

  # cut(1:12, breaks = 10) produces 10 breaks with varying group sizes (e.g. size 2 and 1)
  expect_equal(nrow(rows), 10)
  expect_equal(rows$bin, 1:10)

  # Check parity against ipeval:::cf_calplot for N=12, B=10
  if (requireNamespace("ipeval", quietly = TRUE)) {
    ref <- ipeval:::cf_calplot(
      obs_outcome = reals,
      cf_pred = probs,
      pseudo_i = pseudo_i,
      ipw = weights,
      n = 10L
    )
    expect_equal(rows$x, as.numeric(ref$pred))
    expect_equal(rows$y, as.numeric(ref$obs))
  }
})

test_that("non-divisible N=100, B=8 parity against ipeval", {
  skip_if_not_installed("ipeval")

  set.seed(42)
  probs <- sort(runif(100))
  reals <- rbinom(100, 1, probs)
  treats <- rbinom(100, 1, 0.5)
  weights <- runif(100, 0.5, 2.0)

  ours <- rticausal:::.ipeval_calplot_rows(
    probs = probs,
    reals = reals,
    pseudo_i = treats == 1,
    weights = weights,
    n_bins = 8L
  )
  ref <- ipeval:::cf_calplot(
    obs_outcome = reals,
    cf_pred = probs,
    pseudo_i = treats == 1,
    ipw = weights,
    n = 8L
  )

  expect_equal(ours$x, as.numeric(ref$pred))
  expect_equal(ours$y, as.numeric(ref$obs))
})

test_that("non-target-treatment observations do not contribute to y", {
  probs <- seq(0.1, 0.8, length.out = 8)
  reals <- c(1, 1, 1, 1, 0, 0, 0, 0)
  # Only observations 1 and 2 are in intervention group (pseudo_i = TRUE)
  pseudo_i <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  weights <- rep(1, 8)

  rows <- rticausal:::.ipeval_calplot_rows(
    probs = probs,
    reals = reals,
    pseudo_i = pseudo_i,
    weights = weights,
    n_bins = 4L
  )

  # Bin 1 contains observations 1 and 2 (both pseudo_i = TRUE, reals = 1, y = 1)
  expect_equal(rows$y[1], 1.0)
  # Bin 2, 3, 4 have no pseudo_i = TRUE observations -> y should be NA
  expect_true(is.na(rows$y[2]))
  expect_true(is.na(rows$y[3]))
  expect_true(is.na(rows$y[4]))
})

test_that("changing weights materially changes y", {
  probs <- c(0.1, 0.2, 0.3, 0.4)
  reals <- c(0, 1, 0, 1)
  pseudo_i <- rep(TRUE, 4)

  rows_w1 <- rticausal:::.ipeval_calplot_rows(
    probs = probs, reals = reals, pseudo_i = pseudo_i, weights = c(1, 1, 1, 1), n_bins = 2L
  )
  rows_w2 <- rticausal:::.ipeval_calplot_rows(
    probs = probs, reals = reals, pseudo_i = pseudo_i, weights = c(10, 1, 1, 10), n_bins = 2L
  )

  expect_false(identical(rows_w1$y, rows_w2$y))
  # In bin 1 (obs 1 & 2): w1 gives (0*1 + 1*1)/2 = 0.5; w2 gives (0*10 + 1*1)/11 = 1/11
  expect_equal(rows_w1$y[1], 0.5)
  expect_equal(rows_w2$y[1], 1 / 11)
})

test_that("edge cases: all-identical predictions, n_bins > n, ties", {
  # All identical predictions -> capped at 1 bin
  rows_identical <- rticausal:::.ipeval_calplot_rows(
    probs = rep(0.5, 10), reals = rep(c(0, 1), 5), pseudo_i = rep(TRUE, 10), weights = rep(1, 10), n_bins = 8L
  )
  expect_equal(nrow(rows_identical), 1)
  expect_equal(rows_identical$x, 0.5)
  expect_equal(rows_identical$y, 0.5)

  # n_bins > n (e.g. n_bins = 10, n = 5) -> capped by unique predictions (min 5, 10) = 5
  rows_large_nbins <- rticausal:::.ipeval_calplot_rows(
    probs = seq(0.1, 0.5, length.out = 5), reals = c(0, 1, 0, 1, 0), pseudo_i = rep(TRUE, 5), weights = rep(1, 5), n_bins = 10L
  )
  expect_equal(nrow(rows_large_nbins), 5)

  # Ties in predictions
  rows_ties <- rticausal:::.ipeval_calplot_rows(
    probs = c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3), reals = rep(c(0, 1), 3), pseudo_i = rep(TRUE, 6), weights = rep(1, 6), n_bins = 3L
  )
  expect_equal(nrow(rows_ties), 3)
})

test_that("multiple prediction series each apply their own unique-prediction cap", {
  probs <- list(
    constant_model = rep(0.3, 10),
    varied_model = seq(0.1, 0.9, length.out = 10)
  )
  reals <- rep(c(0, 1), 5)
  treats <- rep(c(0, 1), 5)

  prepared <- rticausal:::.prepare_intervention_calibration(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1,
    n_bins = 8
  )

  dat <- prepared$calibration_bins_dat
  const_rows <- dat[dat$reference_group == "constant_model", ]
  varied_rows <- dat[dat$reference_group == "varied_model", ]

  expect_equal(nrow(const_rows), 1) # capped at 1 unique prediction
  expect_equal(nrow(varied_rows), 8) # capped at min(8, 10) = 8
})

test_that("invalid n_bins input raises error", {
  probs <- list(model = c(0.1, 0.2))
  reals <- c(0, 1)
  treats <- c(1, 1)

  expect_error(
    rticausal:::.prepare_intervention_calibration(probs, reals, treats, intervention = 1, n_bins = 0),
    "n_bins must be a positive integer"
  )
  expect_error(
    rticausal:::.prepare_intervention_calibration(probs, reals, treats, intervention = 1, n_bins = -5),
    "n_bins must be a positive integer"
  )
  expect_error(
    rticausal:::.prepare_intervention_calibration(probs, reals, treats, intervention = 1, n_bins = 2.5),
    "n_bins must be a positive integer"
  )
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

test_that("Plotly and ggplot renderers consume calibration_bins_dat", {
  probs <- list("Model" = seq(0.1, 0.9, length.out = 20))
  reals <- rep(c(0, 1), 10)
  treats <- rep(c(0, 1), 10)
  weights <- rep(1, 20)

  # Test interactive (plotly) output
  p_plotly <- create_calibration_curve(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1,
    weights = weights,
    interactive = TRUE
  )
  expect_s3_class(p_plotly, "plotly")

  # Test non-interactive (ggplot) output
  p_gg <- create_calibration_curve(
    probs = probs,
    reals = reals,
    treats = treats,
    intervention = 1,
    weights = weights,
    interactive = FALSE
  )
  expect_s3_class(p_gg, "ggplot")
})
