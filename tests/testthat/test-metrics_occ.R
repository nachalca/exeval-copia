test_that("observations with DV <= 0 are excluded with a warning", {
  for (assessment in c("Bayesian_forecasting", "a_priori")) {
    sims <- fake_sims(pred = c(9, 11, 12, 8), obs = c(0, 10, 10, -1), assessment)

    expect_warning(res <- metrics_occ(sims, assessment = assessment),
                   "2 observation\\(s\\) with DV <= 0")

    expect_equal(res$metrics$TIME, c(2, 3))
    expect_true(all(is.finite(res$metrics$IPE)))
    expect_true(all(is.finite(unlist(res$metrics_means[c("rBIAS", "MAIPE")]))))
    expect_equal(res$metrics_means$rBIAS, 15)  # mean of 10 and 20
  }
})

test_that("no warning when all observations are positive", {
  sims <- fake_sims(pred = c(9, 11), obs = c(10, 10))
  expect_no_warning(metrics_occ(sims, assessment = "Bayesian_forecasting"))
})

test_that("an error is raised when every observation has DV <= 0", {
  sims <- fake_sims(pred = c(9, 11), obs = c(0, 0))
  expect_error(
    suppressWarnings(metrics_occ(sims, assessment = "Bayesian_forecasting")),
    "metrics. object could not be generated"
  )
})

test_that("a single observation gives NA confidence limits without warnings", {
  sims <- fake_sims(pred = 9, obs = 10)

  expect_no_warning(res <- metrics_occ(sims, assessment = "Bayesian_forecasting"))

  expect_equal(res$metrics_means$rBIAS, -10)
  expect_identical(res$metrics_means$rBIAS_lower, NA_real_)
  expect_identical(res$metrics_means$rBIAS_upper, NA_real_)
})

test_that("confidence limits use the t distribution for two or more observations", {
  sims <- fake_sims(pred = c(9, 12), obs = c(10, 10))  # IPE = -10, 20

  res <- metrics_occ(sims, assessment = "Bayesian_forecasting")

  half <- stats::qt(0.975, df = 1) * stats::sd(c(-10, 20)) / sqrt(2)
  expect_equal(res$metrics_means$rBIAS, 5)
  expect_equal(res$metrics_means$rBIAS_lower, 5 - half)
  expect_equal(res$metrics_means$rBIAS_upper, 5 + half)
})
