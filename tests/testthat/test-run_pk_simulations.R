sim_ids <- c(5, 9, 12)

# Simulated OCC values and IDs per list element
sim_summary <- function(res) {
  vapply(res$simulation_results, function(x) {
    paste(unique(x@data$ID), unique(x@data$OCC), nrow(x@data) > 0)
  }, character(1))
}

test_that("a priori simulations use the reference occasion when occ_ref is set", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  est <- run_map_mocked(data = occ_test_data(), occ_ref = 3,
                        evaluation_type = "backward_reference_updating")
  res <- suppressMessages(run_pk_simulations(NULL, est, assessment = "a_priori"))

  expect_named(res$simulation_results, paste0("OCC_3_ID", sim_ids))
  expect_named(res$ttoocc, "OCC3")
  expect_named(res$events_tto, "OCC_3")
  expect_equal(unname(sim_summary(res)), paste(sim_ids, 3, TRUE))
})

test_that("a priori simulations still use OCC 1 without occ_ref", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  est <- run_map_mocked(data = occ_test_data(),
                        evaluation_type = "sequential_updating")
  res <- suppressMessages(run_pk_simulations(NULL, est, assessment = "a_priori"))

  expect_named(res$simulation_results, paste0("OCC_1_ID", sim_ids))
  expect_named(res$ttoocc, "OCC1")
  expect_named(res$events_tto, "OCC_1")
  expect_equal(unname(sim_summary(res)), paste(sim_ids, 1, TRUE))
})

# Captured individual clearance of every simulated subject, in ID order
sim_cli <- function(res) {
  vapply(res$simulation_results, function(x) x@data$CLi[1], numeric(1))
}

stochastic_estimate <- function() {
  run_map_mocked(data = occ_test_data(), omega = TRUE,
                 evaluation_type = "sequential_updating")
}

test_that("seed makes a priori simulations reproducible and varies between subjects", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  est <- stochastic_estimate()
  run <- function(seed) {
    suppressMessages(run_pk_simulations(NULL, est, assessment = "a_priori",
                                        seed = seed))
  }

  a <- sim_cli(run(123))
  b <- sim_cli(run(123))
  c <- sim_cli(run(456))

  expect_equal(a, b)
  expect_false(isTRUE(all.equal(a, c)))
  # each subject gets its own random-effect draw
  expect_equal(length(unique(round(a, 8))), length(a))
})

test_that("run_pk_simulations() leaves the global RNG state untouched when seeded", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  est <- stochastic_estimate()
  set.seed(1)
  before <- .Random.seed
  suppressMessages(run_pk_simulations(NULL, est, assessment = "a_priori",
                                      seed = 123))
  expect_identical(.Random.seed, before)
})
