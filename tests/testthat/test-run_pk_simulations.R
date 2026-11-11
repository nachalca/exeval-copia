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
