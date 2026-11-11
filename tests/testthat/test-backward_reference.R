test_that("update_map_models() handles backward_reference_updating using occ_ref from map_results", {
  testthat::local_mocked_bindings(use_posterior = function(x, ...) x,
                                  .package = "mapbayr")
  map_results <- list(
    eval_type = "backward_reference_updating",
    occ_ref = 4,
    map_estimations = list(
      "map.estimation.occ_3" = "est3",
      "map.estimation.occ_3_2" = "est32",
      "map.estimation.occ_3_2_1" = "est321"
    )
  )

  res <- suppressMessages(
    update_map_models(map_results, "backward_reference_updating")
  )

  expect_equal(res$eval_type, "backward_reference_updating")
  expect_setequal(
    names(res$ind_model),
    c("a.posteriori_occ1_2", "a.posteriori_occ2_3", "a.posteriori_occ3_4")
  )
})

test_that("update_map_models() gives a clear error when occ_ref is missing for backward updating", {
  map_results <- list(
    eval_type = "backward_reference_updating",
    map_estimations = list("map.estimation.occ_1" = "est1")
  )
  expect_error(
    update_map_models(map_results, "backward_reference_updating"),
    "must contain 'occ_ref'"
  )
})

test_that("run_MAP_estimations() returns occ_ref", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()
  testthat::local_mocked_bindings(mapbayest = function(...) "fake_est",
                                  .package = "mapbayr")

  model_code <- paste(
    "$PARAM CL = 1, V = 10",
    "$CMT CENT",
    "$GLOBAL double OCC = 1;",
    "$ODE dxdt_CENT = -(CL / V) * CENT;",
    "$CAPTURE OCC",
    sep = "\n"
  )
  dat <- data.frame(ID = 1, OCC = 1:4, CMT = 1, TIME = 1:4,
                    EVID = 0, AMT = 0, DV = 1)

  res <- suppressMessages(run_MAP_estimations(
    model = model_code, model_name = "occ_test", check_compile = FALSE,
    data = dat, occ_ref = 4,
    evaluation_type = "backward_reference_updating"
  ))

  expect_equal(res$occ_ref, 4)
  expect_equal(res$eval_type, "backward_reference_updating")
})
