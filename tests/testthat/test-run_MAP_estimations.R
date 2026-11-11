test_that("run_MAP_estimations() checks OCC on the resolved mrgmod, not the raw input", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  # Record what class of object the OCC check receives, then stop early
  testthat::local_mocked_bindings(
    check_OCC_capture = function(modelo) {
      stop("OCC check received: ", paste(class(modelo), collapse = ","),
           call. = FALSE)
    }
  )

  expect_error(
    run_MAP_estimations(
      model = occ_model_code(),
      model_name = "occ_test",
      check_compile = FALSE,
      data = data.frame(),
      evaluation_type = "sequential_updating"
    ),
    "OCC check received: mrgmod"
  )
})
