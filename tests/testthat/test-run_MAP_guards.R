ref_types <- c("sequential_reference_updating", "backward_reference_updating")
all_types <- c("sequential_updating", "stepwise_updating", ref_types)

test_that("reference strategies require occ_ref", {
  for (type in ref_types) {
    expect_error(
      run_MAP_estimations("code", data = data.frame(), evaluation_type = type),
      "occ_ref must be supplied"
    )
  }
})

test_that("occ_ref with a non-reference strategy gives a readable error", {
  expect_error(
    run_MAP_estimations("code", data = data.frame(), occ_ref = 2,
                        evaluation_type = "sequential_updating"),
    "occ_ref must be used with evaluation type"
  )
})

test_that("a single occasion is rejected for every strategy", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  one_occ <- occ_test_data(n_occ = 1)
  for (type in c("sequential_updating", "stepwise_updating")) {
    expect_error(run_map_mocked(data = one_occ, evaluation_type = type),
                 "at least 2 occasions")
  }
  for (type in ref_types) {
    expect_error(run_map_mocked(data = one_occ, occ_ref = 1,
                                evaluation_type = type),
                 "at least 2 occasions")
  }
})

test_that("occ_ref must be at least 2 and within the available occasions", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  dat <- occ_test_data(n_occ = 3)
  for (type in ref_types) {
    expect_error(run_map_mocked(data = dat, occ_ref = 1, evaluation_type = type),
                 "occ_ref must be between 2 and 3")
    expect_error(run_map_mocked(data = dat, occ_ref = 5, evaluation_type = type),
                 "occ_ref must be between 2 and 3")
  }
})

test_that("two occasions are enough for every strategy", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  dat <- occ_test_data(n_occ = 2)
  for (type in c("sequential_updating", "stepwise_updating")) {
    res <- run_map_mocked(data = dat, evaluation_type = type)
    expect_equal(res$eval_type, type)
    expect_length(res$map_estimations, 1)
  }
  for (type in ref_types) {
    res <- run_map_mocked(data = dat, occ_ref = 2, evaluation_type = type)
    expect_equal(res$eval_type, type)
    expect_length(res$map_estimations, 1)
  }
})
