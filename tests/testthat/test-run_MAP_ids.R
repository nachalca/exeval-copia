# every element must hold only the rows of the ID named in it
expect_events_match_ids <- function(events, occ, ids) {
  expect_named(events, paste0("ev.tto.occ", occ, "_ID", ids))
  for (k in seq_along(ids)) {
    expect_gt(nrow(events[[k]]), 0)
    expect_true(all(events[[k]]$ID == ids[k]))
  }
}

test_that("events are built per actual ID when IDs are not consecutive", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  res <- run_map_mocked(data = occ_test_data(), evaluation_type = "sequential_updating")

  expect_events_match_ids(res$treatments_by_occ$tto_occ_2, 2, c(5, 9, 12))
  expect_events_match_ids(res$treatments_by_occ$tto_occ_3, 3, c(5, 9, 12))
  expect_events_match_ids(res$apriori_treatments$apriori_occ_1, 1, c(5, 9, 12))
})

test_that("the SS branch builds events per actual ID", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  res <- run_map_mocked(data = occ_test_data(ss = TRUE),
                        evaluation_type = "sequential_updating")

  expect_events_match_ids(res$treatments_by_occ$tto_occ_2, 2, c(5, 9, 12))
  expect_events_match_ids(res$apriori_treatments$apriori_occ_1, 1, c(5, 9, 12))
})

test_that("reference strategies build events per actual ID", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  res <- run_map_mocked(data = occ_test_data(), occ_ref = 3,
                        evaluation_type = "backward_reference_updating")

  expect_events_match_ids(res$treatments_by_occ$tto_occ_3, 3, c(5, 9, 12))
  expect_events_match_ids(res$apriori_treatments$apriori_occ_3, 3, c(5, 9, 12))
})

test_that("num_ids subsampling keeps the selected IDs", {
  skip_if_not_installed("mrgsolve")
  skip_if_not_installed("mapbayr")
  skip_on_cran()

  res <- run_map_mocked(data = occ_test_data(), num_ids = 2, sampling = FALSE,
                        evaluation_type = "sequential_updating")

  expect_events_match_ids(res$treatments_by_occ$tto_occ_2, 2, c(5, 9))
  expect_events_match_ids(res$apriori_treatments$apriori_occ_1, 1, c(5, 9))
})
