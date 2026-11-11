test_that("update_map_models() works with the default evaluation_type", {
  map_results <- list(eval_type = "sequential_updating", map_estimations = list())
  res <- suppressMessages(update_map_models(map_results))
  expect_equal(res$eval_type, "sequential_updating")
})

test_that("update_map_models() errors on an evaluation_type different from run_MAP_estimations()", {
  map_results <- list(eval_type = "stepwise_updating", map_estimations = list())
  expect_error(update_map_models(map_results), "same evaluation type")
})

test_that("update_map_models() rejects an invalid evaluation_type", {
  map_results <- list(eval_type = "sequential_updating", map_estimations = list())
  expect_error(update_map_models(map_results, "bogus"), "should be one of")
})

test_that("run_MAP_estimations() validates evaluation_type and method up front", {
  expect_error(
    run_MAP_estimations("code", data = data.frame(), evaluation_type = "bogus"),
    "should be one of"
  )
  expect_error(
    run_MAP_estimations("code", data = data.frame(),
                        evaluation_type = "sequential_updating",
                        method = "bogus"),
    "should be one of"
  )
})

fake_cmetrics <- function() {
  list(cmetrics = data.frame(
    Model = rep(c("A", "B"), each = 2),
    OCC = rep(2:3, times = 2),
    rBIAS = c(1, -2, 3, 4),
    rBIAS_lower = c(0, -3, 2, 3),
    rBIAS_upper = c(2, -1, 4, 5)
  ))
}

test_that("combine_metric_plot() works with the default type", {
  expect_s3_class(combine_metric_plot(fake_cmetrics()), "ggplot")
})

test_that("combine_metric_plot() rejects an invalid type", {
  expect_error(combine_metric_plot(fake_cmetrics(), type = "bogus"),
               "should be one of")
})
