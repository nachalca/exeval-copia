fake_metrics <- function(eval_type = NULL) {
  out <- list(metrics_means = data.frame(
    OCC = 2:3,
    rBIAS = c(1, 2),
    rBIAS_lower = c(0, 1),
    rBIAS_upper = c(2, 3),
    MAIPE = c(10, 20),
    IF20 = c(50, 60),
    IF30 = c(70, 80)
  ))
  out$eval_type <- eval_type
  out
}

fake_models <- function(type1, type2) {
  list(
    list(model_name = "A", metrics_list = fake_metrics(type1)),
    list(model_name = "B", metrics_list = fake_metrics(type2))
  )
}

test_that("combine_metrics() errors when eval_type differs between models", {
  expect_error(
    combine_metrics(fake_models("sequential_updating", "stepwise_updating")),
    "same 'eval_type'"
  )
})

test_that("combine_metrics() combines models sharing the same eval_type", {
  res <- combine_metrics(fake_models("sequential_updating", "sequential_updating"))
  expect_setequal(unique(res$cmetrics$Model), c("A", "B"))
})

test_that("combine_metrics() still accepts models without eval_type", {
  res <- combine_metrics(fake_models(NULL, NULL))
  expect_setequal(unique(res$cmetrics$Model), c("A", "B"))
})

test_that("metrics_occ() returns the eval_type of the simulations", {
  methods::setClass("fake_mrgsims", representation(data = "data.frame"))
  sims <- list(
    simulation_results = list(
      new("fake_mrgsims", data = data.frame(
        ID = 1, OCC = 2, TIME = c(1, 2), DV = c(9, 11)
      ))
    ),
    ttoocc = list(OCC2 = data.frame(
      ID = 1, OCC = 2, TIME = c(1, 2), DV = c(10, 10), EVID = 0
    )),
    eval_type = "stepwise_updating",
    assessment = "Bayesian_forecasting"
  )
  res <- metrics_occ(sims, assessment = "Bayesian_forecasting")
  expect_equal(res$eval_type, "stepwise_updating")
})
