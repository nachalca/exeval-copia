# Mock run_MAP_estimations() to report what exeval_ppk() hands to it, then stop
inspect_model_input <- function(model, model_name = NULL) {
  testthat::local_mocked_bindings(
    run_MAP_estimations = function(model, model_name, ...) {
      stop("reached run_MAP: name=", model_name, " class=", class(model)[1],
           " length=", length(model), call. = FALSE)
    },
    .env = parent.frame()
  )
  tryCatch(
    exeval_ppk(model = model, data = data.frame(), model_name = model_name,
               progress = FALSE),
    error = function(e) conditionMessage(e)
  )
}

test_that("exeval_ppk() resolves a built-in model label", {
  msg <- inspect_model_input("TAC_Han2011")
  expect_match(msg, "name=TAC_Han2011 class=character length=1", fixed = TRUE)
})

test_that("exeval_ppk() passes character model code through", {
  msg <- inspect_model_input(occ_model_code(), model_name = "my_model")
  expect_match(msg, "name=my_model class=character length=1", fixed = TRUE)
})

test_that("exeval_ppk() accepts a multi-line character vector of model code", {
  code <- strsplit(occ_model_code(), "\n")[[1]]
  msg <- inspect_model_input(code, model_name = "my_model")
  expect_match(msg, paste0("name=my_model class=character length=", length(code)),
               fixed = TRUE)
})

test_that("exeval_ppk() accepts a compiled mrgmod object", {
  skip_if_not_installed("mrgsolve")
  skip_on_cran()
  mod <- mrgsolve::mcode("occ_ppk", occ_model_code(), quiet = TRUE)
  msg <- inspect_model_input(mod)
  expect_match(msg, "class=mrgmod", fixed = TRUE)
})

test_that("exeval_ppk() passes verbose on to run_pk_simulations()", {
  # Mock the pipeline up to the simulations, which report the verbose they got
  testthat::local_mocked_bindings(
    run_MAP_estimations = function(...) list(),
    update_map_models = function(...) list(),
    run_pk_simulations = function(individual_model, map_results, assessment,
                                  seed = NULL, verbose = FALSE) {
      stop("verbose=", verbose, call. = FALSE)
    }
  )
  run <- function(...) {
    tryCatch(
      exeval_ppk(model = "TAC_Han2011", data = data.frame(), progress = FALSE, ...),
      error = function(e) conditionMessage(e)
    )
  }

  expect_equal(run(verbose = TRUE), "verbose=TRUE")
  expect_equal(run(), "verbose=FALSE")
})
