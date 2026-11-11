model_code <- c(
  "$PARAM CL = 1, V = 10",
  "$CMT CENT",
  "$ODE dxdt_CENT = -(CL / V) * CENT;",
  "$CAPTURE OCC"
)

test_that("check_OCC_capture() accepts a single string with OCC captured", {
  code <- paste(model_code, collapse = "\n")
  expect_true(suppressMessages(check_OCC_capture(code)))
})

test_that("check_OCC_capture() accepts a character vector of lines", {
  expect_true(suppressMessages(check_OCC_capture(model_code)))
})

test_that("check_OCC_capture() accepts an mrgmod object", {
  skip_if_not_installed("mrgsolve")
  skip_on_cran()
  mod <- mrgsolve::mcode(
    "occ_test",
    "$PARAM CL = 1, V = 10\n$CMT CENT\n$ODE dxdt_CENT = -(CL / V) * CENT;\n$CAPTURE OCC\n$GLOBAL double OCC = 1;",
    quiet = TRUE
  )
  expect_true(suppressMessages(check_OCC_capture(mod)))
})

test_that("check_OCC_capture() errors when OCC is not captured", {
  no_occ <- sub("OCC", "CL", model_code)
  expect_error(check_OCC_capture(no_occ), "'OCC' is not present")
})

test_that("check_OCC_capture() errors when $CAPTURE is missing", {
  expect_error(check_OCC_capture(model_code[1:3]), "CAPTURE")
})

test_that("check_OCC_capture() rejects unsupported input", {
  expect_error(check_OCC_capture(1), "must be either")
})

test_that("local_seed() seeds reproducibly and restores the previous RNG state", {
  draw <- function(seed) {
    local_seed(seed)
    stats::runif(1)
  }

  set.seed(10)
  before <- .Random.seed
  expect_equal(draw(1), draw(1))
  expect_identical(.Random.seed, before)
})

test_that("local_seed() leaves no RNG state behind if there was none", {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  saved <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", saved, envir = globalenv()),
          add = TRUE)

  if (had_seed) rm(".Random.seed", envir = globalenv())
  (function() local_seed(1))()
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
