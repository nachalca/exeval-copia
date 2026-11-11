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
