test_that("NAMESPACE uses explicit imports, not blanket package imports", {
  ns_file <- system.file("NAMESPACE", package = "exeval")
  skip_if(ns_file == "", "NAMESPACE file not available")

  ns <- readLines(ns_file)
  blanket <- grep("^import\\(", ns, value = TRUE)
  expect_identical(blanket, character(0))
})
