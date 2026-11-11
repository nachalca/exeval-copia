# Minimal mrgsolve model code that captures OCC, for tests that need a model.
occ_model_code <- function() {
  paste(
    "$PARAM CL = 1, V = 10, OCC = 1",
    "$CMT CENT",
    "$ODE dxdt_CENT = -(CL / V) * CENT;",
    "$CAPTURE OCC",
    sep = "\n"
  )
}

# Small dataset with non-consecutive IDs and several occasions.
occ_test_data <- function(ids = c(5, 9, 12), n_occ = 3, ss = FALSE) {
  d <- expand.grid(OCC = seq_len(n_occ), ID = ids)
  d$CMT <- 1
  d$TIME <- d$OCC
  d$EVID <- 0
  d$AMT <- 0
  d$DV <- 1
  if (ss) d$SS <- 0
  d[order(d$ID, d$OCC), c("ID", "OCC", "CMT", "TIME", "EVID", "AMT", "DV",
                          if (ss) "SS")]
}

# Run run_MAP_estimations() on the test model with mapbayest mocked out.
run_map_mocked <- function(...) {
  testthat::local_mocked_bindings(mapbayest = function(...) "fake_est",
                                  .package = "mapbayr",
                                  .env = parent.frame())
  suppressMessages(run_MAP_estimations(
    model = occ_model_code(), model_name = "occ_ids",
    check_compile = FALSE, ...
  ))
}
