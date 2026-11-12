# Minimal mrgsolve model code that captures OCC, for tests that need a model.
# With `omega = TRUE` it adds a random effect on CL that is also captured
# (as CLi), so stochastic simulations differ between subjects.
occ_model_code <- function(omega = FALSE) {
  paste(
    "$PARAM CL = 1, V = 10, OCC = 1",
    "$CMT CENT",
    if (omega) "$OMEGA 0.5",
    if (omega) "$MAIN double CLi = CL * exp(ETA(1));",
    "$ODE dxdt_CENT = -(CL / V) * CENT;",
    if (omega) "$CAPTURE OCC CLi" else "$CAPTURE OCC",
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
run_map_mocked <- function(..., omega = FALSE) {
  testthat::local_mocked_bindings(mapbayest = function(...) "fake_est",
                                  .package = "mapbayr",
                                  .env = parent.frame())
  suppressMessages(run_MAP_estimations(
    model = occ_model_code(omega), model_name = "occ_ids",
    check_compile = FALSE, ...
  ))
}

# Minimal stand-in for an mrgsims object: metrics_occ() only reads its @data.
methods::setClass("fake_mrgsims", representation(data = "data.frame"))

# Simulation results for one subject and occasion 2, as run_pk_simulations()
# would return them. `pred` fills the DV (posterior) and CP (a priori) columns.
fake_sims <- function(pred, obs, assessment = "Bayesian_forecasting") {
  time <- seq_along(pred)
  list(
    simulation_results = list(methods::new("fake_mrgsims", data = data.frame(
      ID = 1, OCC = 2, TIME = time, DV = pred, CP = pred
    ))),
    ttoocc = list(OCC2 = data.frame(
      ID = 1, OCC = 2, TIME = time, DV = obs, EVID = 0
    )),
    eval_type = "sequential_updating",
    assessment = assessment
  )
}
