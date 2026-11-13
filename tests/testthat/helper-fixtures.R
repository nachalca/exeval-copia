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

# EvalPPK object with two occasions, three subjects and every error band, for
# testing plot.EvalPPK() without running the whole workflow.
fake_evalppk_plots <- function(assessment = "Bayesian_forecasting") {
  m <- data.frame(
    ID = rep(1:3, times = 2), OCC = rep(2:3, each = 3), TIME = 1,
    DV = 10, Ind_Prediction = 10, IPE = c(5, -20, 40, 60, -10, 25)
  )
  m$APE <- abs(m$IPE)
  m$RMSE <- (m$IPE / 100)^2
  m$Fit_Class <- dplyr::case_when(abs(m$IPE) <= 15 ~ "Excellent", abs(m$IPE) <= 30 ~ "Acceptable",
                                  abs(m$IPE) <= 50 ~ "Poor", TRUE ~ "Very Poor")
  means <- do.call(rbind, lapply(split(m, m$OCC), function(d) data.frame(
    rBIAS = mean(d$IPE), rBIAS_lower = mean(d$IPE) - 10, rBIAS_upper = mean(d$IPE) + 10,
    MAIPE = mean(d$APE), rRMSE = 10, IF20 = 100 * mean(abs(d$IPE) <= 20),
    IF30 = 100 * mean(abs(d$IPE) <= 30), OCC = d$OCC[1])))
  structure(list(metrics = list(metrics = m, metrics_means = means),
                 estimates = NULL, updates = NULL, simulations = list(assessment = assessment)),
            class = "EvalPPK")
}
