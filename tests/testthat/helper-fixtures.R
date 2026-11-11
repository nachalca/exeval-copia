# Minimal mrgsolve model code that captures OCC, for tests that need a model.
occ_model_code <- function() {
  paste(
    "$PARAM CL = 1, V = 10",
    "$CMT CENT",
    "$GLOBAL double OCC = 1;",
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
