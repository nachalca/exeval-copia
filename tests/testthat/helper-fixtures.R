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
