# Characterization tests for plot.EvalPPK(): what each plot type looks like.

# Structural summary of a ggplot: geoms, labels, reference lines and facet
plot_summary <- function(p) {
  built <- ggplot2::ggplot_build(p)
  labs <- ggplot2::get_labs(p)
  ref <- function(geom, aes) {
    idx <- which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
    as.numeric(sort(unique(unlist(lapply(idx, function(i) built$data[[i]][[aes]])))))
  }
  lab <- function(x) if (is.null(x)) NULL else as.character(x)  # drop attributes
  list(
    geoms = unname(vapply(p$layers, function(l) sub("^Geom", "", class(l$geom)[1]),
                          character(1))),
    x = lab(labs$x), y = lab(labs$y), title = lab(labs$title),
    hline = ref("GeomHline", "yintercept"),
    vline = ref("GeomVline", "xintercept"),
    facet = class(p$facet)[1]
  )
}

expected_plots <- list(
  bias_barplot = list(geoms = c("Col", "Errorbar", "Hline"), x = "OCC",
                      y = "rBIAS", title = NULL, hline = c(-20, 20)),
  bias_pointrange = list(geoms = c("Errorbar", "Point", "Hline"), x = "OCC",
                         y = "rBIAS (%)", title = NULL, hline = c(-20, 20)),
  MAIPE_barplot = list(geoms = c("Col", "Hline"), x = "OCC", y = "MAIPE",
                       title = NULL, hline = 30),
  bias_boxplot = list(geoms = c("Boxplot", "Hline"), x = "OCC", y = "IPE",
                      title = NULL, hline = c(-20, 20)),
  bias_violin = list(geoms = "Violin", x = "OCC", y = "IPE", title = NULL),
  bias_dotplot = list(geoms = c("Point", "Hline"), x = "OCC",
                      y = "Individual Prediction Error (%)",
                      title = "rBias dotplot", hline = c(-20, 20)),
  bias_density = list(geoms = c("Density", "Vline"),
                      x = "Individual Prediction Error (%)", y = "density",
                      title = " Distribution of rBias per OCC", vline = c(-20, 20)),
  IF20_plot = list(geoms = c("Col", "Hline"), x = "OCC", y = "IF20(%)",
                   title = "IF20- Bayesian Forecasting", hline = 35),
  IF30_plot = list(geoms = c("Col", "Hline"), x = "OCC", y = "IF30(%)",
                   title = "IF30- Bayesian Forecasting", hline = 50),
  error_plot = list(geoms = c("Bar", "Text"), x = "OCC", y = "Proportion",
                    title = "Relative Error Distribution by OCC"),
  fit_class = list(geoms = "Bar", x = "Fit Class", y = "Number of Observations",
                   title = "Fit quality distribution"),
  fit_histogram = list(geoms = "Bar",
                       x = "Absolute Individual Prediction Error (%)",
                       y = "Number of Observations", title = "IPE distribution")
)

test_that("plot.EvalPPK() keeps the layers, labels and reference lines of each type", {
  x <- fake_evalppk_plots()
  for (type in names(expected_plots)) {
    want <- expected_plots[[type]]
    got <- plot_summary(plot(x, type = type))

    expect_equal(got$geoms, want$geoms, info = type)
    expect_equal(got$x, want$x, info = type)
    expect_equal(got$y, want$y, info = type)
    expect_equal(got$title, want$title, info = type)
    expect_equal(got$hline, sort(want$hline %||% numeric(0)), info = type)
    expect_equal(got$vline, sort(want$vline %||% numeric(0)), info = type)
    expect_equal(got$facet, "FacetNull", info = type)
  }
})

test_that("plot.EvalPPK() builds the combined IF plot", {
  p <- plot(fake_evalppk_plots(), type = "IF_plot")
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "ggarrange")
})

test_that("plot.EvalPPK() does not modify the object it plots", {
  x <- fake_evalppk_plots()
  before <- x
  invisible(plot(x, type = "IF_plot"))
  expect_identical(x, before)
})

test_that("error_plot keeps observations with a missing IPE as an NA band", {
  x <- fake_evalppk_plots()
  x$metrics$metrics$IPE[1] <- NA

  p <- plot(x, type = "error_plot")

  expect_s3_class(p, "ggplot")
  expect_true(anyNA(p$data$tramo))
})

test_that("IF plot titles name the assessment that was evaluated", {
  titles <- function(assessment) {
    x <- fake_evalppk_plots(assessment)
    c(as.character(ggplot2::get_labs(plot(x, type = "IF20_plot"))$title),
      as.character(ggplot2::get_labs(plot(x, type = "IF30_plot"))$title))
  }

  expect_equal(titles("Bayesian_forecasting"),
               c("IF20- Bayesian Forecasting", "IF30- Bayesian Forecasting"))
  expect_equal(titles("a_priori"), c("IF20- A priori", "IF30- A priori"))
  expect_equal(titles("Complete"), c("IF20- Complete", "IF30- Complete"))
})
