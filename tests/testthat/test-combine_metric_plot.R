# Characterization tests: pin down what each plot type looks like so the
# implementation can be refactored without changing the output.

cmetrics_fixture <- function() {
  list(cmetrics = data.frame(
    Model = rep(c("A", "B", "C"), each = 2),
    OCC = rep(2:3, times = 3),
    rBIAS = c(1, -2, 3, 4, 5, 6),
    rBIAS_lower = c(0, -3, 2, 3, 4, 5),
    rBIAS_upper = c(2, -1, 4, 5, 6, 7),
    MAIPE = c(10, 20, 30, 40, 50, 60),
    IF20 = c(50, 60, 70, 80, 90, 40),
    IF30 = c(70, 80, 90, 60, 50, 40)
  ))
}

plot_spec <- list(
  bias_barplot = list(y = "rBIAS", label = "rBIAS(%)", title = NULL,
                      geoms = c("GeomCol", "GeomErrorbar", "GeomHline"),
                      hline = c(-20, 20)),
  MAIPE_barplot = list(y = "MAIPE", label = "MAIPE(%)", title = NULL,
                       geoms = c("GeomCol", "GeomHline"), hline = 30),
  IF20_plot = list(y = "IF20", label = "IF20(%)",
                   title = "IF20- Bayesian Forecasting",
                   geoms = c("GeomCol", "GeomHline"), hline = 35),
  IF30_plot = list(y = "IF30", label = "IF30(%)",
                   title = "IF30- Bayesian Forecasting",
                   geoms = c("GeomCol", "GeomHline"), hline = 50)
)

test_that("each combine_metric_plot() type keeps its mapping, layers and reference lines", {
  for (type in names(plot_spec)) {
    spec <- plot_spec[[type]]
    p <- combine_metric_plot(cmetrics_fixture(), type)
    built <- ggplot2::ggplot_build(p)
    labs <- ggplot2::get_labs(p)

    expect_s3_class(p, "ggplot")
    expect_equal(rlang::as_label(p$mapping$y), spec$y, info = type)
    expect_equal(unname(vapply(p$layers, function(l) class(l$geom)[1], character(1))),
                 spec$geoms, info = type)
    expect_equal(labs$y, spec$label, info = type)
    expect_equal(labs$title, spec$title, info = type)

    hline <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"),
                          logical(1)))
    expect_equal(sort(unique(built$data[[hline]]$yintercept)), sort(spec$hline),
                 info = type)
  }
})

test_that("each combine_metric_plot() type facets by occasion with the shared theme", {
  for (type in names(plot_spec)) {
    p <- combine_metric_plot(cmetrics_fixture(), type)
    built <- ggplot2::ggplot_build(p)

    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(p$facet$params$ncol, 2, info = type)  # one column per OCC
    expect_equal(unique(built$data[[1]]$fill),
                 c("#1B9E77", "#D95F02", "#7570B3"), info = type)  # Dark2
    expect_equal(p$theme$axis.text.x$angle, 45, info = type)
    expect_equal(p$theme$legend.position, "right", info = type)
    expect_equal(p$theme$legend.direction, "horizontal", info = type)
    expect_equal(p$theme$strip.text$face, "bold", info = type)
  }
})
