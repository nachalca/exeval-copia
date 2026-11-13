#' Plot combined model performance metrics
#'
#' Generates comparative visualizations of evaluation metrics across multiple
#' externally evaluated models combined with [combine_metrics()].
#'
#' This function is intended for model comparison workflows, allowing visual
#' inspection of predictive performance across evaluation occasions..
#'
#' @param cmetrics Named list returned by [combine_metrics()], containing
#' combined model performance metrics. 
#' 
#' @param type Character string specifying the plot type to generate.
#' Available options are:
#' \itemize{
#'   \item \code{"bias_barplot"}: bar plot of relative bias
#'   (\code{rBIAS}) with confidence intervals.
#'
#'   \item \code{"MAIPE_barplot"}: bar plot of mean absolute individual
#'   prediction error (\code{MAIPE}).
#'
#'   \item \code{"IF20_plot"}: bar plot of \code{IF20} values with
#'   reference threshold.
#'
#'   \item \code{"IF30_plot"}: bar plot of \code{IF30} values with
#'   reference threshold.
#' }
#' 
#' @details
#' Metrics are displayed separately for each evaluation occasion
#' (\code{OCC}) using faceted plots, enabling direct visual comparison
#' between candidate models.
#'
#' @return A \code{ggplot2} object.
#' 
#' @seealso [combine_metrics()], [plot.EvalPPK()]
#'
#' @examples
#' 
#' # Fake  metrics
#' generate_fake_metrics <- function(n_occasions = 3) {
#'data.frame(
#'  OCC = rep(1:n_occasions),  # Simula varias ocasiones
#'  rBIAS = stats::rnorm(n_occasions, mean = 0, sd = 10),
#'  rBIAS_lower = stats::rnorm(n_occasions, mean = -5, sd = 5),
#'  rBIAS_upper = stats::rnorm(n_occasions, mean = 5, sd = 5),
#'  MAIPE = stats::runif(n_occasions, min = 10, max = 50),
#'  IF20 = stats::runif(n_occasions, min = 20, max = 80),
#'  IF30 = stats::runif(n_occasions, min = 30, max = 90)
#')
#'}
#' 
#' simulation1 <- list(metrics_means = generate_fake_metrics())
#' simulation2 <- list(metrics_means = generate_fake_metrics())
#' # List of models
#' models_list <- list(
#' list(model_name = "Test_Model1", metrics_list = simulation1),
#' list(model_name = "Test_Model2", metrics_list = simulation2)
#' )
#' combined_results <- combine_metrics(models_list)
#' 
#' # plot results
#' combine_metric_plot(combined_results, type = 'bias_barplot')
#' 
#' @family model comparison
#' @export
combine_metric_plot <-function(cmetrics,
                               type = c('bias_barplot',
                                      'MAIPE_barplot',
                                      'IF20_plot',
                                      'IF30_plot')) {
  type <- match.arg(type)
  spec <- metric_plot_specs[[type]]
  cmetrics <- cmetrics$cmetrics
  n_occs <- length(unique(cmetrics$OCC))

  pplot <- cmetrics |>
    mutate(OCC = factor(OCC)) |>
    ggplot(aes(x = Model, y = !!sym(spec$y), fill = Model)) +
    geom_col()

  if (isTRUE(spec$errorbar)) {
    pplot <- pplot +
      geom_errorbar(aes(ymin = rBIAS_lower, ymax = rBIAS_upper), width = 0.2)
  }

  pplot +
    geom_hline(data = data.frame(yy = spec$hline), aes(yintercept = yy),
               linetype = "dashed", color = "firebrick") +
    scale_fill_brewer(palette = "Dark2") +
    guides(fill = guide_legend(title.position = "top", nrow = 8, ncol = 3)) +
    labs(title = spec$title, y = spec$label) +
    metric_plot_theme() +
    facet_wrap(~OCC, ncol = n_occs,
               labeller = labeller(OCC = function(x) paste0("OCC ", x)))
}

# What differs between the plot types: the plotted metric, its axis label and
# title, the dashed reference line(s), and whether error bars are drawn.
metric_plot_specs <- list(
  bias_barplot  = list(y = "rBIAS", label = "rBIAS(%)", title = NULL,
                       hline = c(-20, 20), errorbar = TRUE),
  MAIPE_barplot = list(y = "MAIPE", label = "MAIPE(%)", title = NULL,
                       hline = 30, errorbar = FALSE),
  IF20_plot     = list(y = "IF20", label = "IF20(%)",
                       title = "IF20- Bayesian Forecasting",
                       hline = 35, errorbar = FALSE),
  IF30_plot     = list(y = "IF30", label = "IF30(%)",
                       title = "IF30- Bayesian Forecasting",
                       hline = 50, errorbar = FALSE)
)

# Theme shared by all combine_metric_plot() types
metric_plot_theme <- function() {
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.position = "right",
        legend.direction = "horizontal",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold"))
}
