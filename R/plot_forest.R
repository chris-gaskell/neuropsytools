utils::globalVariables(c("count", "y", "change"))

#' Plot a Forest Plot
#'
#' Creates a forest plot visualizing scores and their confidence intervals
#' across different tests and groups. The plot includes optional shading and
#' descriptors to provide additional context.
#'
#' @param data A data frame containing the data to be plotted.
#' @param score The column name in the data frame representing the scores to be
#'   plotted.
#' @param metric The metric of the scores in the data frame. Defaults to "z".
#'   The metric can be converted to "z".
#' @param test The column name in the data frame representing the test names.
#' @param group The column name in the data frame representing the group names.
#' @param ci.lb (Optional) The column name in the data frame representing the
#'   lower bound of the confidence interval.
#' @param ci.ub (Optional) The column name in the data frame representing the
#'   upper bound of the confidence interval.
#' @param axis.label.metric The metric to be used for axis labels. Defaults to
#'   the value of `metric`. Can be "percentile", "t", "index", "scaled", or "z".
#' @param descriptors (Optional) Logical value indicating whether to include
#'   descriptive labels on the plot. Defaults to TRUE.
#' @param shading (Optional) Logical value indicating whether to include shading
#'   on the plot for the average range. Defaults to TRUE.
#' @param abbreviations (Optional) Logical value indicating whether to
#'   abbreviate test names. Defaults to FALSE.
#' @param z.lines (Optional) Logical value indicating whether to draw lines at
#'   z-scores from -4 to 4. Defaults to TRUE.
#' @param z.line.color (Optional) Color of the z-lines. Defaults to "black".
#' @param descriptors (Optional) Logical value indicating whether to include
#'   descriptive labels on the plot (e.g., "Below", "Average", "Above"). Defaults to TRUE.
#' @param descriptors.color (Optional) Color of the descriptor labels. Defaults to "black".
#' @param shading (Optional) Logical value indicating whether to include shading
#'   on the plot for the average range (e.g., z-scores between -1 and 1). Defaults to TRUE.
#' @param shading.color (Optional) Color of the shaded area. Defaults to "#2fa4e7".
#' @param abbreviations (Optional) Logical value indicating whether to abbreviate test names.
#'   Defaults to FALSE.
#'
#' @return A ggplot object representing the forest plot.
#' @export
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   test = rep(c("Test1", "Test2", "Test3"), 2),
#'   group = rep(c("Group1", "Group2"), each = 3),
#'   score = c(1.2, 2.3, 1.5, -1.2, 0.5, 1.1),
#'   ci.lb = c(0.8, 1.8, 1.2, -1.5, 0.2, 0.8),
#'   ci.ub = c(1.6, 2.8, 1.8, -0.9, 0.8, 1.4)
#' )
#'
#' # Plot with default settings
#' plot_forest(df, score = score, test = test, group = group, ci.lb = ci.lb,
#' ci.ub = ci.ub)
#'
#' # Plot with no descriptors and shading
#' plot_forest(df, score = score, test = test, group = group, ci.lb = ci.lb,
#' ci.ub = ci.ub, descriptors = FALSE, shading = FALSE)
plot_forest <- function(data, score, metric = "z", test, group, ci.lb, ci.ub, axis.label.metric = metric,
                        z.lines = TRUE, z.line.color = "black",
                        descriptors = TRUE, descriptors.color = "black", shading = TRUE, shading.color = "#2fa4e7", abbreviations = FALSE) {
  # Check if group or test is missing
  if (missing(group) || missing(test)) {
    stop("Both the 'test' and 'group' arguments are required.")
  }
  if (missing(score)) {
    stop("The 'score' argument is required.")
  }

  # Detect duplicate rows based on test and group
  dup_rows <- duplicated(dplyr::select(data, {{test}}, {{group}}))
  if (any(dup_rows)) {
    dup_indices <- which(dup_rows)
    warning(glue::glue("Duplicate rows detected based on 'test' and row. Rows {dup_indices} will be removed."))
    data <- data[!dup_rows, ]
  }

  # Convert to z
  data <- dplyr::mutate(data, z = neuropsytools::convert_standard({{score}}, metric = metric, metric.new = "z"))

  if (metric != "z" && !missing(ci.lb)) {
    data <- dplyr::mutate(data, ci.lb = neuropsytools::convert_standard({{ci.lb}}, metric = metric, metric.new = "z"))
  }
  if (metric != "z" && !missing(ci.ub)) {
    data <- dplyr::mutate(data, ci.ub = neuropsytools::convert_standard({{ci.ub}}, metric = metric, metric.new = "z"))
  }

  # Axis labels
  label_types <- c("percentile", "t", "index", "scaled", "z")
  label_names <- c("Percentile", "T Score", "Index Score", "Scaled Score", "Z Score")
  labeling_functions <- list(
    percentile = function(x) paste0(format(stats::pnorm(x) * 100, digits = 2), "%"),
    t = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "t")),
    index = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "index")),
    scaled = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "scaled")),
    z = function(x) as.character(x)
  )

  labeling_function <- labeling_functions[[match(axis.label.metric, label_types)]]
  y_label_text <- label_names[match(axis.label.metric, label_types)]

  # Test abbreviations
  if (abbreviations) {
    abbreviate_test_names <- function(x) {
      ifelse(nchar(x) > 2, toupper(abbreviate(x, minlength = 1, dot = FALSE, method = "left.kept")), x)
    }
  } else {
    abbreviate_test_names <- function(x) x
  }

  # Descriptor labels
  descriptor_labels <- data.frame(z = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5), label = c("Below", "Low", "Average", "Average", "High", "Above"))

  # Initialize the plot
  p <- data |>
    dplyr::mutate(
      group = factor({{group}}, levels = unique({{group}})),
      test = factor({{test}}, levels = unique({{test}}))
    ) |>
    dplyr::arrange(group, test) |>
    ggplot2::ggplot(ggplot2::aes(
      x = forcats::fct_rev(test), y = z
    )) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::coord_flip(ylim = c(-4, 4)) +
    ggplot2::facet_grid(
      rows = dplyr::vars(group),
      scales = "free", space = "free", labeller = ggplot2::labeller(group = ggplot2::label_wrap_gen(30))
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::scale_y_continuous(n.breaks = 10, expand = c(0, 0), labels = labeling_function) +
    ggplot2::scale_x_discrete(labels = abbreviate_test_names) +
    ggplot2::theme(
      strip.text.y.right = ggplot2::element_text(angle = 270, size = 12),
      strip.background = ggplot2::element_rect(color = "black"),
      strip.placement = "left",
      plot.title.position = "plot",
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(y = y_label_text, x = "Neuropsychology Tests")

  if (descriptors) {
    first_group <- data |>  dplyr::pull({{group}}) |>  dplyr::first()
    first_test <- data |>  dplyr::pull({{test}}) |>  dplyr::first()
    first_group_count <- data |>  dplyr::group_by({{group}}) |>
      dplyr::summarize(count = dplyr::n()) |>  dplyr::ungroup() |>  dplyr::slice(1) |>  dplyr::select(count) |>  as.numeric()

    p <- p +
      ggplot2::geom_text(data = data.frame(test = first_test,
                                           y = descriptor_labels$z,
                                           label = descriptor_labels$label,
                                           group = first_group),
                         ggplot2::aes(y = y, x = first_group_count + 0.35, label = label), color = descriptors.color)
  }

  if (shading) {
    p <- p +
      ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1), fill = shading.color, alpha = .1)
  }


  if (!missing(ci.lb) && !missing(ci.ub)) {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = {{ci.lb}}, ymax = {{ci.ub}}), width = .2,
                             position = ggplot2::position_dodge(.9))
  }

  if (z.lines) {
    p <- p +
      ggplot2::geom_hline(yintercept = seq(-4, 4, by = 1), size = 0.5, color = z.line.color)
  }

  return(p)
}
