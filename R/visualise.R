#' Create rainforest plot
#'
#' Create a rainforest plot showing odds ratios + confidence intervals sorted based on significance
#'
#' @param data a data.frame created by [contingency_tables_to_fisher()]. Must include the columns "element", "p.value", "odds_ratio", "conf_level", "conf.int.lower", "conf.int.upper", "fdr".
#' @param max_odds the x axis upper limit. Any odds ratios > max_odds will be depicted as an open square off the chart. Any confidence interval > max_odds will have an arrow pointing off-chart
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' #Step 1: Create a list of two or more vectors.
#' input <- list(
#'   colorectal = c("APC", "APC", "APC", "TP53", "APC"),
#'   melanoma = c("BRAF", "BRAF", "BRAF", "BRAF", "BRAF", "TP53", "APC")
#' )
#'
#' # Step 2: Create contingency tables
#' contingency_tables <- list_to_contingency_tables(input)
#'
#' # Step 3: Compute fisher p values & odds ratios
#' comparison <- contingency_tables_to_fisher(contingency_tables)
#'
#' # Step 5: Visualise Results
#' plot_rainforest(comparison)
plot_rainforest <- function(data, max_odds = 5){

  # Validation
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  required_cols <- c("element", "p.value", "odds_ratio", "conf_level", "conf.int.lower", "conf.int.upper", "fdr")
  if(!is.data.frame(data)) stop("data must be a data.frame")
  missing_cols <- setdiff(required_cols, colnames(data))
  if(length(missing_cols) > 0) stop("data must include the following columns: ", paste0(missing_cols, collapse = ", "))

  # Sort elements by p.value
  data <- data[order(data[["p.value"]], decreasing = TRUE),]
  data[["element"]] <- factor(data[["element"]], levels = data[["element"]])


  # Deal with odds ratios > max_odds
  data[["conf.int.upper.fixed"]] <- ifelse(data[["conf.int.upper"]] > max_odds, max_odds + 0.3,  data[["conf.int.upper"]])
  data[["conf.int.lower.fixed"]] <- ifelse(data[["conf.int.lower"]] > max_odds, max_odds + 0.3,  data[["conf.int.lower"]])
  data[["odds_ratio_fixed"]] <- ifelse(data[["odds_ratio"]] > max_odds, max_odds + 0.3,  data[["odds_ratio"]])

  # Get cohort names
  cols <- colnames(data)
  cols <- cols[startsWith(x = cols, prefix = "present_")]
  cohorts <- unique(sub(x=cols, pattern = "present_", replacement = ""))

  # Create Plot Plot
  ggplot2::ggplot(
    data = data,
    ggplot2::aes(x=pmin(.data[["odds_ratio"]], .data[["odds_ratio_fixed"]]), y=.data[["element"]])
  ) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.5, color = "maroon") +

    # Segments with arrow (upper interval)
    ggplot2::geom_segment(
      data = function(df) {df[df[["conf.int.upper"]] > max_odds,,drop=FALSE]},
      ggplot2::aes(x=.data[["conf.int.lower.fixed"]], xend=.data[["conf.int.upper.fixed"]]),
      arrow = grid::arrow(ends = "last", type = "closed", length = ggplot2::unit(6, units = "points")),
      linetype = "solid"
      ) +

    # Add vertical line to lower end of intervals breaching max_odds
    ggplot2::geom_segment(
      data = function(df) {df[df[["conf.int.upper"]] > max_odds & df[["conf.int.lower"]] <=max_odds,,drop=FALSE]},
      ggplot2::aes(x=.data[["conf.int.lower"]], xend=.data[["conf.int.lower"]]),
      arrow = grid::arrow(ends = "first", type = "open", length = ggplot2::unit(6, units = "points"), angle = 90),
      linetype = "solid"
    ) +

    # Segments without line (upper interval)
    ggplot2::geom_segment(
      data = function(df) {df[!df[["conf.int.upper"]] > max_odds,,drop=FALSE]},
      ggplot2::aes(x=.data[["conf.int.lower"]], xend=.data[["conf.int.upper.fixed"]]),
      arrow = grid::arrow(ends = "both", type = "open", length = ggplot2::unit(6, units = "points"), angle = 90),
      linetype = "solid"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[["odds_ratio"]] > max_odds),
      size=3, show.legend = FALSE
    ) +

    ggplot2::ylab(NULL) +
    ggplot2::xlab("Odds ratio") +
    ggplot2::coord_cartesian(xlim = c(0, max_odds)) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(add = c(0.2, 0.5)),
      oob = scales::oob_squish_infinite,
      breaks = 0:max_odds
    ) +
    ggplot2::scale_y_discrete(
      position="left",
      expand = ggplot2::expansion(add = c(0, 0.6))
    ) +

    # Add line at base bottom
    ggplot2::annotate(geom = "segment", x=0, xend=max_odds, y=0, linewidth=1) +
    # Add text labels
    ggplot2::annotate("text", x = 0.9, y = 3.5, label = paste0( "[", cohorts[1], "]"), hjust=1, fontface="bold", size=5) +
    ggplot2::annotate("text", x = 1.1, y = 3.5, label = paste0( "[",cohorts[2], "]"), hjust=0, fontface="bold", size=5) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 0, "FALSE" = 15)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face="bold", size = 14, hjust = 1)
      )

}
