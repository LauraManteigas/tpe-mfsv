#' Equal Breaks for Y-axis
#' 
#' Generates approximately equally spaced y-axis breaks with slight margins.
#' 
#' @param n Number of breaks to generate.
#' @param s Margin scaling factor. Default is 0.1 (10% of range).
#' @return A function that takes a numeric vector and returns rounded break 
#' values.
equal_breaks <- function(n, s = 0.1) {
  function(x) {
    d <- s * diff(range(x)) / (1 + 2 * s) 
    seq <- seq(min(x) + d, max(x) - d, length = n)
    round(seq, -floor(log10(abs(seq[2] - seq[1]))))
  }
}


#' Visualize Time Series with Train/Test Highlight
#' 
#' Plots time series data with facets for each series, highlighting the test 
#' period.
#' 
#' @param data A data.table containing columns: date, stage, and series symbols.
#' @param symbols Character vector of column names representing time series.
#' @return A ggplot object showing time series per symbol with test period 
#' shaded.
visualize_data <- function(data) {
  long_data <- melt(
    data, 
    id.vars = c("date", "stage"), 
    measure.vars = symbols, 
    variable.name = "series", 
    value.name = "value")
  
  min_returns <- min(long_data$value)
  max_returns <- max(long_data$value)
  
  train_data <- long_data[stage == "train", .(date, series, value)]
  test_data <- long_data[stage == "test", .(date, series, value)]
  
  ggplot(long_data, aes(x = date, y = value)) +
    annotate(
      "rect", 
      xmin = min(test_data$date), 
      xmax = max(test_data$date), 
      ymin = -Inf, 
      ymax = Inf, 
      alpha = 0.2, 
      fill = "blue") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~series, scales = "free", nrow = m) +
    labs(title = "Time Series of Demeaned Log Returns", x = NULL, y = NULL) +
    scale_x_date(
      date_labels = "%b %Y", 
      date_breaks = "6 months", 
      expand = c(0, 0)) +
    scale_y_continuous(breaks = equal_breaks(n = 4, s = 0.1)) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 1),
      axis.text = element_text(size = 6),
      strip.text = element_text(face = "italic", size = 8)
    )
}




