# =====================
# SETUP SCRIPT
# =====================

# Clear environment
rm(list = ls())

# Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  ggplot2, tidyr, dplyr, data.table, zoo, factorstochvol,
  quadprog, tidyverse, bayestestR, gridExtra,
  e1071, tseries, mgcv, parallel, scoringRules,
  reticulate, ggsci, abind, rstan
)

# Set ggplot theme
theme_set(
  theme_bw() + 
    theme(
      legend.position = 'bottom',
      strip.text.x = element_text(size = 8.8),
      axis.title = element_text(size = 8.8),
      axis.text = element_text(size = 6.8),
      legend.text = element_text(size = 6.8),
      legend.title = element_text(size = 8.8),
      plot.margin = unit(c(0, 0, 0, 0), "cm"))
)


# Function to source multiple R scripts
source_scripts <- function(paths) {
  invisible(lapply(paths, source))
}

source_scripts(c(
  "src/data-loading.R",
  "src/data-visualization.R",
  "src/tpe.R",
  "src/utils.R"
))

reticulate::source_python("src/kde.py")



symbols <- c("Brent", "Dubai", "Gasoline", "HO", "WTI")
input_files <- paste0("data/", symbols, ".csv")
m <- length(symbols)

horizon <- 50
q <- 2

mcmc <- list( 
  draws = 150e3,
  burnin = 150e3,
  thin = 1)

search_space <- data.table(
  parameter = c("B_mu", "a_0", "b_0", "B_sigma", "a_i", "c", "d"),
  min = c(1, 1, 1, 0.01, 0.1, 0.1, 0.1),
  max = c(150, 50, 50, 1.0, 5.0, 10.0, 10.0))
p <- ncol(search_space)

tpe <- list(
  init_points = 200, 
  n_evals = 50, 
  n_samples = 200,
  gamma_choices = seq(5, 30, 5)/100)


config_color <- c(
  "Best" = "#00A087B2",
  "Worst" = "#E64B35B2"
)

series_color <- c(
  "Brent" = "#E64B35B2",
  "Dubai" ="#4DBBD5B2",
  "WTI" = "#00A087B2",
  "Gasoline" = "#3C5488B2",
  "HO" = "#EFC011B1"

)
