#' Read and Preprocess CSV Data
#' 
#' Reads a CSV file and extracts date and numeric price columns.
#' 
#' @param filename Path to the CSV file.
#' @param symbol Name to assign to the numeric price column.
#' @return A data.table with `date` and named price column.
read_and_preprocess_data <- function(filename, symbol) {
  stopifnot(file.exists(filename))
  data <- fread(filename, select = c("Date", "Price"))
  data[, c("month", "day", "year") := tstrsplit(Date, "/", fixed = TRUE)]
  data[, date := as.Date(paste(day, month, year, sep = "/"), format = "%d/%m/%Y")]
  data[, (symbol) := as.numeric(gsub(",", "", Price))]
  
  if (any(is.na(data[[symbol]]))) {
    warning(sprintf("Some prices in %s could not be converted to numeric.", filename))
  }
  
  return(data[, .(date, get(symbol))][, setnames(.SD, "V2", symbol)])
}


#' Merge Multiple Time Series by Date
#' 
#' Reads and merges multiple datasets by their `date` columns.
#'
#' @return A merged data.table with `date` and all series.
merge_data <- function() {
  datasets <- Map(read_and_preprocess_data, input_files, symbols)
  data <- Reduce(function(x, y) merge(x, y, by = "date"), datasets)
  return(data)
}


#' Split Data into Train and Test Sets
#' 
#' Adds a `stage` column ("train" or "test") based on cutoff date.
#'
#' @return A data.table with an added `stage` column.
cut_data <- function() {
  data <- merge_data()
  
  if (horizon >= nrow(data)) stop("`horizon` must be smaller than the dataset size.")
  
  test_start <- data$date[length(data$date) - horizon]
  test_end <- tail(data$date, 1)
  
  train_start <- data$date[1]
  train_end <- data$date[length(data$date) - horizon - 1]
  train_days <- sum(data$date >= train_start & data$date <= train_end)
  
  message(
    "Training: ", train_start, " to ", train_end, " (", train_days, " points).",
    "\nTesting: ", test_start, " to ", test_end, " (", horizon, " points).")
  
  data[, stage := factor(ifelse(date <= as.Date(test_start), "train", "test"))]
  
  return(data)
}


#' Calculate Demeaned Log Returns
#' 
#' Computes log-returns and demeans based on training mean.
#'
#' @return A data.table of demeaned log returns with `date` and `stage`.
calculate_log_returns <- function() {
  data <- cut_data()
  log_returns <- data[, lapply(.SD, function(x) 100 * diff(log(x))), .SDcols = symbols]
  train_mean <- log_returns[data$stage[-1] == "train", lapply(.SD, mean), .SDcols = symbols]
  log_returns <- data.table(
    date = data$date[-1], 
    log_returns[, Map(`-`, .SD, train_mean), .SDcols = symbols],
    stage = data$stage[-1]
  )
  
  return(log_returns[])
}


#' Compute Summary Statistics
#' 
#' Computes descriptive statistics for selected series and metrics.
#' 
#' @param data A data.table containing the time series.
#' @return A data.table with computed statistics by series.
compute_metrics <- function(data, by = NULL) {
  metrics <- c("mean", "sd", "Q1", "Q2", "Q3", "excess_kurtosis", "skewness", 
              "JB", "ADF")
  
  data[, {
    metric_values <- lapply(.SD, function(x) {
      sapply(metrics, function(metric) {
        switch(metric,
               mean = round(mean(x), 3),
               sd = round(sd(x), 3),
               Q1 = round(quantile(x, 0.25), 3),
               Q2 = round(quantile(x, 0.5), 3),
               Q3 = round(quantile(x, 0.75), 3),
               excess_kurtosis = round(kurtosis(x) - 3, 3),
               skewness = round(skewness(x), 3),
               JB = round(jarque.bera.test(x)$p.value, 3),
               ADF = round(adf.test(x)$p.value, 3),
               stop(sprintf("Unsupported metric: %s", metric)))
      })
    })
    data.table(metric = metrics, setNames(as.data.table(metric_values), symbols))
  }, by = by, .SDcols = symbols]
}


#' Descriptive Statistics Summary
#' 
#' Computes and combines statistics by stage and overall.
#' 
#' @param data A data.table of log returns with `stage` column.
#' @return A combined data.table of statistics for each stage and overall.
descriptive_statistics <- function(data) {
  by_stage <- compute_metrics(data, by = "stage")
  overall <- compute_metrics(data)[, stage := "all"]
  
  result <- rbind(by_stage, overall)
  return(result)
}
