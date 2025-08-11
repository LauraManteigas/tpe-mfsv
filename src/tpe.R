#' Run FSV Sampler with Specified Prior Parameters
#'
#' Uses the `fsvsample` function to run Bayesian estimation of a MFSV model.
#'
#' @param B_mu Scalar prior for mean of log-volatility innovations
#' @param a_0 Shape parameter of Beta prior for persistence
#' @param b_0 Shape parameter of Beta prior for persistence
#' @param B_sigma Prior variance of idiosyncratic and factor volatilities
#' @param a_i Prior for factor loadings
#' @param c NG prior shape
#' @param d NG prior rate
#' @return An object of class `fsvdraws` containing posterior samples
run_mfsv <- function(B_mu, a_0, b_0, B_sigma, a_i, c, d) {
  train_data <- data[data[["stage"]] == "train", ]
  train_dates <- train_data$date
  zoo_train <- zoo(train_data[, -c("stage", "date")], order.by = train_dates)
  
  fsvsample(
    zoo_train,
    
    zeromean = FALSE,
    
    factors = q, 
    draws = mcmc$draws, 
    thin = mcmc$thin,
    burnin = mcmc$burnin,
    
    priormu = c(0, B_mu),
    
    priorsigmaidi = B_sigma,
    priorsigmafac = B_sigma,
    
    priorphiidi = c(a_0, b_0),
    priorphifac = c(a_0, b_0),
    
    restrict = "none",
    priorfacloadtype = "rowwiseng",
    priorfacload = a_i,
    priorng = c(c, d),
    
    interweaving = 4,
    
    quiet = TRUE
  )
}


#' Predict Future Returns from FSV Sample
#'
#' Generates predictive draws from a fitted model for a specified step ahead.
#'
#' @param mfsv_sample Fitted MFSV model object
#' @return A matrix of simulated returns for each series
predict_steps_ahead <- function(mfsv_sample) {
  predobj <- predcond(mfsv_sample, ahead = horizon, each = 1)
  means_vec <- as.vector(predobj$means[,,1])
  vols_vec <- as.vector(predobj$vols[,,1])
  t(matrix(rnorm(length(means_vec), mean = means_vec, sd = vols_vec), nrow = 5))
}


#' Generate Multi-Step Predictions from MFSV Model
#'
#' Runs predictions from the MFSV model for a sequence of future days.
#'
#' @param mfsv_sample Fitted MFSV model object
#' @return A list of matrices with predictive draws per step
compute_predictions <- function(mfsv_sample) {
  lapply(seq_len(horizon), function(h) {
    predict_steps_ahead(mfsv_sample)
  })
}


#' Evaluate Forecast Performance
#'
#' Evaluate the forecast performance of a MFSV model. Computes mean CRPS 
#' (average across all Series and time points)
#'
#' @param mfsv_sample Fitted MFSV model object
#' @return A `numeric` value with mean CRPS
#'
evaluate_model <- function(mfsv_sample){
  y_pred <- compute_predictions(mfsv_sample)
  y_true <- t(data[data[["stage"]] == "test", ][, -c("stage", "date")])
  crps_results <- matrix(NA, nrow = m, ncol = horizon)
  
  for (series in 1:m) {
    for (day in 1:horizon) {
      samples <- y_pred[[day]][series, ]
      crps_results[series, day] <- crps_sample(y = y_true[series, day], dat = samples)
    }
  }
  
  avg_crps_series <- rowMeans(crps_results)
  agg_crps <- mean(avg_crps_series)
  return(agg_crps)
}


#' Random initialization
#'
#' Sample uniformly `init_points` hyperparameter configurations the search
#' spache.
#'
#' @return A `data.table` of (B_mu, a_0, b_0, B_sigma, a_i, c, d)
sample_uniform <- function() {
  samples <- search_space[, .(value = runif(tpe$init_points, min, max)),
                            by = .(parameter)]
  samples[, draw := seq_len(.N), by = parameter]
  samples_wide <- dcast(samples, draw ~ parameter, value.var = "value")[, -"draw"]
  return(samples_wide[])
}


#' Run Optimization
#'
#' Split into good/bad distributions, select the next candidate of (B_mu, a_0, 
#' b_0, B_sigma, a_i, c, d), and finally get the score
#'
#' @return A `data.table` of (B_mu, a_0, b_0, B_sigma, a_i, c, d) with 
#' respective score, iteration number, and gamma
segment_distributions <- function(gamma) {
  sample_params <- sample_uniform()
  param_list <- split(sample_params, seq(tpe$init_points))
  
  # Run model for init_points sets of params
  mfsv_results <- lapply(param_list, function(params) {
    run_mfsv(
      B_mu    = params$B_mu,
      a_0     = params$a_0,
      b_0     = params$b_0,
      B_sigma = params$B_sigma,
      a_i     = params$a_i,
      c       = params$c,
      d       = params$d
    )
  })
  
  mfsv_agg_crps <- sapply(mfsv_results, evaluate_model)
  mfsv_summary <- mapply(function(params, crps) {
    c(params, agg_crps = crps)
  }, param_list, mfsv_agg_crps, SIMPLIFY = FALSE)
  trials <- do.call(rbind, lapply(mfsv_summary, as.data.table))
  
  cut <- quantile(trials$agg_crps, probs = gamma)
  
  l_x <- subset(trials, agg_crps < cut, select = -agg_crps)
  g_x <- subset(trials, agg_crps >= cut, select = -agg_crps)
  l_mat <- as.matrix(l_x)
  g_mat <- as.matrix(g_x)
  l_t <- transform_samples(l_mat, search_space)
  g_t <- transform_samples(g_mat, search_space)

  b_lx <- nrow(l_x) ** (-1 / (7 + 4))
  b_gx <- nrow(l_x) ** (-1 / (7 + 4))
  kde_models <- compute_kde(l_t, g_t, b_lx, b_gx)
  l_kde <- kde_models$l_kde
  g_kde <- kde_models$g_kde
  
  optimal_hps_t <- choose_next_hps(l_kde, g_kde)
  optimal_hps_t <- as.matrix(t(optimal_hps_t))
  colnames(optimal_hps_t) <- colnames(l_x)
  optimal_hps <- inverse_transform_samples(optimal_hps_t, search_space)
  
  final <- data.table(optimal_hps)
  
  return(final[])
}


#' Run TPE
#'
#' Run the full pipeline of TPE, considering several different gammas, and 
#' store the results.
#'
#' @return A `data.table` of (B_mu, a_0, b_0, B_sigma, a_i, c, d) with 
#' respective score, iteration number, and gamma
run_tpe <- function(){
  gamma_choices <- c(.05, .1, .15, .2)
  
  row_counter <- 1
  
  results_bo <- matrix(NA, nrow = length(tpe$gamma_choices) * tpe$n_evals, ncol = 10)
  colnames(results_bo) <- c("gamma", "iter", 
                            "B_mu", "a_0", "b_0", "B_sigma", "a_i", "c", "d",
                            "crps")
  
  row_counter <- 1
  
  for (g in tpe$gamma_choices){
    for (e in 1:tpe$n_evals) {
        next_params <- segment_distributions(g)
        next_mfsv <- run_mfsv(
          B_mu    = next_params$B_mu,
          a_0     = next_params$a_0,
          b_0     = next_params$b_0,
          B_sigma = next_params$B_sigma,
          a_i     = next_params$a_i,
          c       = next_params$c,
          d       = next_params$d
        )
        next_crps <- evaluate_model(next_mfsv)
        
        results_bo[row_counter, ] <- c(g, e, as.numeric(next_params), next_crps)
        row_counter <- row_counter + 1
    }
  }
  
  results_bo <- data.table(results_bo)[order(crps)]
  
  return(results_bo[])
}


#' Load or Compute TPE results
#'
#' If TPE has already been ran, read and load the results. If not, run the 
#' algorithm, save the results and return them.
#'
#' @return A `data.table` of TPE results.
load_tpe <- function() {
  file_path <- "output/tpe_results.csv"
  
  if (file.exists(file_path)) {
    tpe_res <- fread(file_path)
    cat("File loaded successfully.\n")
  } else {
    cat("Computing data and saving to:", file_path, "\n")
    tpe_res <- run_tpe()
    fwrite(data.table(tpe_res), file_path)
  }
  
  return(tpe_res)
}
