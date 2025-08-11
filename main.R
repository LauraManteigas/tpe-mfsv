source("settings.R")


# =====================
# DATA PREPROCESSING
# =====================
data <- calculate_log_returns()
descriptive_statistics(data)
visualize_data(data)

# =====================
# BAYESIAN OPTIMIZATION
# =====================
tpe_params <- load_tpe()

best_params <- head(tpe_params[, .(B_mu, a_0, b_0, B_sigma, a_i, c, d)], 1)
worst_params <- tail(tpe_params[, .(B_mu, a_0, b_0, B_sigma, a_i, c, d)], 1)
