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

best_chains <- get_chains(B_mu    = 63.52732,
                          a_0     = 0.9350036,
                          b_0     = 31.27473,
                          B_sigma = 1.299851,
                          a_i     = 35.3647,
                          c       = 9.979922,
                          d       = 0.145181)

worst_chains <- get_chains(B_mu    = 107.878122,
                           a_0     = 0.9998557,
                           b_0     = 16.074479,
                           B_sigma = 1.3090875,
                           a_i     = 12.844466,
                           c       = 6.169716,
                           d       = 5.7236424)

save(best_chains, file = "output/best_chains150150.RData")
save(worst_chains, file = "output/worst_chains150150.RData")

# load("output/best_chains.RData")
# load("output/worst_chains.RData")

# assess convergence
chains_traceplot(best_chains, worst_chains)

chains_acf(best_chains, worst_chains)

get_ess(best_chains, worst_chains)

pairplot_best_worst(best_chains, worst_chains, maxpoints=500)

all_predicted <- predicted_real_bw(horizon, best_chains, worst_chains)
fwrite(all_predicted, "output/real_pred_chains_150150.csv")
realpred_best_worst(all_predicted)
