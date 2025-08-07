# Bayesian-tuned MFSV forecasting
This code accompanies the article "Bayesian-Tuned Multivariate Factor Stochastic Volatility Model: Forecasting Amid Geopolitical Crisis".

It integrates TPE into a MFSV model to improve 50-day-ahead forecasts for 5 energy futures:
+ Brent
+ WTI
+ Dubai Crude
+ Gasoline
+ Heating Oil (HO)

> [!WARNING]  
> This code is computationally expensive and may take a long time to run.


## Project Structure
```bash
├── Data/                              # Input data files (CSV format)                    
├── Results/                           # Model outputs and diagnostics
│   ├── MFSV/                          # Outputs specific to MFSV
├── Scripts/
│   ├── config.R                       # Model configuration and parameters               
│   ├── setup.R                        # Package loading and environment setup          
│   ├── 00-data-loading.R              # Data reading and preprocessing
│   ├── 01-data-visualization.R        # Data plot highlighting train/test
│   ├── 02-mfsv.R                      # MFSV model estimation  
│   ├── 03-bayesian-optimization.R     # Hyperparameter tuning via Bayesian Optimization
│   ├── 04-save-results.R              # Save results and intermediate outputs   
│   └── 05-mfsv-visualization.R        # Forecast and volatility plots
├── main.R                             # Main workflow script                   
└── README.md     
```

## Notes
Hyperparameters, forecast horizon, and model settings can be adjusted in `Scripts/config.R`.
