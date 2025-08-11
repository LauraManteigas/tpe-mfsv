# Bayesian-tuned MFSV forecasting
This code accompanies the article "Bayesian-Tuned Multivariate Factor Stochastic Volatility Model: Forecasting Amid Geopolitical Crisis".

It uses TPE to optimize the parameters of a MFSV model, improving 50-day-ahead forecasts for 5 energy futures:
+ Brent
+ WTI
+ Dubai Crude
+ Gasoline
+ Heating Oil (HO)

> [!WARNING]  
> This code is computationally expensive and may take a long time to run.


## Project Structure
```bash
├── data/                         # Input data files (CSV format)                    
├── src/                          # Model outputs and diagnostics                 
│   ├── data-loading.R            # Data reading and preprocessing
│   ├── data-visualization.R      # Data plot highlighting train/test
│   ├── kde.py                    # Multidimensional kernel
│   ├── utils.R                   # Auxiliary functions
│   ├── tpe                       # Outputs specific to TPE (and MFSV)
├── settings.R                    # Package loading and environment setup
├── main.R                        # Get results            
└── README.md     
```

## Notes
Hyperparameters, forecast horizon, and model settings can be adjusted in `Scripts/config.R`.
