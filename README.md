# Masters_Thesis
Quantitative Analysis of Shift in Search Interest of Non-Consensual Sexualised GenAI Content following new AI Model Launches. This repository contains the full research pipeline for my Master of Science in Criminology thesis at the University of Pennsylvania. This looks at search volume trends of deepfake technology and AI-generated adult content.

I am building a time-series computational crime forecasting model that predicts NCII-related search demand using past behavioral signals and platform release indicators. Concretely, the dependent variable will be weekly (log-transformed) NCII Google Trends intensity, and the predictors will include multiple lags of NCII demand (e.g., 1–8 weeks), lagged search interest from related markets (privacy-focused platforms, mainstream AI terms, mainstream adult sites), and dummy variables for major AI model releases. I plan to compare regularised regression approaches such as Ridge and Lasso, as well as Gradient Boosting, using rolling time-series cross-validation to avoid data leakage and ensure proper out-of-sample evaluation. 

The goal of this model is not causal inference but forward-looking risk prediction to assess how well we can forecast shifts in NCII demand and to simulate how future platform iterations might alter demand trajectories.





