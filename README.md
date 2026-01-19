# Decision-Aware Stochastic Forecasting for Small High-cost Inventory Under Covariate Scarcity

This project explores Bayesian forecasting of small, high-variance inventory data based on a biotech QC environment. Weekly usage is modeled using Poisson–Gamma conjugacy, resulting in a Negative Binomial posterior predictive distribution scaled by forecast horizon.

Project URL: https://thefifthpostulate.github.io/Stochastic-Consumption-Forecasting/InventoryProject.html

The analysis includes:
- Feature engineering from raw inventory records
- Posterior predictive forecasting across rolling windows
- Residual analysis and posterior diagnostics
- Visualization of high-error cases to assess model failure modes

**Key finding:**  
Despite theoretically appropriate modeling, posterior uncertainty remains high due to context-dependent usage patterns. The analysis demonstrates that some operational processes are not well-suited for automated forecasting and are better managed through expert oversight.

This project emphasizes model diagnostics, uncertainty interpretation, and decision relevance over point-estimate accuracy.
