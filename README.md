# employment-contract-type
MSc Thesis Project Files

* file 'data_trimming': R syntax for creating snapshot dataset(s)
* folder 'identifying_covariate': all files for finding least relevant covariate
* folder '????_dataset': all files related to the analyses of the ???? dataset
  - step0_????.lgs creates the data????Step0.sav dataset.
  - '.brew' files: template(s) for creating '.lgs' files with function 'brew' in R
  - generate-model-????.Rmd: creates LG syntax for all models of interest
    - (the models should be estimated in LG by opening the files and selecting 'estimate all')
  - generate-step2-????.Rmd: creates LG syntax for all models of step2 of the stepwise LRT method
    - (the models should be estimated in LG by opening the files and selecting 'estimate all')
  - final-LRT-????.Rmd: application of stepwise LRT method on ???? dataset
  - final-BIC-????.Rmd: application of stepwise LRT method on ???? dataset
* folder 'compare_years_BIC': manual comparison in R for the best-fitting models according to BIC in different years
* folder 'profiles': profile (per category level) for each best-fitting model per method and covariate
  - the parameter estimates were obtained by estimating the best-fitting model in LG and copying output
* folder 'simulation': all files related to the simulation study
  - '.brew' files: template(s) for creating '.lgs' files with function 'brew' in R
  - exampleData.dat: example dataset for simulating data with LG
  - generate_simulate_estimate.Rmd: creates LG syntax, simulates data by passing to LG, and estimates all models for all simulation conditions
  - final-LRT-simulated.Rmd: application of stepwise LRT method on simulated datasets
  - final-AIC-BIC-simulated.Rmd: application of exhaustive BIC method on simulated datasets
  - simulation_covariate_analyses.Rmd: parameters and stepwise selection of simulation covariates
