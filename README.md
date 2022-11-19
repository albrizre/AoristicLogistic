R code for the paper *A Bayesian aoristic logistic regression to model spatio-temporal crime risk under the presence of interval-censored event times*.

The scripts 'code_event_times_complete_cases.R' and 'code_event_times_missing.R' contain the NIMBLE code to implement the complete cases and full model, respectively, described in the paper. The scripts 'Model_fit_complete_cases_simulation.R' and 'Model_fit_missing_times_full_simulation.R' define the structure of the data, constants, and initial values in order to call the $\textsf{nimbleMCMC}$ function accordingly.

Folder 'Data' contains six simulated datasets that correspond to the six scenarios analyzed in the simulation study included in the paper.
Folder 'Functions' contains a code to implement temporal random effects based on a second-order random walk.
Folder 'Boroughs' contains the shapefile of the boroughs of Valencia (Spain), which allow including a spatial random effect in the model.


