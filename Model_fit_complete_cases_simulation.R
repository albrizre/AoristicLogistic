library(spatstat)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(nimble)
library(ggplot2)
library(spdep)
library(Rcpp)
library(pkgbuild)

# Load data

load("Data/rel.rda")
load("Data/aux_Week_SpUnit_num.rda")
source("Functions/RW2.R")
source("code_event_times_complete_cases.R")
source("code_event_times_missing.R")

for (i in 1:6){

  # Load data
  
  load(paste0("Data/data_analysis_scenario_",i,".rda"))
  
  # Remove first rows with NA in Week

  data_analysis=data_analysis[-which(is.na(data_analysis$Week) & !is.na(data_analysis$Day_event)),]

  # Consider complete cases only

  data_analysis=data_analysis[data_analysis$Uncertainty_intro==0,]

  # RW2 structure for the time

  # Week
  W <- max(data_analysis$Week,na.rm=T)
  RW2_W <- RW2(W)

  # Day
  D <- max(data_analysis$Day_event,na.rm=T)
  RW2_D <- RW2(D)

  # Spatial structure

  # Load grid Valencia

  grid=readOGR("Boroughs/barrios.shp")
  proj4string(grid)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  grid=spTransform(grid,"+proj=utm +zone=30 ellps=WGS84")
  plot(grid)
  # Filter 
  grid=grid[grid$CODDISTRIT%in%1:16,]
  grid@data$SpUnit=1:length(grid)
  grid_nb=poly2nb(grid,snap=1.5)
  grid_WB=nb2WB(grid_nb)

  constants <- list(
   
  N = nrow(data_analysis),  
  G = length(grid),  
  N_INT = length(grid)*length(RW2_W$num),
  
  N_W = length(RW2_W$num),
  N_D = length(RW2_D$num),

  N_W_adj = length(RW2_W$adj),
  N_D_adj = length(RW2_D$adj),
  N_grid_w = length(grid_WB$adj),
  
  adj = RW2_W$adj,                             
  num = RW2_W$num,
  weights = RW2_W$weights, 
  
  adj2 = RW2_D$adj,                             
  num2 = RW2_D$num,
  weights2 = RW2_D$weights,
  
  adj_grid = grid_WB$adj,                             
  num_grid = grid_WB$num,
  weights_grid = grid_WB$weights,
  
  Tuesday = data_analysis$Tuesday, 
  Wednesday = data_analysis$Wednesday, 
  Thursday = data_analysis$Thursday, 
  Friday = data_analysis$Friday, 
  Saturday = data_analysis$Saturday, 
  Sunday = data_analysis$Sunday, 
  
  Week = data_analysis$Week,
  Day_event = data_analysis$Day_event,
  
  Day_from = data_analysis$Day_from,
  Day_to = data_analysis$Day_to,
  
  SpUnit = data_analysis$SpUnit,
  
  Week_SpUnit = data_analysis$Week_SpUnit,
  
  rel_Day_event = rel$Day_event,
  rel_Week = rel$Week,
  rel_Tuesday = rel$Tuesday,
  rel_Wednesday = rel$Wednesday,
  rel_Thursday = rel$Thursday,
  rel_Friday = rel$Friday,
  rel_Saturday = rel$Saturday,
  rel_Sunday = rel$Sunday,
  aux_Week_SpUnit_num = aux_Week_SpUnit_num
  )

  # Fit model

  data <- list(Response = data_analysis$Response)
  inits <- function() list(alpha = 0,
                         beta_tuesday = 0, 
                         beta_wednesday = 0, 
                         beta_thursday = 0, 
                         beta_friday = 0, 
                         beta_saturday = 0, 
                         beta_sunday = 0, 
                         sigma2.delta=0.1,
                         sigma2.epsilon=0.1,
                         sigma2.u=0.1,
                         sigma2.v=0.1,
                         delta=rep(0,constants$N_W),
                         epsilon=rep(0,constants$N_W),
                         u=rep(0,constants$G),
                         v=rep(0,constants$G))
  mcmc.output <- nimbleMCMC(code_event_times_complete_cases, data = data, inits = inits, constants = constants,
                          monitors = c("alpha", 
                                       "beta_tuesday", 
                                       "beta_wednesday", 
                                       "beta_thursday", 
                                       "beta_friday",
                                       "beta_saturday",
                                       "beta_sunday",
                                       "delta",
                                       "epsilon",
                                       "u",
                                       "v",
                                       "tau.delta",
                                       "tau.epsilon",
                                       "tau.u",
                                       "tau.v",
                                       "pi"), thin = 10,
                          niter = 20000, nburnin = 4000, nchains = 1, 
                          summary = TRUE, WAIC = TRUE)
  save(mcmc.output, file=paste0("Models/model_complete_cases_scenario_",i,".rda"))

}
