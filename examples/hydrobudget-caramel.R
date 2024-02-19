library(rechaRge)
library(caRamel)

# Input data
# use input example files provided by the package
base_url <-
  "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
input_rcn <-
  data.table::fread(paste0(base_url, "rcn.csv.gz")) # RCN values per RCN cell ID
input_climate <-
  data.table::fread(paste0(base_url, "climate.csv.gz")) # precipitation total in mm/d per climate cell ID
input_rcn_climate <-
  data.table::fread(paste0(base_url, "rcn_climate.csv.gz")) # relation between climate and RCN cell IDs
input_rcn_gauging <-
  data.table::fread(paste0(base_url, "rcn_gauging.csv.gz")) # relation between gaugins station and RCN cell IDs
input_observed_flow <-
  data.table::fread(paste0(base_url, "observed_flow.csv.gz")) # flow rates in mm/d
input_alpha_lyne_hollick <-
  data.table::fread(paste0(base_url, "alpha_lyne_hollick.csv.gz"))

# Number of objectives
#nobj <- 3 #for rmse qtot, qbase, p-q
nobj <- 2 #for rmse qtot, qbase
#number of variables
#nvar <- 8 #8 calibration parameters
nvar <-
  7 #if time to freez the soil is taken out of the optimization
#all the objectives are to be minimized
#minmax <- c(FALSE, FALSE, FALSE) # to minimize the rmse
#minmax <- c(TRUE, TRUE, TRUE) # to maximize the KGE
minmax <- c(TRUE, TRUE) # to maximize the KGE without the P-Q
#range of the parameters
bounds <- matrix(nrow = nvar, ncol = 2)
#bounds[, 1] <- c(-2, 3, 5, -20, 1, 0.5, 0.01, 50)
#bounds[, 1] <- c(-1, 3, -20, 3, 0.5, 0.01, 200)    #for watershed 19
#bounds[, 1] <- c(-1, 3, -17, 2.5, 0.5, 0.01, 95)    #for watershed 25
#bounds[, 1] <- c(-1, 3, -20, 3, 0.53, 0.05, 200)    #for watershed 24
#bounds[, 1] <- c(0, 3.5, -7, 3.5, 0.5, 0.03, 400)    #for watershed 0
#bounds[, 1] <- c(0.5, 3.5, -20, 2.4, 0.52, 0.08, 250)    #for watershed 13
bounds[, 1] <-
  c(1, 4,-20, 3.05, 0.5, 0.01, 160)    #for watershed 47
#bounds[, 1] <- c(-1, 3, -16, 3.2, 0.52, 0.05, 400)    #for watershed 01
#bounds[, 1] <- c(1.3, 4.5, -20, 2, 0.52, 0.02, 350)    #for watershed 16

#bounds[, 2] <- c(2, 6.5, 30, 0, 5, 1, 0.25, 900)
#bounds[, 2] <- c(1, 4.5, -10, 4, 0.6, 0.09, 450)     #for watershed 19
#bounds[, 2] <- c(1.5, 4.5, -4, 4.5, 0.7, 0.05, 650)     #for watershed 25
#bounds[, 2] <- c(2, 5.5, -12, 4.5, 0.66, 0.2, 900)     #for watershed 24
#bounds[, 2] <- c(1.5, 5, -2, 5, 0.55, 0.07, 700)     #for watershed 0
#bounds[, 2] <- c(2, 5, -9, 5, 0.65, 0.25, 900)     #for watershed 13
bounds[, 2] <-
  c(2.5, 6.5,-12, 4.8, 0.6, 0.05, 720)     #for watershed 47
#bounds[, 2] <- c(1.5, 5, -3, 5, 0.7, 0.2, 850)     #for watershed 01
#bounds[, 2] <- c(2.1, 6.3, -12, 4.5, 0.63, 0.09, 900)     #for watershed 16

#T_snow<-0
#T_m<-x1
#C_m<-x2
#temps_gel_sol <- x3
#temp_gel <- x4
#temps_api <- x5
#facteur_rcn_2 <- x6
#pourc_infiltration <- x7
#volume_max_vadose<- x8

# Simulation period
simul_period <- c(2017, 2017)

hydrobudget_eval <- function(i) {
  # Calibration parameters
  HB <- rechaRge::new_hydrobugdet(
    T_m = x[i,1], # melting temperature (°C)
    C_m = x[i,2], # melting coefficient (mm/°C/d)
    TT_F = x[i,3], # Threshold temperature for soil frost (°C)
    F_T = 16.4, # Freezing time (d)
    t_API = x[i,4], # Antecedent precipitation index time (d)
    f_runoff = x[i,5], # Runoff factor (-)
    sw_m = x[i,7], # Maximum soil water content (mm)
    f_inf = x[i,6] # infiltration factor (-)
  )
  HB$rcn_columns <- list(
    rcn_id = "cell_ID",
    RCNII = "RCNII",
    lon = "X_L93",
    lat = "Y_L93"
  )
  HB$climate_columns$climate_id <- "climate_cell"
  HB$rcn_climate_columns <- list(
    climate_id = "climate_cell",
    rcn_id = "cell_ID"
  )
  HB$rcn_gauging_columns <- list(
    rcn_id = "cell_ID",
    station_id = "gauging_stat"
  )
  HB$alpha_lyne_hollick_columns$station_id <- "station"
  print(HB$calibration)

  # Simulation with the HydroBudget model
  water_budget <- rechaRge::compute_recharge(
    HB,
    rcn = input_rcn,
    climate = input_climate,
    rcn_climate = input_rcn_climate,
    period = simul_period
    # nb_core = nb_core
  )

  # Do simultation
  result <- rechaRge::compute_simulation_quality_assessment(
    HB,
    water_budget = water_budget,
    rcn_gauging = input_rcn_gauging,
    observed_flow = input_observed_flow,
    alpha_lyne_hollick = input_alpha_lyne_hollick,
    period = simul_period
  )

  return(c(mean(result$simulation_metadata$KGE_qtot_cal), mean(result$simulation_metadata$KGE_qbase_cal)))
}

results_ <- caRamel(
  nobj = nobj,
  nvar = nvar,
  minmax =  minmax,
  bounds = bounds,
  func = hydrobudget_eval,
  popsize = 50,
  archsize = 100,
  maxrun = 500,
  prec = matrix(0.01, nrow = 1, ncol = nobj),

  carallel = FALSE
)
