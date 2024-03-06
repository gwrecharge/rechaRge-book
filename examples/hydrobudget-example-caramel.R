library(rechaRge)
library(caRamel)
library(data.table)

# Number of objectives
nobj <- 2
# Number of variables
nvar <- 8
# All the objectives are to be maximized
minmax <- c(TRUE, TRUE)
# Range of the parameters
bounds <- matrix(nrow = nvar, ncol = 2)
bounds[, 1] <- c(1, 4, -20, 5, 3.05, 0.5, 160, 0.01)
bounds[, 2] <- c(2.5, 6.5, -12, 30, 4.8, 0.6, 720, 0.05)

hydrobudget_eval <- function(i) {
  # Input data
  # Quiet download
  options(datatable.showProgress = FALSE)
  # use input example files provided by the package
  base_url <- "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
  input_rcn <- paste0(base_url, "rcn.csv.gz") # RCN values per RCN cell ID
  input_climate <- paste0(base_url, "climate.csv.gz") # precipitation total in mm/d per climate cell ID
  input_rcn_climate <- paste0(base_url, "rcn_climate.csv.gz") # relation between climate and RCN cell IDs
  input_rcn_gauging <- paste0(base_url, "rcn_gauging.csv.gz") # relation between gauging station and RCN cell IDs
  input_observed_flow <- paste0(base_url, "observed_flow.csv.gz") # flow rates in mm/d
  input_alpha_lyne_hollick <- paste0(base_url, "alpha_lyne_hollick.csv.gz")
  # Simulation period
  simul_period <- c(2017, 2017)
  # Calibration parameters
  HB <- rechaRge::new_hydrobugdet(
    T_m = x[i, 1],
    # melting temperature (°C)
    C_m = x[i, 2],
    # melting coefficient (mm/°C/d)
    TT_F = x[i, 3],
    # Threshold temperature for soil frost (°C)
    F_T = x[i, 4],
    # Freezing time (d)
    t_API = x[i, 5],
    # Antecedent precipitation index time (d)
    f_runoff = x[i, 6],
    # Runoff factor (-)
    sw_m = x[i, 7],
    # Maximum soil water content (mm)
    f_inf = x[i, 8] # infiltration factor (-)
  )
  # Input data specific settings
  HB$rcn_columns <- list(
    rcn_id = "cell_ID",
    RCNII = "RCNII",
    lon = "X_L93",
    lat = "Y_L93"
  )
  HB$climate_columns$climate_id <- "climate_cell"
  HB$rcn_climate_columns <- list(climate_id = "climate_cell",
                                 rcn_id = "cell_ID")
  HB$rcn_gauging_columns <- list(rcn_id = "cell_ID",
                                 station_id = "gauging_stat")
  HB$alpha_lyne_hollick_columns$station_id <- "station"

  # Simulation with the HydroBudget model
  water_budget <- rechaRge::compute_recharge(
    HB,
    rcn = input_rcn,
    climate = input_climate,
    rcn_climate = input_rcn_climate,
    period = simul_period,
    nb_core = 1
  )

  # Evaluate simulation quality
  result <- rechaRge::compute_simulation_quality_assessment(
    HB,
    water_budget = water_budget,
    rcn_gauging = input_rcn_gauging,
    observed_flow = input_observed_flow,
    alpha_lyne_hollick = input_alpha_lyne_hollick,
    period = simul_period
  )

  return(c(
    mean(result$simulation_metadata$KGE_qtot_cal),
    mean(result$simulation_metadata$KGE_qbase_cal)
  ))
}

#
# Simulations
#

results_ <- caRamel(
  nobj = nobj,
  nvar = nvar,
  minmax =  minmax,
  bounds = bounds,
  func = hydrobudget_eval,
  popsize = 50,
  archsize = 100,
  maxrun = 200,
  prec = matrix(0.01, nrow = 1, ncol = nobj),
  carallel = 1,
  numcores = 2
)

#
# Results
#

# Merging simulation outputs with front objectives, to get corresponding parameters
output_front <- data.table::data.table(cbind(results_$parameters, results_$objectives))
colnames(output_front) <- c("T_m", "C_m", "TT_F", "F_T", "t_API", "f_runoff", "sw_m", "f_inf", "KGE_qtot", "KGE_qbase")

# Plot using caRamel
plot_caramel(results_, objnames = c("KGE_qtot", "KGE_qbase"))

# Plot all using ggplot
library(ggplot2)
front <- data.table(results_$objectives)
colnames(front) <- c("KGE_qtot", "KGE_qbase")
all <- data.table(results_$total_pop[, 9:10])
colnames(all) <- c("KGE_qtot", "KGE_qbase")
combined_data <- rbind(all, front)
combined_data$group <- c(rep("All", nrow(all)), rep("Front", nrow(front)))
ggplot(combined_data, aes(x = KGE_qtot, y = KGE_qbase, color = group)) +
  geom_point() +
  labs(title = "Scatter plot of simulations",
       x = "KGE_qtot", y = "KGE_qbase",
       color = "Dataset") +
  theme_minimal()
