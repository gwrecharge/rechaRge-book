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

# Input data
# Quiet download
options(datatable.showProgress = FALSE)
# use input example files provided by the package
input_rcn <- "rcn.csv.gz" # RCN values per RCN cell ID
input_climate <- "climate.csv.gz" # precipitation total in mm/d per climate cell ID
input_rcn_climate <- "rcn_climate.csv.gz" # relation between climate and RCN cell IDs
input_rcn_gauging <- "rcn_gauging.csv.gz" # relation between gaugins station and RCN cell IDs
input_observed_flow <- "observed_flow.csv.gz" # flow rates in mm/d
input_alpha_lyne_hollick <- "alpha_lyne_hollick.csv.gz"
# Simulation period
simul_period <- c(2017, 2017)

out_dir <- file.path(getwd(), "caramel")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

hydrobudget_eval <- function(i) {
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

  # Dump parameters and associated objectives
  res1 <- result$simulation_metadata[1,]
  output <- list(
    T_m = res1$T_m,
    C_m = res1$C_m,
    TT_F = res1$TT_F,
    F_T = res1$F_T,
    t_API = res1$t_API,
    f_runoff = res1$f_runoff,
    sw_m = res1$sw_m,
    f_inf = res1$f_inf,
    KGE_qtot = mean(result$simulation_metadata$KGE_qtot_cal),
    KGE_qbase = mean(result$simulation_metadata$KGE_qbase_cal)
  )
  data.table::fwrite(output, file.path(out_dir, "output.csv"), append = TRUE)

  return(c(
    output$KGE_qtot,
    output$KGE_qbase
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
  carallel = 0
  #carallel = 1,
  #numcores = 4
)

#
# Results
#

# Save caRamel results
saveRDS(results_, file.path(out_dir, "results_caramel.rds"))

# Plot using caRamel
png(file.path(out_dir, "plot_caramel.png"), width = 1000, height = 500)
plot_caramel(results_, objnames = c("KGE_qtot", "KGE_qbase"))
dev.off()

# Plot all using ggplot
library(ggplot2)
# Front (as reported by caRamel)
front <- data.table(results_$objectives)
colnames(front) <- c("KGE_qtot", "KGE_qbase")
# Simulations
output <- fread(file.path(out_dir, "output.csv"))
all <- output[, c("KGE_qtot", "KGE_qbase")]
combined_data <- rbind(front, all)
combined_data$group <- c(rep("All", nrow(all)), rep("Front", nrow(front)))
ggplot(combined_data, aes(x = KGE_qtot, y = KGE_qbase, color = group)) +
  geom_point() +
  labs(title = "Scatter plot of simulations",
       x = "KGE_qtot", y = "KGE_qbase",
       color = "Dataset") +
  theme_minimal()
ggsave(file.path(out_dir, "scatter.png"))

# Filtering simulation outputs with front objectives, to get corresponding parameters
round_digits <- 6 # precision loss
output[, c("KGE_qtot", "KGE_qbase") := lapply(.SD, round, digits = round_digits), .SDcols = c("KGE_qtot", "KGE_qbase")]
front[, c("KGE_qtot", "KGE_qbase") := lapply(.SD, round, digits = round_digits), .SDcols = c("KGE_qtot", "KGE_qbase")]
output_front <- output[front, on = .(KGE_qtot, KGE_qbase)]
fwrite(output_front, file.path(out_dir, "output_front.csv"))
