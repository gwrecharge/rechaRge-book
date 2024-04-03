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

make_hydrobudget_eval <- function() {
  # Preload input data
  # Quiet download
  options(datatable.showProgress = T)
  # use input example files provided by the package
  base_url <- "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
  input_rcn <- fread(paste0(base_url, "rcn.csv.gz")) # RCN values per RCN cell ID
  input_climate <- fread(paste0(base_url, "climate.csv.gz")) # precipitation total in mm/d per climate cell ID
  input_rcn_climate <- fread(paste0(base_url, "rcn_climate.csv.gz")) # relation between climate and RCN cell IDs
  input_rcn_gauging <- fread(paste0(base_url, "rcn_gauging.csv.gz")) # relation between gauging station and RCN cell IDs
  input_observed_flow <- fread(paste0(base_url, "observed_flow.csv.gz")) # flow rates in mm/d
  input_alpha_lyne_hollick <- fread(paste0(base_url, "alpha_lyne_hollick.csv.gz"))
  # Simulation period
  simul_period <- c(2017, 2017)

  hydrobudget_eval <- function(i) {
    # Calibration parameters
    HB <- rechaRge::new_hydrobudget(
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
    rechaRge::with_verbose(FALSE)
    water_budget <- rechaRge::compute_recharge(
      HB,
      rcn = input_rcn,
      climate = input_climate,
      rcn_climate = input_rcn_climate,
      period = simul_period,
      workers = 1
    )

    # Evaluate simulation quality
    quality <- rechaRge::evaluate_simulation_quality(
      HB,
      water_budget = water_budget,
      rcn_gauging = input_rcn_gauging,
      observed_flow = input_observed_flow,
      alpha_lyne_hollick = input_alpha_lyne_hollick,
      period = simul_period
    )

    return(c(
      mean(quality$simulation_metadata$KGE_qtot_cal),
      mean(quality$simulation_metadata$KGE_qbase_cal)
    ))
  }

  return(hydrobudget_eval)
}


#
# Calibration with sensitivity
#

results_ <- caRamel(
  nobj = nobj,
  nvar = nvar,
  minmax =  minmax,
  bounds = bounds,
  sensitivity = TRUE,
  func = make_hydrobudget_eval(),
  popsize = 10,
  archsize = 100,
  maxrun = 10,
  prec = matrix(0.01, nrow = 1, ncol = nobj),
  carallel = 1,
  numcores = 2
)

#
# Results
#

results_
# saveRDS(results_, "results.rds")

# Merging simulation outputs with front objectives, to get corresponding parameters
output_front <- data.table::data.table(cbind(results_$parameters, results_$objectives))
colnames(output_front) <- c("T_m", "C_m", "TT_F", "F_T", "t_API", "f_runoff", "sw_m", "f_inf", "KGE_qtot", "KGE_qbase")
# Order by "best" score
y <- 0.6
output_front[, `:=`(KGE_score = (KGE_qtot * (1 - y) + KGE_qbase * y))]
output_front <- output_front[order(KGE_score, decreasing = TRUE)]
output_front

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


#
# Use all front parameters in simulations
#

# for each proposed set of parameters, run the water budget simulation
param_ids <- as.numeric(rownames(output_front))

# preload input data sets
library(data.table)
base_url <- "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
input_rcn <- fread(paste0(base_url, "rcn.csv.gz")) # RCN values per RCN cell ID
input_climate <- fread(paste0(base_url, "climate.csv.gz")) # precipitation total in mm/d per climate cell ID
input_rcn_climate <- fread(paste0(base_url, "rcn_climate.csv.gz")) # relation between climate and RCN cell IDs
# Simulation period
simul_period <- c(2017, 2017)

# run simulations
water_budgets <- lapply(param_ids, FUN = function(i) {
  print(output_front[i,])
  x <- output_front
  HB <- rechaRge::new_hydrobudget(
    T_m = as.numeric(x[i, 1]),
    C_m = as.numeric(x[i, 2]),
    TT_F = as.numeric(x[i, 3]),
    F_T = as.numeric(x[i, 4]),
    t_API = as.numeric(x[i, 5]),
    f_runoff = as.numeric(x[i, 6]),
    sw_m = as.numeric(x[i, 7]),
    f_inf = as.numeric(x[i, 8])
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

  # Simulation with the HydroBudget model
  rechaRge::with_verbose(TRUE)
  rechaRge::with_progress(TRUE)
  rechaRge::compute_recharge(
    HB,
    rcn = input_rcn,
    climate = input_climate,
    rcn_climate = input_rcn_climate,
    period = simul_period,
    workers = 2
  )
})

# make one row per year-month and one column per simulated measure
plot_metric <- function(water_budgets, metric, title = NULL) {
  # spatialized: add metric values starting from best fit
  metrics <- data.table(year = water_budgets[[1]]$year,
                        month = water_budgets[[1]]$month)
  for (i in param_ids) {
    param_id <- paste0(metric, i)
    set(metrics, j = param_id, value = water_budgets[[i]][[metric]])
  }

  # non-spatialized: calculate mean, group by year-month
  metrics_monthly <- unique(metrics, by = c("year", "month"))[, c("year", "month")]
  for (i in param_ids) {
    param_id <- paste0(metric, i)
    metrics_id <- metrics[ , .(mean = mean(get(param_id))), by = c("year", "month")]
    set(metrics_monthly, j = param_id, value = metrics_id$mean)
  }
  # add mean and sd for each year-month row
  ym_cols <- c("year", "month")
  set(metrics_monthly, j = "mean", value = apply(metrics_monthly[, !..ym_cols], 1, mean))
  set(metrics_monthly, j = "sd", value = apply(metrics_monthly[, !..ym_cols], 1, sd))
  set(metrics_monthly, j = "date", value = as.POSIXct(paste(metrics_monthly$year, metrics_monthly$month, "1", sep="-")))
  set(metrics_monthly, j = "min", value = metrics_monthly$mean - metrics_monthly$sd)
  set(metrics_monthly, j = "max", value = metrics_monthly$mean + metrics_monthly$sd)
  colnames(metrics_monthly)[[3]] <- "best"

  # plot uncertainty
  library(ggplot2)
  library(scales)
  ggplot(data = metrics_monthly, aes(x = date)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "gray", alpha = 0.4) +
    geom_line(aes(y = best, color = "best")) +
    geom_line(aes(y = mean, color = "mean")) +
    labs(title = title, color = metric, x = "date", y = metric) +
    scale_color_manual(values = c(best = "red", mean = "cyan")) +
    scale_x_datetime(date_labels = "%Y-%m", breaks = date_breaks("months")) +
    theme(axis.text.x = element_text(angle = 90), legend.position = "top")
}
plot_metric(water_budgets, "vi", title = "Simulations: vertical inflow")
plot_metric(water_budgets, "gwr", title = "Simulations: groundwater recharge")
plot_metric(water_budgets, "aet", title = "Simulations: actual evapotranspiration")
plot_metric(water_budgets, "runoff", title = "Simulations: runoff")
plot_metric(water_budgets, "runoff_2", title = "Simulations: runoff (2)")

