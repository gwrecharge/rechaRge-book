#
# Usage example of a calibration calculation on a remote R server
# (server side)
#

# Build the objectives evaluation function
hydrobudget_eval_builder <- function(yearFrom = 1961, yearTo = 2017) {
  # Save master process lib paths
  libs <- .libPaths()
  return(function(i) {
    # Reinstate master process lib paths into forked process
    .libPaths(libs)

    # Output directory
    out_dir <- file.path(getwd(), "caramel")

    # Debug exec environment
    # fileConn<-file(file.path(out_dir, paste0("session", i, ".txt")))
    # writeLines(c(
    #   jsonlite::serializeJSON(sessionInfo(), pretty = T),
    #   jsonlite::toJSON(.libPaths(), pretty = T),
    #   jsonlite::toJSON(installed.packages()[,1], pretty = T)
    #              ), fileConn)
    # close(fileConn)

    # Input data
    input_rcn <- "rcn.csv.gz"
    input_climate <- "climate.csv.gz"
    input_rcn_climate <- "rcn_climate.csv.gz"
    input_rcn_gauging <- "rcn_gauging.csv.gz"
    input_observed_flow <- "observed_flow.csv.gz"
    input_alpha_lyne_hollick <- "alpha_lyne_hollick.csv.gz"
    # Simulation period
    simul_period <- c(yearFrom, yearTo)

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
    res1 <- result$simulation_metadata[1, ]
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

    return(c(output$KGE_qtot,
             output$KGE_qbase))
  })
}


# Calibration runs: triggers caRamel and save results
run_caramel <- function(yearFrom = 1961,
                        yearTo = 2017,
                        popsize = 50,
                        archsize = 100,
                        maxrun = 50,
                        carallel = 0,
                        numcores = 1) {
  # Prepare output directory
  out_dir <- file.path(getwd(), "caramel")
  if (file.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE)
  }
  dir.create(out_dir, recursive = TRUE)

  # Number of objectives
  nobj <- 2
  # Number of variables
  nvar <- 8
  # All the objectives are to be maximized
  minmax <- c(TRUE, TRUE)
  # Range of the parameters
  bounds <- matrix(nrow = nvar, ncol = 2)
  bounds[, 1] <- c(1, 4,-20, 5, 3.05, 0.5, 160, 0.01)
  bounds[, 2] <- c(2.5, 6.5,-12, 30, 4.8, 0.6, 720, 0.05)

  # Run simulations
  results_ <- caRamel::caRamel(
    nobj = nobj,
    nvar = nvar,
    minmax =  minmax,
    bounds = bounds,
    func = hydrobudget_eval_builder(yearFrom, yearTo),
    popsize = popsize,
    archsize = archsize,
    maxrun = maxrun,
    prec = matrix(0.01, nrow = 1, ncol = nobj),
    carallel = carallel,
    numcores = numcores
  )

  # Save caRamel results
  saveRDS(results_, file.path(out_dir, "results_caramel.rds"))

  # Plot using caRamel
  png(file.path(out_dir, "plot_caramel.png"),
      width = 1000,
      height = 500)
  caRamel::plot_caramel(results_, objnames = c("KGE_qtot", "KGE_qbase"))
  dev.off()

  # Plot all using ggplot
  library(ggplot2)
  # Front (as reported by caRamel)
  front <- data.table::data.table(results_$objectives)
  colnames(front) <- c("KGE_qtot", "KGE_qbase")
  # Simulations
  output <- data.table::fread(file.path(out_dir, "output.csv"))
  all <- output[, c("KGE_qtot", "KGE_qbase")]
  combined_data <- rbind(front, all)
  combined_data$group <-
    c(rep("All", nrow(all)), rep("Front", nrow(front)))
  ggplot(combined_data, aes(x = KGE_qtot, y = KGE_qbase, color = group)) +
    geom_point() +
    labs(
      title = "Scatter plot of simulations",
      x = "KGE_qtot",
      y = "KGE_qbase",
      color = "Dataset"
    ) +
    theme_minimal()
  ggsave(file.path(out_dir, "scatter.png"))

  # Merging simulation outputs with front objectives, to get corresponding parameters
  output_front <- data.table::data.table(cbind(results_$parameters, results_$objectives))
  colnames(output_front) <- c("T_m", "C_m", "TT_F", "F_T", "t_API", "f_runoff", "sw_m", "f_inf", "KGE_qtot", "KGE_qbase")
  data.table::fwrite(output_front, file.path(out_dir, "output_front.csv"))

  # zip results output folder
  files2zip <- dir("caramel", full.names = TRUE)
  zip(zipfile = "caramel.zip", files = files2zip)

  return(list(
    parameters = list(
      yearFrom = yearFrom,
      yearTo = yearTo,
      popsize = popsize,
      archsize = archsize,
      maxrun = maxrun,
      carallel = carallel,
      numcores = numcores
    ),
    results = results_,
    output_front = output_front,
    output_archive = "caramel.zip"
    ))
}

sessionInfo()
