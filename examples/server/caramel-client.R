#
# Usage example of a calibration calculation on a remote R server
# (client side)
#
library(rockr)

# Make a connection object
url <- 'http://localhost:8085'
conn <- rockr.connect('user', 'password', url = url)

# Open a remote R session
rockr.open(conn)

# Input files upload
for (input_file in dir("../input")) {
  rockr.file_upload(conn, source = paste0("../input/", input_file), destination = input_file, overwrite = TRUE)
}
# Prepare R code to be executed in remote R session
info <- rockr.eval.source(conn, "caramel-server.R")

# Launch calibration calculations as an async R command
cmd <- rockr.eval(conn, quote(run_caramel(yearFrom = 2016, popsize = 50, maxrun = 50, carallel = 1, numcores = 2)), async =  TRUE)

# Check periodically for the remote command to complete
library(progress)
pb <- progress_bar$new(
  format = "Calculating (:spin) :current mins",
  clear = FALSE, total = NA, width = 60)
finished <- FALSE
while(!finished) {
  pb$tick()
  status <- rockr.command(conn, cmd$id)
  finished <- status$finished
  Sys.sleep(60) # seconds
}
pb$terminate()
status <- rockr.command(conn, cmd$id)
status

# Get results object
res <- rockr.command_result(conn, cmd$id)
res
if (!is.null(res) && res$results$success) {
  results_ <- res$results

  # Merging simulation outputs with front objectives, to get corresponding parameters
  output_front <- data.table::data.table(cbind(results_$parameters, results_$objectives))
  colnames(output_front) <- c("T_m", "C_m", "TT_F", "F_T", "t_API", "f_runoff", "sw_m", "f_inf", "KGE_qtot", "KGE_qbase")

  # Plot using caRamel
  caRamel::plot_caramel(results_, objnames = c("KGE_qtot", "KGE_qbase"))

  # Plot all using ggplot
  library(ggplot2)
  front <- data.table(results_$objectives)
  colnames(front) <- c("KGE_qtot", "KGE_qbase")
  all <- data.table(results_$total_pop[, 9:10])
  colnames(all) <- c("KGE_qtot", "KGE_qbase")
  combined_data <- rbind(all, front)
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
}

# Terminate the remote R session
rockr.close(conn)

# Note: possible improvement is to make several calibration sessions
# and then optimize results by combining them.
# https://cran.r-project.org/web/packages/caRamel/vignettes/MultiPareto.html
