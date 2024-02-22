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

# File upload and download
for (input_file in dir("../input")) {
  rockr.file_upload(conn, source = paste0("../input/", input_file), destination = input_file, overwrite = TRUE)
}
rockr.eval.source(conn, "caramel-server.R")

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

# Download results folder into working directory
res <- rockr.command_result(conn, cmd$id)
res
if (!is.null(res) && res$results$success) {
  # Download results archive
  rockr.file_download(conn, source = res$output_archive, overwrite = TRUE)

  # Extract results
  unzip(res$output_archive, overwrite = TRUE)
  results <- readRDS("./caramel/results_caramel.rds")
  data.table::fread("./caramel/output_front.csv")
  utils::browseURL("./caramel/scatter.png")
  utils::browseURL("./caramel/plot_caramel.png")
}

# Terminate the remote R session
rockr.close(conn)
