library(rockr)

# Make a connection object
url <- 'http://localhost:8085'
conn <- rockr.connect('user', 'password', url = url)

# Get the status of the R server (admin only)
rockr.status(conn)

# Restart the remote R server (admin only)
rockr.start(conn)

# Open an R session
rockr.open(conn)

# File upload and download
for (input_file in dir("../input")) {
  rockr.file_upload(conn, source = paste0("../input/", input_file), destination = input_file, overwrite = TRUE)
}
cmd <- rockr.eval.source(conn, "caramel-server.R", async = TRUE)

library(progress)
pb <- progress_bar$new(
  format = "(:spin) :current at :tick_rate/sec",
  clear = FALSE, total = NA, width = 60)
finished <- FALSE
while(!finished) {
  pb$tick()
  Sys.sleep(10)
  status <- rockr.command(conn, cmd$id)
  finished <- status$finished
}
pb$terminate()

# Download results folder into working directory
rockr.assign(conn, "files2zip", quote(dir("caramel", full.names = TRUE)))
rockr.eval(conn, quote(zip(zipfile = "caramel.zip", files = files2zip)))
rockr.file_download(conn, source = "caramel.zip")

# Extract results
unzip('caramel.zip')
results <- readRDS("./caramel/results_caramel.rds")
data.table::fread("./caramel/output_front.csv")
utils::browseURL("./caramel/scatter.png")
utils::browseURL("./caramel/plot_caramel.png")

# Terminate the remote R session
rockr.close(conn)
