
load_aimee_data <- function(package) {
  data_dir <- system.file("data", package = package)
  rda_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)
  for (file in rda_files) {
    load(file, envir = .GlobalEnv)
  }
}
