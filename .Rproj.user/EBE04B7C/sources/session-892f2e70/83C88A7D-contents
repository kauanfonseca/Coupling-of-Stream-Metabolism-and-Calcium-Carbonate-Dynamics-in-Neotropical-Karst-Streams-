# ==============================================================================
# Script: Metabolism Data Preparation and Model Execution
# Coupling of Stream Metabolism and Calcium Carbonate Dynamics
# Description: 
#   1. Creates formatted input data per stream
#   2. Runs metabolism models and saves outputs in stream-specific folders
# ==============================================================================

# ---- Load Required Libraries ----
library(dplyr)
library(lubridate)
library(R2jags)
library(BASEmetab)
library(ggplot2)

# ---- Load High-Frequency Data ----
HighFrequencyData <- read.csv("./data/HighFrequencyData.csv")

# ---- Parse Datetime Column ----
HighFrequencyData$sampling_datetime <- as.POSIXct(
  HighFrequencyData$sampling_datetime, format = "%Y%m%d %H:%M:%S"
)

# ---- Set Output Paths ----
output_data_dir <- file.path(getwd(), "incremental", "Metabolism_Modelling", "BASEmetab_input")
results_base_dir <- file.path(getwd(), "results", "BASEmetab_output")

# Create folders if they don’t exist
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_base_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Step 1: Format and Save Input Data Files Per Stream ----
streams <- HighFrequencyData %>%
  distinct(stream_name, stream_abreviation)

message("Creating BASE_input files per stream...")

for (i in 1:nrow(streams)) {
  
  abbrev <- streams$stream_abreviation[i]
  
  stream_data <- HighFrequencyData %>%
    filter(stream_name == streams$stream_name[i]) %>%
    mutate(
      Date = as.Date(sampling_datetime),
      Time = format(sampling_datetime, "%H:%M:%S"),
      I = par_umol_m2_s,
      tempC = temp_c,
      DO.meas = dissolved_oxygen_mgL,
      atmo.pressure = 1,
      salinity = 0
    ) %>%
    select(Date, Time, I, tempC, DO.meas, atmo.pressure, salinity)
  
  # ---- Remove rows with NA values (only for RT and CC) ----
  if (abbrev %in% c("RT", "CC")) {
    stream_data <- stream_data %>% filter(complete.cases(.))
  }
  
  # ---- Save Prepared Data File ----
  file_name <- paste0("BASE_input_", abbrev, ".csv")
  write.csv(stream_data, file.path(output_data_dir, file_name), row.names = FALSE)
  
  message("Saved input file for: ", abbrev)
}

# ---- Step 2: Run BASEmetab Models and Save Per Stream ----
input_files <- list.files(output_data_dir, pattern = "^BASE_input_.*\\.csv$", full.names = TRUE)

message("\n Running BASEmetab models for each stream. This might take a bit of time...\n")

for (file_path in input_files) {
  
  stream_label <- gsub("BASE_input_", "", tools::file_path_sans_ext(basename(file_path)))  # e.g., "CD"
  
  # Create temp input folder with required 'BASE_input.csv' filename
  temp_input_dir <- file.path(tempdir(), paste0("metab_run_", stream_label))
  dir.create(temp_input_dir, recursive = TRUE, showWarnings = FALSE)
  
  file.copy(from = file_path,
            to = file.path(temp_input_dir, "BASE_input.csv"),
            overwrite = TRUE)
  
  # Set stream-specific output directory
  stream_output_dir <- file.path(results_base_dir, stream_label)
  dir.create(stream_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Run metabolism model
  message("Running model for stream: ", stream_label)
  tryCatch({
    bayesmetab(data.dir = temp_input_dir, results.dir = stream_output_dir, interval = 600)
  }, error = function(e) {
    message("Model failed for stream ", stream_label, ": ", e$message)
  })
}

message("\nAll model runs completed and saved to individual stream folders.")

# ---- End of Script ----