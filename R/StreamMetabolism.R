# ==============================================================================
# Script: Metabolism Data Preparation - Coupling of Stream Metabolism and Calcium Carbonate Dynamics
# Description: Prepares high-frequency data for stream metabolism analyses.
#              Converts dataset into required format for metabolic modeling.
# ==============================================================================

# ---- Load Required Libraries ----
library(dplyr)
library(lubridate)
library(R2jags)
library(BASEmetab)
library(lubridate)


# ---- Load High-Frequency Data ----
HighFrequencyData <- read.csv("./data/HighFrequencyData.csv")  # Adjust filename as needed

# ---- Prepare Data ----

# Parse datetime column 
HighFrequencyData$sampling_datetime <- ymd_hms(HighFrequencyData$sampling_datetime)

# Extract Date and Time from sampling_datetime
BASE_input <- HighFrequencyData %>%
  group_by(stream_name, stream_abreviation) %>% 
  mutate(
    Date = as.Date(sampling_datetime),                       # Extract date
    Time = format(sampling_datetime, "%H:%M:%S"),            # Extract time
    I = par_umol_m2_s,                                       # PAR
    tempC = temp_c,                                          # Temp
    DO.meas = dissolved_oxygen_mgL,                          # Dissolved O2
    atmo.pressure = 1,                                       # Atmospheric Pressure
    salinity = 0                                             # Salinity
  ) %>%
  select(Date, Time, I, tempC, DO.meas, atmo.pressure, salinity)

# ---- Check Data Preview ----
head(BASE_input)

# ---- Save Final Prepared Data ----
write.csv(BASE_input, "./data/BASE_input.csv", row.names = FALSE)

data.dir <- "./data/BASE_input" 

#set output directory to Output folder in current working directory.
results.dir <- file.path(getwd(), "./results/BASEmeta_output")

if (dir.exists(results.dir)){} else {
  dir.create(results.dir)}

#run model.
results <- bayesmetab(data.dir, results.dir, interval = 600)


# ---- End of Script ----