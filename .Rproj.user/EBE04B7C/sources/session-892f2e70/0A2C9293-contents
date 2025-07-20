# ==============================================================================
# Script: Hydraulic variables 
# Description: Linear mixed-effects models for hydraulic variables + diagnostics
# ==============================================================================

# ---- Load Libraries ----
library(lme4)
library(lmerTest)
library(performance)

# ---- Load Data ----
hydro_df <- read.csv("./data/HydraulicData.csv")

# ---- Fit Linear Mixed-Effects Models ----
vel_model <- lmer(log10(Water_velocity_ms) ~ Deposition + (1 | Stream), data = hydro_df)
q_model   <- lmer(log10(Discharge_Ls)       ~ Deposition + (1 | Stream), data = hydro_df)
z_model   <- lmer(log10(Stream_depth_m)     ~ Deposition + (1 | Stream), data = hydro_df)

# ---- Models Diagnostics  ----
message("\nRunning check_model diagnostics for all models...\n")

message("Water Velocity Model:")
check_model(vel_model)

message("Discharge Model:")
check_model(q_model)

message("Stream Depth Model:")
check_model(z_model)

# ---- Save Model Summaries to File ----
sink("./results/hydraulic_model_summaries.txt")
cat("=== Water Velocity Model ===\n\n")
print(summary(vel_model))

cat("\n\n=== Discharge Model ===\n\n")
print(summary(q_model))

cat("\n\n=== Stream Depth Model ===\n\n")
print(summary(z_model))
sink()

# ---- End of Script ----