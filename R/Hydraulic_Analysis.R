# ==============================================================================
# Script: Hydraulic Models - Coupling of Stream Metabolism and Calcium Carbonate Dynamics
# Description: Linear mixed-effects models for hydraulic variables + Diagnostics
# ==============================================================================

# ---- Load Libraries ----
library(lme4)
library(lmerTest)
library(ggplot2)
library(patchwork)  

# ---- Load Data ----
hydro_df <- read.csv("./data/HydraulicData.csv", sep = ",", header = TRUE)

# ---- Fit Models ----

## Water Velocity Model
vel_model <- lmer(log10(Water_velocity_ms) ~ Deposition + (1 | Stream), data = hydro_df)
summary(vel_model)

## Discharge Model
q_model <- lmer(log10(Discharge_Ls) ~ Deposition + (1 | Stream), data = hydro_df)
summary(q_model)

## Stream Depth Model
z_model <- lmer(log10(Stream_depth_m) ~ Deposition + (1 | Stream), data = hydro_df)
summary(z_model)

# ---- Save Model Summaries to File ----
sink("./results/hydraulic_model_summaries.txt")
cat("Water Velocity Model Summary\n")
print(summary(vel_model))
cat("\n\nDischarge Model Summary\n")
print(summary(q_model))
cat("\n\nStream Depth Model Summary\n")
print(summary(z_model))
sink()

# ---- Plot Model Diagnostics ----
model_list <- list(
  "Water Velocity" = vel_model,
  "Discharge" = q_model,
  "Stream Depth" = z_model
)

diagnostic_plots <- list()

for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  
  # Residuals vs Fitted Plot
  p <- ggplot(data.frame(Fitted = fitted(model), Residuals = resid(model)), 
              aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste(model_name, "- Residuals vs Fitted"),
         x = "Fitted Values",
         y = "Residuals")
  
  # Save individual plot
  plot_filename <- paste0("./figs/", gsub(" ", "_", tolower(model_name)), "_diagnostic_plot.png")
  ggsave(plot_filename, plot = p, width = 8, height = 6)
  
  # Store for combined plot
  diagnostic_plots[[model_name]] <- p
}

# ---- Combine & Display All Diagnostics Together ----
combined_plot <- diagnostic_plots[["Water Velocity"]] + 
  diagnostic_plots[["Discharge"]] + 
  diagnostic_plots[["Stream Depth"]] +
  plot_layout(ncol = 1)

print(combined_plot)

# ---- End of Script ----