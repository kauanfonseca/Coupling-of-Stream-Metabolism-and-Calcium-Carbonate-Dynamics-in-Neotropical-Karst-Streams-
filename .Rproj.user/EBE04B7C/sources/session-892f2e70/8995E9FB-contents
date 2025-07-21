# ==============================================================================
# Script: Linear Mixed-Effects Models – Discrete Sampling of Streams
# Description: 
#   Analyzes differences in stream physicochemical variables between deposition groups
#   using linear mixed-effects models. Outputs fixed effects to Word and Excel.
# ==============================================================================

# ---- Load Required Libraries ----
library(lmerTest)
library(dplyr)
library(officer)
library(flextable)
library(openxlsx)
library(performance)

# ---- Load Data ----
ProbeData <- read.csv("./data/ProbeData_DiscreteSampling.csv")

# ---- Define Model Function with check_model() ----
run_lmer_model <- function(response_var, df, log_transform = FALSE) {
  if (log_transform) {
    df[[response_var]] <- log10(df[[response_var]])  # Apply log10 transformation
  }
  
  formula <- as.formula(paste(response_var, "~ Deposition + (1 | Stream)"))
  model <- lmer(formula, data = df)
  
  # ---- Model diagnostics ----
  message("\nModel diagnostics for: ", response_var)
  print(check_model(model))  # <- this ensures the plots are shown!
  
  # ---- Extract fixed effects summary ----
  fixed_effects <- summary(model)$coefficients
  fixed_effects_df <- as.data.frame(fixed_effects[, c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")])
  fixed_effects_df$Effect_Type <- ifelse(grepl("(Intercept)", rownames(fixed_effects_df)), "Intercept", "Effect Size")
  fixed_effects_df$Variable <- response_var
  
  # Reorder and rename columns
  fixed_effects_df <- fixed_effects_df[, c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")]
  colnames(fixed_effects_df) <- c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "P")
  
  return(fixed_effects_df)
}

# ---- Run Models for Key Variables ----
fixed_effects_ph         <- run_lmer_model("pH", ProbeData)
fixed_effects_cond       <- run_lmer_model("Specific_Cond_uS_cm", ProbeData, log_transform = TRUE)
fixed_effects_temp       <- run_lmer_model("Temp_C", ProbeData, log_transform = TRUE)
fixed_effects_do_percent <- run_lmer_model("DissolvedOxygen_.", ProbeData, log_transform = TRUE)
fixed_effects_do_mgL     <- run_lmer_model("DissolvedOxygen_mgL", ProbeData, log_transform = TRUE)

# ---- Combine All Results into One Table ----
combined_results <- bind_rows(
  fixed_effects_ph,
  fixed_effects_cond,
  fixed_effects_temp,
  fixed_effects_do_percent,
  fixed_effects_do_mgL
)

# ---- Save as .xlsx File ----
write.xlsx(combined_results, file = "./results/LinearMixedEffect_ProbeData_output.xlsx", rowNames = FALSE)

# ---- View Formatted Table ----
flextable(combined_results)

# ---- End of Script ----