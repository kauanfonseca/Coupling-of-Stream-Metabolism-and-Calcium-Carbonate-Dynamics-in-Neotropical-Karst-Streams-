# ==============================================================================
# Script: Alkalinity_Model_Analysis.R
# Description: Tests the effect of deposition regime on dissolved inorganic 
#              carbon (DIC) using linear mixed-effects models.
# ==============================================================================

# ---- Load Packages ----
library(lme4)         
library(lmerTest)    
library(performance)  
library(car)          

# ---- Load and Clean Data ----
Alk_df <- read.csv("./data/alkalinity.csv", sep = ",", header = TRUE)

# Remove known outlier (RA, 2023-08-04)
Alk_clean <- Alk_df[-4, ]

# ---- Fit Linear Mixed-Effects Model ----
DIC_model <- lmer(dissolved_inorganic_carbon_mmol_L ~ deposition + (1 | stream), data = Alk_clean)

# ---- Check Model Assumptions ----
check_model(DIC_model)  # Visual check: residuals vs fitted should show no pattern

# ---- Levene’s Test for Homogeneity of Variance ----
resid_df <- data.frame(
  residuals = resid(DIC_model),
  deposition = Alk_clean$deposition
)

leveneTest(residuals ~ deposition, data = resid_df)

# ---- Summarize Model ----
summary(DIC_model)

# ---- Model Summary Function ----
run_lmer_model <- function(response_var, df, log_transform = FALSE) {
  if (log_transform) {
    df[[response_var]] <- log10(df[[response_var]])
  }
  
  formula <- as.formula(paste(response_var, "~ deposition + (1 | stream)"))
  model <- lmer(formula, data = df)
  
  # Extract fixed effects
  fixed <- summary(model)$coefficients
  fixed_df <- as.data.frame(fixed[, c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")])
  
  # Add metadata columns
  fixed_df$Effect_Type <- ifelse(rownames(fixed_df) == "(Intercept)", "Intercept", "Effect Size")
  fixed_df$Variable <- response_var
  
  # Rename and reorder columns
  colnames(fixed_df) <- c("Estimate", "Std. Error", "df", "t value", "P", "Effect_Type", "Variable")
  fixed_df <- fixed_df[, c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "P")]
  
  return(fixed_df)
}

# ---- Run Summary for DIC Model ----
fixed_effects_DIC <- run_lmer_model("dissolved_inorganic_carbon_mmol_L", Alk_clean)
print(fixed_effects_DIC)
