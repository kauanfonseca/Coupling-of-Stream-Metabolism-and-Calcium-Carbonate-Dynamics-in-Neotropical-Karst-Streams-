# ==============================================================================
# Script: Coupling_Alkalinity_Calcium
# Description: Tests the linear relationship between calcium concentration 
#              and alkalinity (as CaCO₃ equivalents) in stream water.
# ==============================================================================

# ---- Load Required Libraries ----
library(car)  # For diagnostics if needed later

# ---- Load Data ----
df <- read.csv("./data/Alkalinity_Calcium_Coupling.csv", sep = ",", header = TRUE)

# ---- Fit Linear Model: Alkalinity (CaCO₃) ~ Calcium ----
model <- lm(alkalinity_calcium.carbonate_mol_L ~ calcium_mol_L + 0, data = df)

# ---- Model Summary ----
summary(model)

# ---- Extract Fixed Effects ----
model_df <- as.data.frame(summary(model)$coefficients)
model_df$R2 <- 0.9732  # Add R² manually from summary output

# ---- Export Results ----
write.csv(model_df, "./results/Coupling_Alkalinity_Calcium_summaries.csv", row.names = FALSE)

# ---- View Results ----
print(model_df)
