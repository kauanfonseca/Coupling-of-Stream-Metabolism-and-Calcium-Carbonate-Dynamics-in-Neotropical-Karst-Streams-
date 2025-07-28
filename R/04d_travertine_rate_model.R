# ==============================================================================
# Script: Calculating travertine formation rates.R
# Description: Fits calcium stock change (g/m²) over time (hours),
#              grouped by stream, daytime, and sampling date.
# ==============================================================================

# ---- Load Required Libraries ----
library(dplyr) 
library(writexl)
# ---- Load Data ----
df <- read.csv("./data/Ca_Stock_Variation_LinearSegments.csv", sep = ",", header = TRUE)

#---- Linear Regression Function ----
create_linear_models <- function(df) {
  model_results <- data.frame()
  combos <- with(df, unique(interaction(day_period, date, stream, drop = TRUE)))
  for (combo in combos) {
    subset_df <- subset(df, interaction(day_period, date, stream, drop = TRUE) == combo)
    if (length(unique(subset_df$diff_h)) > 1) {
      lm_model <- lm(calcium_g_m2 ~ diff_h, data = subset_df)
      model_summary <- summary(lm_model)
      coefficients <- coef(lm_model)
      std_errors <- coef(model_summary)[, "Std. Error"]
      p_value <- coef(model_summary)[, "Pr(>|t|)"][2]
      model_result <- data.frame(
        day_period = unique(subset_df$day_period),
        date = unique(subset_df$date),
        stream = unique(subset_df$stream),
        slope = coefficients[2],
        intercept = coefficients[1],
        slope_std_error = std_errors[2],
        intercept_std_error = std_errors[1],
        r_squared = model_summary$r.squared,
        f_statistic = model_summary$fstatistic[1],
        p_value = p_value,
        residual_std_error = model_summary$sigma,
        adjusted_r_squared = model_summary$adj.r.squared
      )
      model_results <- bind_rows(model_results, model_result)
    }
  }
  return(model_results)
}

travertine_formation_rates.df <-create_linear_models(df)

View(travertine_formation_rates.df)

# ---- Save Results ----
write.xlsx(travertine_formation_rates.df, "./results/TableS3.xlsx")
