# ==============================================================================
# Script: Calcium vs Specific Conductivity Calibration
# Description: Linear model and visualization of Ca²⁺ concentration vs specific conductivity
# ==============================================================================

# ---- Load Required Libraries ----
library(ggplot2)
library(dplyr)
library(cowplot)
library(openxlsx)

# ---- Load Data ----
Ca_SpC_df <- read.csv("./data/calcium_specific_conductivity_data.csv")

# ---- Fit Linear Model ----
Ca_SpC_model <- lm(calcium_concentration_mg_L ~ specific_conductivity_uS_cm, data = Ca_SpC_df)

# ---- Diagnostic Plots (Optional Interactive) ----
par(mfrow = c(2, 2))
plot(Ca_SpC_model)
par(mfrow = c(1, 1))

# ---- Check model results ----
summary(Ca_SpC_model)

# ---- Save Model Summary to Excel ----
model_summary <- as.data.frame(summary(Ca_SpC_model)$coefficients)
write.xlsx(model_summary, file = "./results/Ca_SpC_summary_LM.xlsx", rowNames = TRUE)

# ---- Plot Relationship with Equation Annotation ----
calibration_plot <- ggplot(Ca_SpC_df, aes(x = specific_conductivity_uS_cm, y = calcium_concentration_mg_L)) +
  geom_abline(slope = coef(Ca_SpC_model)[2], intercept = coef(Ca_SpC_model)[1], linetype = "solid", linewidth = 1) +
  geom_point(aes(colour = deposition, shape = stream, fill = deposition),
             size = 4, stroke = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c(6, 8,21,22,23,9,24)) +
  scale_fill_manual(values = c('#0072B2', '#CC79A7'), guide = "none") +  # Suppress the `fill` legend
  scale_color_manual(values = c('#0072B2', '#CC79A7'), name = "Deposition") +  # Keep the `colour` legend
  stat_regline_equation(aes(label = paste("y == -33.4 + .23 * x", "~~", "R^2 == 0.8")), parse = TRUE) +
  scale_y_continuous(name = expression(Ca^{"2+"} ~ (mg ~ L^{"-1"}))) +
  scale_x_continuous(
    name = expression(Specific ~ Conductivity ~ (mu ~ S ~ cm^{"-1"})),
    breaks = seq(from = 225, to = 474, by = 25)) +
  labs(shape = "Stream") +
  theme_cowplot(font_size = 20, line_size = .5) + 
  panel_border(color = "black", size = 1) +
  update_geom_defaults("text", list(size = 5))

calibration_plot
# ---- Save Final Plot ----
ggsave(plot = calibration_plot,
       filename = "./figs/FigureS1.tiff",
       width = 8,
       height = 5,
       dpi = 300)

# ---- End of the script ----
