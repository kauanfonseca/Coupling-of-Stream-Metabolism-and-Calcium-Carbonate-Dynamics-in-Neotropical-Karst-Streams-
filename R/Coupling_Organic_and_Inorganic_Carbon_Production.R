#==============================================================================
# Script: Metabolism Data Preparation 
# Description: Prepares high-frequency data for stream metabolism analyses.
# ==============================================================================

# ---- Packages ----
library(ggplot2)
library(ggpubr)
library(cowplot)
library(scales)
library(lmerTest)
library(dplyr)
library(performance)
library(broom)
library(openxlsx)

# ---- Data Import and Preparation ----
df <- read.csv("./data/streamMetabolism_TravertineFormation_Data.csv")

# Factor conversion
df$stream <- as.factor(df$stream)

# Conversion function: grams of Ca to mol
grams_to_mol <- function(grams) {
  molar_mass_Ca <- 40.08  # g/mol
  grams / molar_mass_Ca
}

# ---- GPP effect on calcium flux (Deposition) ----

# Model: GPP vs CaCO3 flux (excluding SR 2023-10-09, row 1)
modelGPP_all_molar <- lm(
  log10(abs(daytime_caco3_flux_mol_m2_d)) ~ log10(gpp_mol_c_m2_d),
  data = df[-1, ]
)
check_model(modelGPP_all_molar)

# Plot: GPP vs CaCO3 flux
GPP_Ca <- ggplot(df[-2, ], aes(x = gpp_mol_c_m2_d, y = abs(daytime_caco3_flux_mol_m2_d))) +
  geom_errorbarh(aes(xmin = gpp_mol_c_m2_d - gpp_mol_c_m2_d_sd,
                     xmax = gpp_mol_c_m2_d + gpp_mol_c_m2_d_sd,
                     color = stream),
                 height = 0.04, alpha = 0.4, linewidth = 0.7) +
  geom_errorbar(aes(ymin = abs(daytime_caco3_flux_mol_m2_d) - daytime_caco3_flux_mol_m2_d_sd,
                    ymax = abs(daytime_caco3_flux_mol_m2_d) + daytime_caco3_flux_mol_m2_d_sd,
                    color = stream),
                width = 0.04, alpha = 0.4, linewidth = 0.7) +
  geom_abline(intercept = 0.8752, slope = 0.9580, linewidth = 1) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 3.5, stroke = 1.5, alpha = 0.7) +
  labs(title = "a", fill = "Stream", color = "Stream", shape = "Stream") +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  stat_regline_equation(
    label.y = log10(0.06), label.x = log10(0.015),
    aes(label = paste("y == 1.13 + 0.96 * x", "~~", "R^2 == 0.6")),
    parse = TRUE
  ) +
  scale_x_log10(name = expression(Gross~primary~production~(mol~C~m^{"-2"}~d^{"-1"}))) +
  scale_y_log10(name = expression("|"~Deposition~rates~"|"~(mol~CaCO[3]~m^{"-2"}~d^{"-1"}))) +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = c(.05, .8)) +
  panel_border(color = "black", size = 1) +
  update_geom_defaults("text", list(size = 5))

GPP_Ca

# ---- GPP effect on calcium accrual (Daytime Ca stock) ----

# Model: GPP vs Ca stock (excluding rows 1–3)
modelGPP_caa <- lm(
  log10(grams_to_mol(daytime_ca_standing_stock_g_m2)) ~ log10(gpp_mol_c_m2_d),
  data = df[-c(1:3), ]
)
check_model(modelGPP_caa)

# Plot: GPP vs Ca stock
GPP_Caa <- ggplot(df, aes(x = gpp_mol_c_m2_d, y = daytime_ca_standing_stock_g_m2)) +
  geom_errorbarh(aes(xmin = gpp_mol_c_m2_d - gpp_mol_c_m2_d_sd,
                     xmax = gpp_mol_c_m2_d + gpp_mol_c_m2_d_sd,
                     color = stream),
                 height = 1, alpha = 0.4, linewidth = 0.7) +
  geom_errorbar(aes(ymin = daytime_ca_standing_stock_g_m2 - daytime_ca_standing_stock_g_m2_sd,
                    ymax = daytime_ca_standing_stock_g_m2 + daytime_ca_standing_stock_g_m2_sd,
                    color = stream),
                width = 0.01, alpha = 0.4, linewidth = 0.7) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 3.5, stroke = 1.5, alpha = 0.7) +
  labs(title = "b") +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  scale_x_continuous(name = expression(Gross~primary~production~(mol~C~m^{"-2"}~d^{"-1"}))) +
  scale_y_continuous(name = expression(Stock~of~Calcium[~daytime]~(mol~Ca^{"2+"}~m^{"-2"}))) +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = "none") +
  panel_border(color = "black", size = 1) +
  update_geom_defaults("text", list(size = 5))

GPP_Caa


# ---- ER effect on calcium flux (Dissolution rates) ----

# Model: ER vs CaCO3 dissolution (excluding outliers: rows 9 and 16)
modelER_adj <- lm(
  log10(abs(nighttime_caco3_flux_mol_m2_d)) ~ log10(er_mol_c_m2_d),
  data = df[-c(9, 16), ]
)
check_model(modelER_adj)

# Plot: ER vs CaCO3 dissolution
ER_Ca <- ggplot(df[-c(9, 16), ], aes(x = abs(er_mol_c_m2_d), y = abs(nighttime_caco3_flux_mol_m2_d))) +
  geom_errorbarh(aes(xmin = abs(er_mol_c_m2_d) - er_mol_c_m2_d_sd,
                     xmax = abs(er_mol_c_m2_d) + er_mol_c_m2_d_sd,
                     color = stream),
                 height = 0.04, alpha = 0.4, linewidth = 0.7) +
  geom_errorbar(aes(ymin = abs(nighttime_caco3_flux_mol_m2_d) - nighttime_caco3_flux_mol_m2_d_sd,
                    ymax = abs(nighttime_caco3_flux_mol_m2_d) + nighttime_caco3_flux_mol_m2_d_sd,
                    color = stream),
                width = 0.03, alpha = 0.4, linewidth = 0.7) +
  geom_abline(slope = 0.7029, intercept = 0.12147, linewidth = 1) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 4, stroke = 1.5, alpha = 0.7) +
  labs(title = "c") +
  stat_regline_equation(
    aes(label = paste("y == 0.12 + 0.7 * x", "~~", "R^2 == 0.85")),
    parse = TRUE
  ) +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  scale_y_log10(name = expression(Dissolution~rates~(mol~CaCO[3]~m^{"-2"}~d^{"-1"}))) +
  scale_x_log10(name = expression("|"~Ecosystem~Respiration~"|"~(mol~C~m^{"-2"}~d^{"-1"}))) +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = "none") +
  panel_border(color = "black", size = 1)

ER_Ca

# ---- ER effect on calcium accrual (Nighttime Ca stock) ----

# Model: ER vs Ca standing stock (full data)
modelER_Caa <- lm(
  log10(grams_to_mol(abs(nighttime_ca_standing_stock_g_m2))) ~ log10(er_mol_c_m2_d),
  data = df
)
check_model(modelER_Caa)

# Plot: ER vs Ca stock
ER_Caa <- ggplot(df, aes(x = abs(er_mol_c_m2_d),
                         y = abs(grams_to_mol(nighttime_ca_standing_stock_g_m2)))) +
  geom_errorbarh(aes(xmin = abs(er_mol_c_m2_d) - er_mol_c_m2_d_sd,
                     xmax = abs(er_mol_c_m2_d) + er_mol_c_m2_d_sd,
                     color = stream),
                 height = 0.02, alpha = 0.4, linewidth = 0.7) +
  geom_errorbar(aes(ymin = abs(grams_to_mol(nighttime_ca_standing_stock_g_m2)) - grams_to_mol(nighttime_ca_standing_stock_g_m2_sd),
                    ymax = abs(grams_to_mol(nighttime_ca_standing_stock_g_m2)) + grams_to_mol(nighttime_ca_standing_stock_g_m2_sd),
                    color = stream),
                width = 0.03, alpha = 0.4, linewidth = 0.7) +
  geom_abline(slope = 0.5, intercept = 0.42, linewidth = 1) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 4, stroke = 1.5, alpha = 0.7) +
  labs(title = "d") +
  stat_regline_equation(
    aes(label = paste("y == 0.42 + 0.5 * x", "~~", "R^2 == 0.83")),
    parse = TRUE
  ) +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  scale_y_log10(name = expression(Stock~of~Calcium[~nighttime]~(mol~Ca^{"2+"}~m^{"-2"}))) +
  scale_x_log10(name = expression("|"~Ecosystem~Respiration~"|"~(mol~C~m^{"-2"}~d^{"-1"}))) +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = "none") +
  panel_border(color = "black", size = 1)

ER_Caa

# ---- NEP effect on net travertine formation ----

# Model: NEP vs Net CaCO3 formation (asinh transformation)
nep_model <- lm(asinh(net_caco3_mol_m2_d) ~ asinh(nep_mol_c_m2_d), data = df)
summary(nep_model)
check_model(molar_model)

# Plot: NEP vs Net travertine formation
NEP_NET <- ggplot(df, aes(x = nep_mol_c_m2_d, y = net_caco3_mol_m2_d)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 4, stroke = 1.5, alpha = 0.7) +
  labs(title = "e") +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  scale_y_continuous(name = expression(Net~travertine~formation~(mol~CaCO[3]~m^{"-2"}~d^{"-1"}))) +
  scale_x_continuous(name = expression(Net~ecosystem~production~(mol~C~m^{"-2"}~d^{"-1"}))) +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = "none") +
  panel_border(color = "black", size = 1)

NEP_NET

# ---- NEP effect on mean calcium accrual (in mmol) ----

# Model: NEP vs mean Ca stock (removing RA 2023-09-01 and SR 2023-10-09)
MeanCaa_model <- lm(
  asinh(grams_to_mol(mean_standing_stock_g_m2) * 1000) ~ asinh(nep_mol_c_m2_d * 1000),
  data = df[-c(4, 17), ]) 

summary(MeanCaa_model)
check_model(MeanCaa_model)

# Plot: NEP vs mean calcium stock (mmol scale)
NEP_NET_Caa <- ggplot(df[-c(4, 17), ], aes(x = nep_mol_c_m2_d * 1000,
                                           y = grams_to_mol(mean_standing_stock_g_m2) * 1000)) +
  geom_abline(slope = -0.50877, intercept = 4.10596, linewidth = 1) +
  geom_point(aes(colour = stream, shape = stream, fill = stream),
             size = 4, stroke = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c(21:24)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
  scale_x_continuous(
    trans = asinh_trans(),
    limits = c(-1500, -10),
    breaks = c(-1500, -500, -250, -100, -10),
    name = expression(Net~ecosystem~production~(mmol~C~m^{"-2"}~day^{"-1"}))
  ) +
  scale_y_continuous(
    trans = asinh_trans(),
    limits = c(100, 1500),
    breaks = c(100, 250, 550, 1000, 1500),
    name = expression(Mean~calcium~accrual~(mmol~Ca^{"2+"}~m^{"-2"}))
  ) +
  stat_regline_equation(
    label.y = 6, label.x = -8,
    aes(label = paste("y == 4.1 -0.5 * x", "~~", "R^2 == 0.83")),
    parse = TRUE
  ) +
  labs(title = "f") +
  theme_cowplot(font_size = 16, line_size = 0.5) +
  theme(legend.position = "none") +
  panel_border(color = "black", size = 1)

NEP_NET_Caa

# ---- Combine all figures into a single panel ----

# Requires: GPP_Ca, GPP_Caa, ER_Ca, ER_Caa, NEP_NET, NEP_NET_Caa
library(gridExtra)

combined_plot <- grid.arrange(
  GPP_Ca, GPP_Caa,
  ER_Ca, ER_Caa,
  NEP_NET, NEP_NET_Caa,
  ncol = 3
)

# Save final figure
ggsave(
  plot = combined_plot,
  filename = "./figs/Figure4.tiff",
  width = 16,
  height = 10,
  dpi = 300
)


# ---- Function to extract model summaries ----
extract_model_summary <- function(model, model_name, predictor_transformation) {
  coef_df <- broom::tidy(model)
  model_stats <- broom::glance(model)
  
  coef_df$p.value <- ifelse(
    coef_df$p.value < 0.05, 
    "p < 0.05", 
    format(round(coef_df$p.value, 4), scientific = FALSE)
  )
  
  intercept <- coef_df %>% filter(term == "(Intercept)")
  slope <- coef_df %>% filter(term != "(Intercept)")
  predictor <- all.vars(formula(model))[2]
  
  data.frame(
    Model = model_name,
    Predictor = predictor,
    Transformation = predictor_transformation,
    
    Intercept = round(intercept$estimate, 4),
    `Intercept Std. Error` = round(intercept$std.error, 4),
    `Intercept t value` = round(intercept$statistic, 4),
    `Intercept p value` = intercept$p.value,
    
    Slope = round(slope$estimate, 4),
    `Slope Std. Error` = round(slope$std.error, 4),
    `Slope t value` = round(slope$statistic, 4),
    `Slope p value` = slope$p.value,
    
    R2 = round(model_stats$r.squared, 2),
    `F-statistic` = round(model_stats$statistic, 4),
    df = model_stats$df.residual,
    Residuals = round(model_stats$sigma, 4)
  )
}

# ---- Summarize all models and export ----

models <- list(
  modelGPP_all_molar,
  modelGPP_caa,
  modelER_adj,
  modelER_Caa,
  molar_model,
  MeanCaa_model
)

model_names <- c(
  "Deposition rates",
  "Daytime calcium accrual",
  "Dissolution rates",
  "Nighttime calcium accrual",
  "Net travertine formation",
  "Mean calcium accrual"
)

transformations <- c(
  "log10(GPP)",
  "log10(GPP)",
  "log10(ER)",
  "log10(ER)",
  "asinh(NEP)",
  "asinh(NEP)"
)

# Create summary table
summary_table <- do.call(
  rbind,
  mapply(extract_model_summary, models, model_names, transformations, SIMPLIFY = FALSE)
)

# Print to console
print(summary_table)

# Export to Excel
openxlsx::write.xlsx(
  summary_table,
  file = "./results/StreamMetabolism_Influence_On_Travertine_LMs_summaries.xlsx",
  sheetName = "All Models",
  rowNames = FALSE
)
