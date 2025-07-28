# ==============================================================================
# Script: Geochemical Modeling – Saturation Index, pCO₂, and Mass Transfer
# Description: Linear mixed-effects models to assess the effect of deposition
#              on calcite saturation, CO₂, and calcium precipitation.
#              Includes diagnostics via check_model().
# ==============================================================================

# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(lmerTest)
library(performance)
library(cowplot)

# ---- Load Data ----
Geochemical_df <- read.csv("./data/GeochemicalData.csv")
CaMass_df      <- read.csv("./data/CalciumMassTransferReactions.csv")

# ---- Define Model Runner Function with Diagnostics ----
run_lmer_model <- function(response_var, df, asinh_transform = TRUE, log_transform = FALSE) {
  if (asinh_transform) {
    df[[response_var]] <- asinh(df[[response_var]])
  } else if (log_transform) {
    df[[response_var]] <- log(df[[response_var]])
  }
  
  formula <- as.formula(paste(response_var, "~ deposition + (1 | stream)"))
  model <- lmer(formula, data = df)
  
  # Diagnostics
  message("\nModel diagnostics for: ", response_var)
  print(check_model(model))
  
  # Summary
  print(summary(model))
  return(model)
}

# ==============================================================================
# Model 1: Calcite Saturation Index
# ==============================================================================

SIC_model <- run_lmer_model("si_calcite", Geochemical_df, asinh_transform = FALSE)

# ---- Plot SI[Calcite] ----
SIC_plot <- ggplot(Geochemical_df, aes(x = deposition, y = si_calcite,
                                       colour = deposition, fill = deposition)) +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 1) +
  geom_hline(yintercept = 0.8, linetype = "longdash", linewidth = 1, color = "#3998DB") +
  geom_boxplot(width = 0.35, linewidth = 1, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(size = 3, alpha = 0.5, position = position_dodge2(0.1)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 18,
               position = position_dodge(width = 0.45)) +
  scale_color_manual(values = c('#0072B2','#CC79A7'), aesthetics = c("colour", "fill")) +
  labs(title = "a", x = "Deposition", y = expression(SI[Calcite])) +
  cowplot::theme_cowplot(font_size = 20, line_size = 0.5) +
  theme(legend.position = "none") +
  cowplot::panel_border(color = "black", size = 1) +
  annotate('text', x = 1.5, y = 1.2, label = "*", size = 12)

print(SIC_plot)

# ==============================================================================
# Model 2: pCO₂
# ==============================================================================

pCO2_model <- run_lmer_model("pco2", Geochemical_df, asinh_transform = FALSE)

# ---- Plot pCO₂ ----
pCO2_plot <- ggplot(Geochemical_df, aes(x = deposition, y = (10^pco2)*10^6,
                                        colour = deposition, fill = deposition)) +
  geom_hline(yintercept = 0, linetype = "longdash", lwd = 1) +
  geom_boxplot(width = .35, lwd = 1, outlier.shape = NA, alpha =.7) +
  geom_jitter(aes(color = deposition, stroke = 1), alpha = .5, 
              size =3, position = position_dodge2(.1)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3,
               shape = 18, position = position_dodge(width = 0.45)) + 
  scale_color_manual(values = c('#0072B2','#CC79A7'),
                     aesthetics = c("colour", "fill"),
                     name = "Deposition") +
  labs(title = "b",
       x = "Deposition",
       y = expression(pCO[2])) +
  theme_cowplot(font_size = 20, line_size = .5) +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size = 14)) + 
  cowplot::panel_border(color = "black", size = 1) +
  update_geom_defaults("text", list(size = 5)) +
  annotate('text', x = 1.5, y = 22500, label = "*", size = 12, color = "black")  
  


print(pCO2_plot)

# ==============================================================================
# Model 3: Ca²⁺ Precipitation Rate (Mass Transfer)
# ==============================================================================

MT_model <- run_lmer_model("precipitation_rate_mgCa_m2_s", CaMass_df)

# ---- Plot Ca²⁺ Mass Transfer ----
MT_plot <- ggplot(CaMass_df, aes(x = deposition, y = precipitation_rate_mgCa_m2_s,
                                 colour = deposition, fill = deposition)) +
  geom_hline(yintercept = 0, linetype = "longdash", lwd = 1) +
  geom_boxplot(width = .35, lwd = 1, outlier.shape = NA, alpha =.7) +
  geom_jitter(aes(color = deposition, stroke = 1.3), alpha = .5, size =3,
              position = position_dodge2(.1)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3,
               shape = 18, position = position_dodge(width = 0.45)) + 
  scale_color_manual(values = c('#0072B2','#CC79A7'), aesthetics = c("colour", "fill")) +
  labs(title = "c", x = "Deposition") +
  scale_y_continuous(name=expression(Ca^{"2+"}~mass~transfer~reaction~(mg~m^{"-2"}~s^{"-1"}))) +
  cowplot::theme_cowplot(font_size = 20, line_size = .5) +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(size = 14)) + 
  cowplot::panel_border(color = "black", size = 1) +
  update_geom_defaults("text", list(size = 5)) +
  annotate('text', x = 1.5, y = 310, label = "*", size = 12, color = "black")  

print(MT_plot)

# ---- Combine All Figures Side by Side with Labels ----
library(patchwork)

combined_figure <- SIC_plot + pCO2_plot + MT_plot +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "none") 

# ---- Save Combined Figure ----
ggsave("figs/Figure3.tiff", plot = combined_figure, width = 18, height = 6, dpi = 300)
# ---- End of Script ----
