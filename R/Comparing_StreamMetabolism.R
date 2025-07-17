# ===============================
# Stream Metabolism Mixed-Effects Models
# Author: Kauan Fonseca
# Description: Fits LMMs to compare GPP, ER, NEP, and P:R between deposition types,
#              applies appropriate transformations, and includes model diagnostics
# ===============================

# ---- Load Required Packages ----
library(dplyr)
library(ggplot2)
library(lmerTest)
library(performance)
library(openxlsx)

# ---- Load and Clean Data ----
df <- read.csv("./data/metabolism_data.csv")

# Remove RT stream (rows 23:25) due to biased estimates
df_filtered <- df[-c(23:25), ]

# ---- Modeling Functions ----

# For log10-transformed response (e.g., GPP, ER, P:R)
run_lmer_model <- function(response_var, df, log_transform = FALSE) {
  if (log_transform) {
    df[[response_var]] <- log10(df[[response_var]])
  }
  
  df <- df %>% filter(!is.na(.data[[response_var]]))
  formula <- as.formula(paste(response_var, "~ deposition + (1 | stream)"))
  model <- lmer(formula, data = df)
  
  # Model diagnostics
  message("\nModel diagnostics for: ", response_var)
  print(performance::check_model(model))  # <- Plots in Viewer
  
  # Extract fixed effects
  fixed_effects <- summary(model)$coefficients
  fixed_effects_df <- as.data.frame(fixed_effects[, c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")])
  fixed_effects_df$Effect_Type <- ifelse(grepl("(Intercept)", rownames(fixed_effects_df)), "Intercept", "Effect Size")
  fixed_effects_df$Variable <- response_var
  
  fixed_effects_df <- fixed_effects_df[, c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")]
  colnames(fixed_effects_df) <- c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "P")
  
  return(fixed_effects_df)
}

# For asinh-transformed response (e.g., NEP)
run_lmer_model_asinh <- function(response_var, df) {
  df[[response_var]] <- asinh(df[[response_var]])
  df <- df %>% filter(!is.na(.data[[response_var]]))
  
  formula <- as.formula(paste(response_var, "~ deposition + (1 | stream)"))
  model <- lmer(formula, data = df)
  
  # Model diagnostics
  message("\nModel diagnostics for: ", response_var)
  print(performance::check_model(model))  # <- Plots in Viewer
  
  # Extract fixed effects
  fixed_effects <- summary(model)$coefficients
  fixed_effects_df <- as.data.frame(fixed_effects[, c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")])
  fixed_effects_df$Effect_Type <- ifelse(grepl("(Intercept)", rownames(fixed_effects_df)), "Intercept", "Effect Size")
  fixed_effects_df$Variable <- response_var
  
  fixed_effects_df <- fixed_effects_df[, c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")]
  colnames(fixed_effects_df) <- c("Variable", "Effect_Type", "Estimate", "Std. Error", "df", "t value", "P")
  
  return(fixed_effects_df)
}

# ---- Run Models ----

# GPP: log10 transform
fixed_effects_GPP <- run_lmer_model("gpp_g_m2_d", df_filtered, log_transform = TRUE)

# ER: log10 transform
fixed_effects_ER <- run_lmer_model("er_g_m2_d", df_filtered, log_transform = TRUE)

# NEP: asinh transform
fixed_effects_NEP <- run_lmer_model_asinh("nep_g_m2_d", df_filtered)

# P:R ratio: log10 transform (optional)
fixed_effects_PR <- run_lmer_model("pr_mean", df_filtered, log_transform = TRUE)

# ---- Combine and Export Results ----
combined_results <- bind_rows(fixed_effects_GPP, fixed_effects_ER, fixed_effects_NEP, fixed_effects_PR)

write.xlsx(combined_results,
           file = "./results/StreamMetabolism_LinearMixedEffect_output.xlsx",
           rowNames = FALSE)

message("✅ Fixed effects exported to: ./results/StreamMetabolism_LinearMixedEffect_output.xlsx")

# ---- Plotting Functions ----
plot_metab_box <- function(data, y_var, y_lab, title_label, y_scale = "log10", limits = NULL, breaks = waiver()) {
  ggplot(data, aes(x = deposition, y = .data[[y_var]], color = deposition, fill = deposition)) +
    geom_boxplot(width = 0.35, linewidth = 1, outlier.shape = NA, alpha = 0.7) +
    geom_jitter(aes(stroke = 1.3), alpha = 0.5, size = 3, position = position_dodge2(0.1)) +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 18,
                 position = position_dodge(width = 0.45)) +
    scale_color_manual(values = c("Active" = "#0072B2", "Non-active" = "#CC79A7")) +
    scale_fill_manual(values = c("Active" = "#0072B2", "Non-active" = "#CC79A7")) +
    {if (y_scale == "log10") scale_y_log10(name = y_lab) else scale_y_continuous(name = y_lab, trans = y_scale, limits = limits, breaks = breaks)} +
    labs(title = title_label, x = "Deposition") +
    theme_cowplot(font_size = 20, line_size = 0.5) +
    panel_border(color = "black", size = 1) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = 14))
}

# ---- Generate Individual Plots ----
gpp_plot <- plot_metab_box(df, "gpp_g_m2_d", 
                           expression(Gross~primary~production~(g~O[2]~m^{-2}~d^{-1})), "a")

er_plot <- plot_metab_box(df, "er_g_m2_d", 
                          expression("|"~Ecosystem~respiration~"|"~(g~O[2]~m^{-2}~d^{-1})), "b")

nep_plot <- plot_metab_box(df, "nep_g_m2_d",
                           expression(Net~ecosystem~production~(g~O[2]~m^{-2}~d^{-1})),
                           "c", y_scale = "asinh", limits = c(-150, 0), breaks = c(-150, -50, -10, 0))

pr_plot <- ggplot(df, aes(x = deposition, y = pr_mean, color = deposition, fill = deposition)) +
  geom_hline(yintercept = 1, linewidth = 1, linetype = "longdash") +
  geom_boxplot(width = 0.35, linewidth = 1, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(stroke = 1.3), alpha = 0.5, size = 3, position = position_dodge2(0.1)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 18,
               position = position_dodge(width = 0.45)) +
  scale_color_manual(values = c("Active" = "#0072B2", "Non-active" = "#CC79A7")) +
  scale_fill_manual(values = c("Active" = "#0072B2", "Non-active" = "#CC79A7")) +
  labs(title = "d", x = "Deposition", y = "P:R ratio") +
  theme_cowplot(font_size = 20, line_size = 0.5) +
  panel_border(color = "black", size = 1) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14))

# ---- Combine and Save Plots ----
combined_metab_plot <- grid.arrange(gpp_plot, er_plot, nep_plot, pr_plot, ncol = 2)

ggsave("./figs/FigureS3.tiff", plot = combined_metab_plot,
       width = 10.5, height = 10.5, dpi = 300)

message("✔ All models run, diagnostics shown, and plots saved.")
