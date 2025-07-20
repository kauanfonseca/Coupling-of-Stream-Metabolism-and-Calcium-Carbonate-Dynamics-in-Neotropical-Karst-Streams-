# ===============================
# Stream Metabolism Scaling Plots
# Description: Visualizes the relationship between GPP and ER
#              for streams with active and non-active travertine deposition
# ===============================

# ---- Load Required Packages ----
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)

# ---- Load Data ----
df <- read.csv("./data/metabolism_data.csv")

# ---- Filter Data by Deposition Type ----
meta_active <- df %>% filter(deposition == "Active")
meta_nonactive <- df %>% filter(deposition == "Non-active")

# ---- Plot: Active Deposition Streams ----
Met_active <- ggplot(meta_active, aes(x = gpp_g_m2_d, y = er_g_m2_d,
                                      color = deposition, fill = deposition)) +
  geom_abline(slope = 1, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_abline(slope = 3.3, intercept = 7.62, color = "#0072B2", linewidth = 1) +
  geom_point(aes(shape = stream), size = 4, stroke = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_color_manual(values = c("Active" = "#0072B2", "Non-active" = "#CC79A7"),
                     aesthetics = c("colour", "fill")) +
  scale_x_continuous(
    name = expression(Gross~primary~production~(g~O[2]~m^{-2}~d^{-1})),
    limits = c(0, 55)) +
  scale_y_continuous(
    name = expression("|"~Ecosystem~respiration~"|"~(g~O[2]~m^{-2}~d^{-1})),
    limits = c(0, 55)) +
  labs(title = "a", fill = "Deposition", color = "Deposition") +
  theme_cowplot(font_size = 20, line_size = 0.5) +
  panel_border(color = "black", size = 1) +
  annotate("text", x = 32, y = 53,
           label = "y == 7.62 + 3.31 * x ~~ R^2 == 0.84",
           parse = TRUE, color = "#0072B2", size = 6)

# ---- Plot: Non-active Deposition Streams ----
Met_nonactive <- ggplot(meta_nonactive, aes(x = gpp_g_m2_d, y = er_g_m2_d,
                                            shape = stream, color = deposition, fill = deposition)) +
  geom_abline(slope = 1, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_point(size = 4, stroke = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c(6, 8, 9)) +
  scale_color_manual(values = c("Non-active" = "#CC79A7"),
                     aesthetics = c("colour", "fill")) +
  scale_x_continuous(
    name = expression(Gross~primary~production~(g~O[2]~m^{-2}~d^{-1})),
    limits = c(0, 55)) +
  scale_y_continuous(
    name = expression("|"~Ecosystem~respiration~"|"~(g~O[2]~m^{-2}~d^{-1})),
    limits = c(0, 200)) +
  labs(title = "b", fill = "Deposition", color = "Deposition", shape = "Stream") +
  theme_cowplot(font_size = 20, line_size = 0.5) +
  panel_border(color = "black", size = 1)

# ---- Combine Plots ----
Met_combined <- Met_active + Met_nonactive + plot_layout(ncol = 2)
Met_combined

# ---- Save Figure ----
ggsave(plot = Met_combined,
       filename = "./figs/Figure2.tiff",
       width = 16, height = 6, dpi = 300)

message("Figure2.tiff saved to ./figs/")

