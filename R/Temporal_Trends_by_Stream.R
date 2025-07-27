#===============================================================================
# Stream Data Visualization: Temporal Trends by Stream and Variable
#===============================================================================

# ---- Libraries ----
library(tidyverse)
library(lubridate)
library(cowplot)
library(patchwork)

# ---- Load Data ----
df <- read.csv("./data/HighFrequencyData.csv", sep = ",", header = TRUE)

# ---- Fix Encoding in Stream Names ----
df <- df %>%
  mutate(stream_name = recode(
    stream_name,
    "C\x97rrego Baixo"          = "Córrego Baixo",
    "C\x97rrego Cabelo Duro"    = "Córrego Cabelo Duro",
    "C\x97rrego Cedral"         = "Córrego Cedral",
    "C\x97rrego Santa Rosa"     = "Córrego Santa Rosa",
    "Ribeir\x8bo Chiqueir\x8bo" = "Ribeirão Chiqueirão",
    "Rio A\x8d\x9ccar"          = "Rio Açúcar",
    "Rio Triste"                = "Rio Triste"
  ))

# ---- Convert Date ----
df$sampling_datetime <- ymd_hms(df$sampling_datetime, tz = "America/Sao_Paulo")

# ---- Utility: Gap Correction (only for Córrego Cabelo Duro) ----
adjust_cabelo_duro_time <- function(df, datetime_col = "sampling_datetime", stream_col = "stream_name") {
  df %>%
    arrange(.data[[stream_col]], .data[[datetime_col]]) %>%
    group_by(.data[[stream_col]]) %>%
    mutate(
      Time_diff = .data[[datetime_col]] - lag(.data[[datetime_col]]),
      Cum_gap_shift = if_else(
        .data[[stream_col]] == "Córrego Cabelo Duro",
        as.difftime(cumsum(
          if_else(
            is.na(Time_diff) | Time_diff < as.difftime(0.09, units = "days"),
            0,
            as.numeric(Time_diff - as.difftime(0.09, units = "days"), units = "mins")
          )
        ), units = "mins"),
        as.difftime(0, units = "mins")
      ),
      Time_plot = if_else(
        .data[[stream_col]] == "Córrego Cabelo Duro",
        .data[[datetime_col]] - Cum_gap_shift,
        .data[[datetime_col]]
      )
    ) %>%
    ungroup()
}

# ---- Utility: Nighttime Rectangle Generator ----
generate_night_rects <- function(df, time_col, group_col) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    reframe(dates = unique(as.Date(.data[[time_col]]))) %>%
    rowwise() %>%
    mutate(
      xmin = as.POSIXct(paste(dates, "18:00:00")),
      xmax = as.POSIXct(paste(dates + 1, "06:00:00")),
      ymin = -Inf,
      ymax = Inf
    ) %>%
    ungroup()
}

# ---- Plot Function Template ----

make_stream_plot <- function(data, scaling_factors, var1, var2, scale_factor_label, color_map, y_primary_label, y_secondary_label) {
  data %>%
    filter(!is.na(travertine_deposition)) %>%
    split(.$Stream_status) %>%
    map(~ {
      stream_label <- unique(.x$Stream_status)
      stream_name  <- unique(.x$stream_name)
      
      x_var <- "Time_plot"
      night_rects_local <- generate_night_rects(.x, x_var, "stream_name")
      
      scale_factor <- scaling_factors %>%
        filter(Stream_status == stream_label) %>%
        pull(scaling_factor)
      
      if (stream_name == "Córrego Cabelo Duro") {
        time_map <- .x %>%
          distinct(Time_plot, sampling_datetime) %>%
          drop_na()
        
        labels_x <- function(x_ticks) {
          sapply(x_ticks, function(tick) {
            idx <- which.min(abs(difftime(tick, time_map$Time_plot)))
            real_date <- time_map$sampling_datetime[idx]
            strftime(real_date, format = "%d\n%b\n%Y", tz = "America/Sao_Paulo")
          })
        }
      } else {
        labels_x <- waiver()
      }
      
      ggplot(.x, aes(x = .data[[x_var]], colour = Variable)) +
        geom_rect(
          data = night_rects_local, inherit.aes = FALSE,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "gray80", alpha = 0.3
        ) +
        geom_line(aes(y = ifelse(Variable == var1, Value, Value * scale_factor)), linewidth = 3) +
        scale_x_datetime(
          date_breaks = "1 day",
          labels = scales::label_date("%d\n%b\n%Y")
        ) +
        scale_y_continuous(
          name = y_primary_label,
          sec.axis = sec_axis(~ . / scale_factor, name = y_secondary_label)
        ) +
        scale_colour_manual(values = color_map, labels = scale_factor_label) +
        labs(x = "Date and Time", colour = NULL) +
        ggtitle(stream_label) +
        panel_border(color = "black", size = 1) +
        theme_cowplot(font_size = 35, line_size = 1) +
        theme(
          axis.text.x = element_text(size = 35),
          legend.position = "bottom",
          legend.text = element_text(size = 30),
          strip.text = element_text(size = 32),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    })
}
options(patchwork.force_independent = TRUE)

# ==============================================================================
# ---- Plot Block 1: Calcium & Dissolved Oxygen (mg/L) ----
# ==============================================================================
df_ca_o2 <- df %>%
  pivot_longer(c(calcium_mgL, dissolved_oxygen_mgL), names_to = "Variable", values_to = "Value") %>%
  adjust_cabelo_duro_time() %>%
  mutate(Stream_status = paste(stream_name, "-", travertine_deposition))

scaling_factors_ca <- df_ca_o2 %>%
  filter(Variable %in% c("calcium_mgL", "dissolved_oxygen_mgL")) %>%
  group_by(Stream_status, Variable) %>%
  summarize(median_value = median(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = median_value) %>%
  mutate(scaling_factor = calcium_mgL / dissolved_oxygen_mgL)

color_map_ca <- c("calcium_mgL" = "#FFC20A", "dissolved_oxygen_mgL" = "#0C7BDC")
label_map_ca <- c(
  "calcium_mgL" = expression(Ca^{"2+"}~(mg~L^{"-1"})),
  "dissolved_oxygen_mgL" = expression(O[2]~(mg~L^{-1}))
)

plots_ca <- make_stream_plot(
  data = df_ca_o2,
  scaling_factors = scaling_factors_ca,
  var1 = "calcium_mgL",
  var2 = "dissolved_oxygen_mgL",
  scale_factor_label = label_map_ca,
  color_map = color_map_ca,
  y_primary_label = expression(Ca^{"2+"}~(mg~L^{-1})),
  y_secondary_label = expression(O[2]~(mg~L^{-1}))
)

final_plot_Ca_O2 <- wrap_plots(plots_ca, ncol = 4) & theme(axis.title.x = element_blank())

# ==============================================================================
# ---- Plot Block 2: Specific Conductivity & DO (%) ----
# ==============================================================================
df_spc_do <- df %>%
  pivot_longer(c(specific_cond_uS_cm, dissolved_oxygen_percent), names_to = "Variable", values_to = "Value") %>%
  adjust_cabelo_duro_time() %>%
  mutate(Stream_status = paste(stream_name, "-", travertine_deposition))

scaling_factors_spc <- df_spc_do %>%
  filter(Variable %in% c("specific_cond_uS_cm", "dissolved_oxygen_percent")) %>%
  group_by(Stream_status, Variable) %>%
  summarize(median_value = median(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = median_value) %>%
  mutate(scaling_factor = specific_cond_uS_cm / dissolved_oxygen_percent)

color_map_spc <- c("specific_cond_uS_cm" = "#E1BE6A", "dissolved_oxygen_percent" = "#40B0A6")
label_map_spc <- c(
  "specific_cond_uS_cm" = expression(Specific~Conductivity~(µS~cm^{-1})),
  "dissolved_oxygen_percent" = expression(O[2]~("% saturation"))
)

plots_spc <- make_stream_plot(
  data = df_spc_do,
  scaling_factors = scaling_factors_spc,
  var1 = "specific_cond_uS_cm",
  var2 = "dissolved_oxygen_percent",
  scale_factor_label = label_map_spc,
  color_map = color_map_spc,
  y_primary_label = expression(Specific~Conductivity~(µS~cm^{-1})),
  y_secondary_label = expression(O[2]~("% saturation"))
)

final_plot_SpC_DO <- wrap_plots(plots_spc, ncol = 4) & theme(axis.title.x = element_blank())

# ==============================================================================
# ---- Plot Block 3: Temperature & Light ----
# ==============================================================================
df_temp_par <- df %>%
  pivot_longer(c(temp_c, par_umol_m2_s), names_to = "Variable", values_to = "Value") %>%
  mutate(Value_scaled = ifelse(Variable == "par_umol_m2_s", Value / 10, Value)) %>%
  adjust_cabelo_duro_time() %>%
  mutate(Stream_status = paste(stream_name, "-", travertine_deposition))

plots_temp <- df_temp_par %>%
  filter(!is.na(travertine_deposition)) %>%
  split(.$Stream_status) %>%
  map(~ {
    night_rects_local <- generate_night_rects(.x, "Time_plot", "stream_name")
    ggplot(.x, aes(x = Time_plot, y = Value_scaled, colour = Variable)) +
      geom_rect(data = night_rects_local, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "gray80", alpha = 0.3) +
      geom_line(linewidth = 3) +
      scale_x_datetime(
        date_breaks = "1 day",
        labels = scales::label_date("%d\n%b\n%Y")
      ) +
      scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = sec_axis(~ . * 10, name = expression(PAR~(µmol~m^{-2}~s^{-1})))
      ) +
      scale_colour_manual(
        values = c("temp_c" = "#5D3A9B", "par_umol_m2_s" = "#E66100"),
        labels = c(
          "temp_c" = "Temperature (°C)",
          "par_umol_m2_s" = expression(PAR~(µmol~m^{-2}~s^{-1}))
        )
      ) +
      labs(x = "Date and Time", colour = NULL) +
      ggtitle(unique(.x$Stream_status)) +
      panel_border(color = "black", size = 1) +
      theme_cowplot(font_size = 35, line_size = 1) +
      theme(
        axis.text.x = element_text(size = 35),
        legend.position = "bottom",
        legend.text = element_text(size = 30),
        strip.text = element_text(size = 32),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

final_plot_Temp_PAR <- wrap_plots(plots_temp, ncol = 4)

# ==============================================================================
# ---- Combine All Plots ----
# ==============================================================================
FigureS4 <- final_plot_Ca_O2 / final_plot_SpC_DO / final_plot_Temp_PAR

ggsave("./figs/FigureS4.pdf", plot = FigureS4, width = 52, height = 60, dpi = 300, limitsize = FALSE)
