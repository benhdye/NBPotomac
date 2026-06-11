library(dplyr)
library(zoo)
library(ggplot2)
library(readr)

# ── 0. Load data ──────────────────────────────────────────────────────────────
flows_all_15min <- read_csv(
  "https://media.githubusercontent.com/media/benhdye/NBPotomac/main/CSVs/flows_all_15min.csv",
  col_types = cols(
    dateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")
  )
) %>%
  mutate(dateTime = force_tz(dateTime, tz = "UTC")) %>%
  arrange(site, dateTime)

# ── 1. Feature engineering ────────────────────────────────────────────────────
flows_all_15min <- flows_all_15min %>%
  group_by(site) %>%
  mutate(
    # --- Peak detection: local max within ±90 min (13 steps @ 15 min) ---
    roll_max  = rollapply(Flow_Inst, width = 13, FUN = max,
                          fill = NA, align = "center"),
    is_peak   = Flow_Inst == roll_max,
    
    # --- Rise/fall slopes (6 steps = 90 min) ---
    pre_slope  = Flow_Inst - lag(Flow_Inst, 6),
    post_slope = lead(Flow_Inst, 6) - Flow_Inst,
    asymmetry  = pre_slope / abs(post_slope),
    
    # --- Baseline: median flow over 24 hrs BEFORE the event (96 steps) ---
    pre_baseline = rollapply(Flow_Inst, width = 96, FUN = median,
                             fill = NA, align = "right"),
    
    # --- Return-to-baseline: median flow over 6 hrs AFTER peak ---
    post_return    = rollapply(Flow_Inst, width = 24, FUN = median,
                               fill = NA, align = "left"),
    baseline_ratio = post_return / pre_baseline,
    
    # --- Antecedent stability: CV over prior 24 hrs ---
    pre_cv = rollapply(Flow_Inst, width = 96,
                       FUN = function(x) sd(x) / mean(x),
                       fill = NA, align = "right"),
    
    # --- Implausible spike guard: flag if rise > 5x in one step ---
    frac_change   = Flow_Inst / lag(Flow_Inst),
    suspect_spike = frac_change > 5 & lead(frac_change) < 0.25,
    
    # --- Post-peak recession: flow 4 hrs after peak relative to peak ---
    post_4hr       = lead(Flow_Inst, 16),
    recession_frac = post_4hr / Flow_Inst
  ) %>%
  ungroup()

# ── 2. Candidate filter ───────────────────────────────────────────────────────
hydropeak_candidates <- flows_all_15min %>%
  filter(
    is_peak         == TRUE,
    pre_slope        > 0,
    post_slope       < 0,
    asymmetry        > 2,
    baseline_ratio   > 0.7,
    baseline_ratio   < 1.4,
    pre_cv           < 0.3,
    suspect_spike   == FALSE,
    recession_frac   < 0.85    # flow dropped ≥15% within 4 hrs of peak
  )

# ── 3. Cluster and keep one peak per event (3-hr gap = new event) ─────────────
hydropeak_candidates <- hydropeak_candidates %>%
  arrange(site, dateTime) %>%
  group_by(site) %>%
  mutate(
    time_since_last = as.numeric(difftime(dateTime, lag(dateTime), units = "mins")),
    new_event       = is.na(time_since_last) | time_since_last > 180
  ) %>%
  filter(new_event) %>%
  ungroup()

# ── 4. Inter-event spacing summary ───────────────────────────────────────────
spacing_summary <- hydropeak_candidates %>%
  group_by(site) %>%
  summarize(
    n_events           = n(),
    median_interval_hr = median(time_since_last, na.rm = TRUE) / 60,
    cv_interval        = sd(time_since_last, na.rm = TRUE) /
      median(time_since_last, na.rm = TRUE),
    .groups = "drop"
  )
print(spacing_summary)

# ── 5. Plot helper ────────────────────────────────────────────────────────────
plot_event_window <- function(df, event_time, site_name, before = 24, after = 24) {
  
  site_data  <- df %>%
    filter(site == site_name) %>%
    arrange(dateTime) %>%
    ungroup()
  
  event_time <- as.POSIXct(event_time, tz = "UTC")
  event_idx  <- which.min(abs(as.numeric(difftime(site_data$dateTime, event_time, units = "secs"))))
  
  start_idx  <- max(1, event_idx - before)
  end_idx    <- min(nrow(site_data), event_idx + after)
  
  window <- site_data[start_idx:end_idx, ]
  window$is_event <- seq_len(nrow(window)) == (event_idx - start_idx + 1)
  
  ggplot(window, aes(x = dateTime, y = Flow_Inst)) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(color = is_event), size = 3) +
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    scale_x_datetime(date_labels = "%b %d %H:%M", date_breaks = "2 hours") +
    labs(
      title    = paste0(site_name, ": Hydropeak candidate at ",
                        format(event_time, "%Y-%m-%d %H:%M")),
      subtitle = paste0(before * 15, " min before / ", after * 15, " min after"),
      x = "Time", y = "Discharge (cfs)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 45, hjust = 1)
    )
}

# ── 6. Quick visual check of top candidates ───────────────────────────────────
top_candidates <- hydropeak_candidates %>%
  filter(site == "Barnum") %>%
  arrange(desc(pre_slope)) %>%
  slice(1:10)

for (i in seq_len(nrow(top_candidates))) {
  p <- plot_event_window(
    flows_all_15min,
    event_time = top_candidates$dateTime[i],
    site_name  = "Barnum"
  )
  print(p)
}
