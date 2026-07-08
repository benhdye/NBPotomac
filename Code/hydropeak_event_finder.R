library(dplyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(plotly)
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
top_candidates_barnum <- hydropeak_candidates %>%
  filter(site == "Barnum") %>%
  arrange(desc(pre_slope)) %>%
  slice(1:10)

top_candidates_all <- hydropeak_candidates %>% 
  arrange(desc(pre_slope)) %>% 
  slice()

for (i in seq_len(nrow(top_candidates_barnum))) {
  p <- plot_event_window(
    flows_all_15min,
    event_time = top_candidates_barnum$dateTime[i],
    site_name  = top_candidates_barnum$site[i]
  )
  print(p)
}

p

library(plotly)

# ── 7. Synchronized Barnum vs Kitzmiller comparison ───────────────────────────

# Build a plotly version of the event-window plot, returns a plotly object
plot_event_window_plotly <- function(df, event_time, site_name, before = 24, after = 24,
                                     mark_event = TRUE) {
  
  site_data <- df %>%
    filter(site == site_name) %>%
    arrange(dateTime) %>%
    ungroup()
  
  event_time <- as.POSIXct(event_time, tz = "UTC")
  event_idx  <- which.min(abs(as.numeric(difftime(site_data$dateTime, event_time, units = "secs"))))
  
  start_idx <- max(1, event_idx - before)
  end_idx   <- min(nrow(site_data), event_idx + after)
  
  window <- site_data[start_idx:end_idx, ]
  window$is_event <- seq_len(nrow(window)) == (event_idx - start_idx + 1)
  
  p <- plotly::plot_ly(window, x = ~dateTime, y = ~Flow_Inst,
                       type = "scatter", mode = "lines",
                       line = list(color = "grey40", width = 1.5),
                       name = site_name)
  
  if (mark_event) {
    event_pt <- window[window$is_event, ]
    p <- p %>%
      plotly::add_trace(data = event_pt, x = ~dateTime, y = ~Flow_Inst,
                        type = "scatter", mode = "markers",
                        marker = list(color = "red", size = 9),
                        name = paste(site_name, "event"), showlegend = FALSE)
  }
  
  p %>%
    plotly::layout(
      yaxis = list(title = "Discharge (cfs)"),
      xaxis = list(title = "")
    )
}

# Build a stacked Barnum/Kitzmiller comparison for ONE event time
compare_event_plotly <- function(df, event_time, before = 24, after = 24) {
  
  p_barnum <- plot_event_window_plotly(df, event_time, "Barnum",
                                       before = before, after = after,
                                       mark_event = TRUE) %>%
    plotly::layout(yaxis = list(title = "Barnum (cfs)"))
  
  p_kitz <- plot_event_window_plotly(df, event_time, "Kitzmiller",
                                     before = before, after = after,
                                     mark_event = FALSE) %>%
    plotly::layout(yaxis = list(title = "Kitzmiller (cfs)"))
  
  plotly::subplot(p_barnum, p_kitz, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
    plotly::layout(
      title = paste0("Barnum hydropeak event vs Kitzmiller — ",
                     format(as.POSIXct(event_time, tz = "UTC"), "%Y-%m-%d %H:%M")),
      showlegend = FALSE
    )
}

# ── 8. Run it over your Barnum candidates ─────────────────────────────────────
barnum_events <- hydropeak_candidates %>%
  filter(site == "Barnum") %>%
  arrange(desc(pre_slope))

stacked_plots <- vector("list", nrow(barnum_events))

for (i in seq_len(nrow(barnum_events))) {
  stacked_plots[[i]] <- compare_event_plotly(
    flows_all_15min,
    event_time = barnum_events$dateTime[i]
  )
}

# Print one at a time, e.g. the top event by pre_slope:
stacked_plots[[10]]


#Known release periods
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(plotly)

###############################################################################
# 1. Build wide dataframe (handles duplicate timestamps)
###############################################################################

flows_wide <- flows_all_15min %>%
  filter(site %in% c("Barnum", "Kitzmiller")) %>%
  group_by(dateTime, site) %>%
  summarise(
    Flow_Inst = mean(Flow_Inst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = dateTime,
    names_from = site,
    values_from = Flow_Inst
  ) %>%
  arrange(dateTime)

###############################################################################
# 2. Keep only timestamps where both gauges exist
###############################################################################

flows_wide <- flows_wide %>%
  filter(
    !is.na(Barnum),
    !is.na(Kitzmiller)
  )

###############################################################################
# 3. Add release-season information
###############################################################################

flows_release <- flows_wide %>%
  mutate(
    
    local_time = with_tz(dateTime,"America/New_York"),
    
    year  = year(local_time),
    month = month(local_time),
    day   = day(local_time),
    hour  = hour(local_time),
    wday  = wday(local_time,label=TRUE),
    
    rafting_season =
      month %in% c(4,5),
    
    avf_season =
      (month==8 & day>=20) |
      (month==9 & day<=10)
    
  ) %>%
  
  filter(
    rafting_season | avf_season
  )

###############################################################################
# 4. Compute hydropeaking metrics
###############################################################################

flows_release <- flows_release %>%
  mutate(
    
    ##############################################################
    ## Barnum response
    ##############################################################
    
    barnum_change_15 =
      Barnum-lag(Barnum),
    
    barnum_change_1hr =
      Barnum-lag(Barnum,4),
    
    barnum_change_2hr =
      Barnum-lag(Barnum,8),
    
    ##############################################################
    ## Kitz response
    ##############################################################
    
    kitz_change_1hr =
      Kitzmiller-lag(Kitzmiller,4),
    
    ##############################################################
    ## Response ratio
    ##############################################################
    
    response_ratio =
      barnum_change_1hr/
      pmax(abs(kitz_change_1hr),1),
    
    ##############################################################
    ## Peak
    ##############################################################
    
    barnum_peak =
      Barnum==
      rollapply(
        Barnum,
        width=9,
        max,
        fill=NA,
        align="center"
      ),
    
    ##############################################################
    ## Baseline
    ##############################################################
    
    barnum_baseline =
      rollapply(
        Barnum,
        width=96,
        median,
        fill=NA,
        align="right"
      ),
    
    ##############################################################
    ## Return toward baseline
    ##############################################################
    
    recession =
      lead(Barnum,16)/Barnum,
    
    ##############################################################
    ## Upstream variability
    ##############################################################
    
    kitz_sd =
      rollapply(
        Kitzmiller,
        width=16,
        sd,
        fill=NA,
        align="center"
      )
    
  )

###############################################################################
# 5. Score every timestamp
###############################################################################

flows_release <- flows_release %>%
  mutate(
    
    score =
      
      3*(barnum_change_1hr > 150) +
      
      2*(barnum_change_2hr > 300) +
      
      3*(abs(kitz_change_1hr) < 30) +
      
      2*(response_ratio > 8) +
      
      2*(barnum_peak) +
      
      2*(recession < 0.90) +
      
      1*(kitz_sd < 20)
    
  )

###############################################################################
# 6. Keep high-scoring events
###############################################################################

release_candidates <- flows_release %>%
  filter(score >= 10)

###############################################################################
# 7. Collapse nearby timestamps into one event
###############################################################################

release_candidates <- release_candidates %>%
  arrange(dateTime) %>%
  mutate(
    
    minutes_since =
      as.numeric(
        difftime(
          dateTime,
          lag(dateTime),
          units="mins"
        )
      ),
    
    new_event =
      is.na(minutes_since) |
      minutes_since > 360
    
  ) %>%
  filter(new_event)

###############################################################################
# 8. Rank events
###############################################################################

release_candidates <- release_candidates %>%
  arrange(
    desc(score),
    desc(response_ratio),
    desc(barnum_change_2hr)
  )

print(release_candidates)

###############################################################################
# 9. Plot top candidates
###############################################################################

library(ggplot2)

plot_release_event <- function(df, event_time, hours_before = 12, hours_after = 24){
  
  window <- df %>%
    filter(
      site %in% c("Barnum","Kitzmiller"),
      dateTime >= event_time - hours(hours_before),
      dateTime <= event_time + hours(hours_after)
    ) %>%
    group_by(site) %>%
    mutate(
      Flow_scaled = (Flow_Inst - mean(Flow_Inst, na.rm=TRUE)) /
        sd(Flow_Inst, na.rm=TRUE)
    ) %>%
    ungroup()
  
  ggplot(window,
         aes(dateTime, Flow_scaled, color = site)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = event_time,
               linetype = 2,
               color = "black") +
    labs(
      title = paste("Candidate hydropeaking event",
                    format(event_time,"%Y-%m-%d %H:%M")),
      x = "",
      y = "Standardized flow"
    ) +
    theme_bw()
}

#10#####
for(i in seq_len(min(10,nrow(release_candidates)))){
  
  print(
    plot_release_event(
      flows_all_15min,
      release_candidates$dateTime[i]
    )
  )
  
}


##Plotly Figure Method:
#Load Data
library(dplyr)
library(lubridate)

flows_plot <- flows_all_15min %>%
  filter(site %in% c("Barnum","Kitzmiller")) %>%
  mutate(
    local_time = with_tz(dateTime, "America/New_York"),
    year  = year(local_time),
    month = month(local_time),
    day   = day(local_time)
  )

#Plot Figure
library(plotly)

plot_release_year <- function(df, yr){
  
  april_may <-
    df %>%
    filter(
      year == yr,
      month %in% c(4,5)
    )
  
  avf <-
    df %>%
    filter(
      year == yr,
      (month==8 & day>=20) |
        (month==9 & day<=10)
    )
  p1 <-
    plot_ly(
      april_may,
      x = ~local_time,
      y = ~Flow_Inst,
      color = ~site,
      colors = c("Barnum" = "#d62728",
                 "Kitzmiller" = "#1f77b4"),
      type = "scatter",
      mode = "lines"
    ) %>%
    layout(
      title = "April–May Whitewater Releases",
      yaxis = list(title = "Flow (cfs)")
    )
  
  p2 <-
    plot_ly(
      avf,
      x = ~local_time,
      y = ~Flow_Inst,
      color = ~site,
      colors = c("Barnum" = "#d62728",
                 "Kitzmiller" = "#1f77b4"),
      type = "scatter",
      mode = "lines",
      showlegend = FALSE
    ) %>%
    layout(
      title = "Annual Variable Flow",
      yaxis = list(title = "Flow (cfs)")
    )
  
  subplot(
    p1,
    p2,
    nrows=2,
    shareX=FALSE,
    titleY=TRUE
  ) %>%
    layout(
      title=paste("Hydropeaking Candidate Windows -",yr)
    )
  
}

#3. Loop

years <- sort(unique(flows_plot$year))

plots <- vector("list", length(years))

for(i in seq_along(years)){
  
  plots[[i]] <- plot_release_year(flows_plot, years[i])
  
}

#4. View

plots[[which(years==2014)]]

#5. Find event and switch to gg plot to export

library(dplyr)
library(ggplot2)
library(lubridate)

#-------------------------------------------------
# Convert to local time
#-------------------------------------------------

flows_plot <- flows_all_15min %>%
  filter(site %in% c("Barnum","Kitzmiller")) %>%
  mutate(
    local_time = with_tz(dateTime, "America/New_York")
  )

#-------------------------------------------------
# Define event centers
#-------------------------------------------------

may_event <- ymd_hm("2015-05-24 12:00", tz = "America/New_York")

aug_event <- ymd_hm("2015-08-23 12:00", tz = "America/New_York")

#-------------------------------------------------
# Window around each event
# (change days() if you want more/less context)
#-------------------------------------------------

window_days <- 2

may_data <- flows_plot %>%
  filter(
    local_time >= may_event - days(window_days),
    local_time <= may_event + days(window_days)
  ) %>%
  mutate(Event = "Whitewater Release\nMay 23–25, 2015")

aug_data <- flows_plot %>%
  filter(
    local_time >= aug_event - days(window_days),
    local_time <= aug_event + days(window_days)
  ) %>%
  mutate(Event = "Annual Variable Flow\nAugust 22–25, 2015")

plot_data <- bind_rows(may_data, aug_data)

#-------------------------------------------------
# Plot
#-------------------------------------------------

ggplot(plot_data,
       aes(local_time,
           Flow_Inst,
           color = site)) +
  
  geom_line(linewidth = 0.9) +
  
  facet_wrap(
    ~Event,
    ncol = 1,
    scales = "free_x"
  ) +
  
  scale_x_datetime(
    date_breaks = "6 hours",
    date_labels = "%b %d\n%H:%M"
  ) +
  
  scale_color_manual(
    values = c(
      "Barnum" = "red",
      "Kitzmiller" = "blue"
    )
  ) +
  
  labs(
    x = "",
    y = expression("Discharge ("*ft^3*"/s)"),
    color = ""
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )
