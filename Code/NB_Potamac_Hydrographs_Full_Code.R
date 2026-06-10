library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(dataRetrieval)
library(lubridate)


# pull interval data (instantaneous values)

# 
# flows_Kitzmiller <- readNWISuv(
#   siteNumbers = "01595500",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)
# 
# flows_Barnum <- readNWISuv(
#   siteNumbers = "01595800",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)
# 
# flows_Barton <- readNWISuv(
#   siteNumbers = "01596500",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

#Daily Flow
# 
# flows_Kitzmiller <- readNWISdv(
#   siteNumbers = "01595500",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow) - Flow)
# 
# flows_Barnum <- readNWISuv(
#   siteNumbers = "01595800",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow) - Flow)
# 
# flows_Barton <- readNWISuv(
#   siteNumbers = "01596500",
#   parameterCd = "00060",
#   startDate = "2003-10-01",
#   endDate = "2025-09-17"
# ) %>% renameNWISColumns() %>%
#   mutate(flow_diff = lead(Flow) - Flow)

# #Pull data from .rds files to save time
# 
# #saveRDS(flows_Barnum, "flows_Barnum.rds")
# flows_Barnum <- readRDS("flows_Barnum.rds")
# 
# #saveRDS(flows_Barton, "flows_Barton.rds")
# flows_Barton <- readRDS("flows_Barton.rds")
# 
# #saveRDS(flows_Kitzmiller, "flows_Kitzmiller.rds")
# flows_Kitzmiller <- readRDS("flows_Kitzmiller.rds")

flows_Barnum <- read_csv("flows_Barnum.csv")
flows_Barton <- read_csv("flows_Barton.csv")
flows_Kitzmiller <- read_csv("flows_Kitzmiller.csv")

#make hourly
flows_Barnum <- flows_Barnum %>%
  mutate(
    dateTime = parse_date_time(dateTime, 
                               orders = c("ymd_HMS", "ymd_HM", "mdy_HMS", "ymd")),
    hour = round_date(dateTime, unit = "hour")
  )

flows_Barnum_15min <- flows_Barnum %>%
  group_by(hour) %>%
  slice_min(abs(dateTime - hour), n = 1) %>%
  ungroup()

flows_Barton <- flows_Barton %>%
  mutate(
    dateTime = parse_date_time(dateTime, 
                               orders = c("ymd_HMS", "ymd_HM", "mdy_HMS", "ymd")),
    hour = round_date(dateTime, unit = "hour")
  )

flows_Barton_15min <- flows_Barton %>%
  group_by(hour) %>%
  slice_min(abs(dateTime - hour), n = 1) %>%
  ungroup()

flows_Kitzmiller <- flows_Kitzmiller %>%
  mutate(
    dateTime = parse_date_time(dateTime, 
                               orders = c("ymd_HMS", "ymd_HM", "mdy_HMS", "ymd")),
    hour = round_date(dateTime, unit = "hour")
  )

flows_Kitzmiller_15min <- flows_Kitzmiller %>%
  group_by(hour) %>%
  slice_min(abs(dateTime - hour), n = 1) %>%
  ungroup()



# ggplot(data=flows_Kitzmiller, mapping = aes(x=dateTime, y= Flow_Inst))+
#   geom_line(color="red")+labs(title = "Kitzmiller cfs inst 2003-2025")+xlab("Time")+ylab("Discharge cfs isnt")
# 
# ggplot(data=flows_Barnum, mapping = aes(x=dateTime, y= Flow_Inst))+
#   geom_line(color="blue")+
# +labs(title = "Barnum cfs inst 2003-2025")+xlab("Time")+ylab("Discharge cfs inst")
# 
# ggplot(data=flows_Barton, mapping = aes(y=dateTime, y= Flow_Inst))+
#   geom_line(color="purple")+labs(title = "Barton cfs inst 2003-2025")+xlab("Time")+ylab("Discharge cfs inst")

# Create a data frame for the reference line
line_data <- data.frame(
  Flow_Inst = 10^seq(0, 5, length.out = 100)  # from 1 to 100,000 in log space
) %>%
  mutate(flow_diff = 0.1 * Flow_Inst)  # 10% increase line

# kitzmiller_p <- ggplot(data=flows_Kitzmiller, mapping = aes(y=flow_diff, x= Flow_Inst))+
#   geom_point(color="red")+
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff),
#             color = "black", linetype = "solid", linewidth = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(title = "Kitzmiller cfs inst vs Difference 2003-2025")+xlab("low Instananous")+ylab("Flow Difference")

# barnum_p <- ggplot(data=flows_Barnum, mapping = aes(y=flow_diff, x= Flow_Inst))+
#   geom_point(color="blue")+
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff), 
#             color = "black", linetype = "solid", linewidth = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(title = "Barnum cfs inst vs Difference 2003-2025")+xlab("Flow Instananous")+ylab("Flow Difference")
# 
# barton_p <- ggplot(data=flows_Barton, mapping = aes(y=flow_diff, x= Flow_Inst))+
#   geom_point(color="purple")+
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff), 
#             color = "black", linetype = "solid", linewidth = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#  labs(title = "Barton cfs inst vs Difference 2003-2025")+xlab("Flow Instananous")+ylab("Flow Difference")

# print(kitzmiller_p)
# print(barnum_p)
# print(barton_p)
# 
# flows_Kitzmiller %>%
#   mutate(time_diff = difftime(lead(dateTime), dateTime, units = "mins")) %>%
#   pull(time_diff) %>%
#   table()

flows_Barnum_15min <- flows_Barnum %>%
  mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
  filter(time_diff == 15) %>%
  mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

flows_Kitzmiller_15min <- flows_Kitzmiller %>%
  mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
  filter(time_diff == 15) %>%
  mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

flows_Barton_15min <- flows_Barton %>%
  mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
  filter(time_diff == 15) %>%
  mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

filter_15min_intervals <- function(df) {
  df %>%
    mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
    filter(time_diff == 15) %>%
    mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)
}

flows_Kitzmiller_15min <- filter_15min_intervals(flows_Kitzmiller)
flows_Barnum_15min     <- filter_15min_intervals(flows_Barnum)
flows_Barton_15min     <- filter_15min_intervals(flows_Barton)

line_data <- data.frame(
  Flow_Inst = 10^seq(0, 5, length.out = 100)  # 1 to 100,000
) %>%
  mutate(flow_diff = 0.1 * Flow_Inst)

# ggplot(flows_Kitzmiller_15min, aes(x = Flow_Inst, y = flow_diff)) +
#   geom_point(color = "red", alpha = 0.4) +
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff),
#             color = "black", size = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(
#     title = "Kitzmiller: Flow Inst vs Flow Diff (15-min only)",
#     x = "Flow (cfs)",
#     y = "Flow Diff (lead - current)"
#   )

# ggplot(flows_Barton_15min, aes(x = Flow_Inst, y = flow_diff)) +
#   geom_point(color = "purple", alpha = 0.4) +
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff),
#             color = "black", size = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(
#     title = "Barton: Flow Inst vs Flow Diff (15-min only)",
#     x = "Flow (cfs)",
#     y = "Flow Diff (lead - current)"
#   )

# ggplot(flows_Barnum_15min, aes(x = Flow_Inst, y = flow_diff)) +
#   geom_point(color = "blue", alpha = 0.4) +
#   geom_line(data = line_data, aes(x = Flow_Inst, y = flow_diff),
#             color = "black", size = 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(
#     title = "Barton: Flow Inst vs Flow Diff (15-min only)",
#     x = "Flow (cfs)",
#     y = "Flow Diff (lead - current)"
#   )

###Density Plots:
# Linear scale:
ggplot(flows_Barnum, aes(x = flow_diff)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Barnum Flow Difference PDF (Linear Scale)")

ggplot(flows_Barton, aes(x = flow_diff)) +
  geom_density(fill = "purple", alpha = 0.4) +
  labs(title = "Barton Flow Difference PDF (Linear Scale)")

ggplot(flows_Kitzmiller, aes(x = flow_diff)) +
  geom_density(fill = "red", alpha = 0.4) +
  labs(title = "Kitzmiller Flow Difference PDF (Linear Scale)")
# Log scale
ggplot(flows_Barnum, aes(x = flow_diff)) +
  geom_density(fill = "blue", alpha = 0.4) +
  scale_x_log10() +
  labs(title = "Barnum Flow Difference PDF (Log10 Scale)")

ggplot(flows_Barton, aes(x = flow_diff)) +
  geom_density(fill = "purple", alpha = 0.4) +
  scale_x_log10() +
  labs(title = "Barton Flow Difference PDF (Log10 Scale)")

ggplot(flows_Kitzmiller, aes(x = flow_diff)) +
  geom_density(fill = "red", alpha = 0.4) +
  scale_x_log10() +
  labs(title = "Kitzmiller Flow Difference PDF (Log10 Scale)")

flow_all_15min <- bind_rows(
  flows_Barnum_15min %>% mutate(site = "Barnum"),
  flows_Barton_15min %>% mutate(site = "Barton"),
  flows_Kitzmiller_15min %>% mutate(site = "Kitzmiller")
)

# Compute medians for each site
medians <- flow_all_15min %>%
  filter(flow_diff > 0) %>%
  group_by(site) %>%
  summarize(median_diff = median(flow_diff, na.rm = TRUE))

# Plot PDFs with medians
ggplot(flow_all_15min, aes(x = flow_diff, fill = site, color = site)) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  geom_vline(
    data = medians,
    aes(xintercept = median_diff, color = site),
    linetype = "dashed",
    linewidth = 1, alpha = 0.3
  ) +
  labs(
    title = "Flow Difference PDFs with Median Lines (2003–2025)",
    x = "Flow Difference (cfs, log10 scale)",
    y = "Density"
  )


flows_Barnum_15min <- flows_Barnum_15min %>%
  mutate(
    flow_diff = lead(Flow_Inst) - Flow_Inst,
    frac_change = flow_diff / Flow_Inst,
    pct_change = 100 * frac_change
  )

flows_Barton_15min <- flows_Barton_15min %>%
  mutate(
    flow_diff = lead(Flow_Inst) - Flow_Inst,
    frac_change = flow_diff / Flow_Inst,
    pct_change = 100 * frac_change
  )

flows_Kitzmiller_15min <- flows_Kitzmiller_15min %>%
  mutate(
    flow_diff = lead(Flow_Inst) - Flow_Inst,
    frac_change = flow_diff / Flow_Inst,
    pct_change = 100 * frac_change
  )

flow_all_15min <- bind_rows(
  flows_Barnum_15min %>% mutate(site = "Barnum"),
  flows_Barton_15min %>% mutate(site = "Barton"),
  flows_Kitzmiller_15min %>% mutate(site = "Kitzmiller")
)

ggplot(flow_all_15min, aes(x = frac_change, fill = site, color = site)) +
  geom_density(alpha = 0.3) +
  scale_x_log10() +
  geom_vline(xintercept = 0.1, linetype = "solid", color = "black", linewidth = 1) +
  annotate("text", x = 0.1, y = 0.05, label = "10% increase", angle = 90, vjust = -0.5) +
  labs(
    title = "Normalized Flow Change PDFs (ΔQ/Q, 15-min intervals)",
    x = "Log10 scale Fractional Change (ΔQ / Q)",
    y = "Density"
  )


# --- Identify and analyze >10% fractional increases ---



# 10% flows
high_flows_10pct <- flow_all_15min %>%
  filter(frac_change > 0.10) %>%
  drop_na(frac_change, Flow_Inst)
summary(high_flows_10pct)
table(high_flows_10pct$site)# Filter for flow increases greater than 10%



# --- Overlapping PDF for >10% flow increases ---

medians <- high_flows_10pct %>%
  group_by(site) %>%
  summarize(med_flow = median(Flow_Inst, na.rm = TRUE))

ggplot(high_flows_10pct, aes(x = Flow_Inst, color = site, fill = site)) +
  geom_density(alpha = 0.25) +
  geom_vline(data = medians, aes(xintercept = med_flow, color = site), linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Flow Distributions During >10% Increases (with Median Lines)",
    x = "Instantaneous Flow (cfs, log10 scale)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

#convert to m^3/s

high_flows_10pct <- high_flows_10pct %>%
  mutate(
    Flow_Inst_m3s = Flow_Inst * 0.0283168,
    flow_diff_m3s = flow_diff * 0.0283168
  )
ggplot(high_flows_10pct, 
       aes(x = Flow_Inst_m3s, y = flow_diff_m3s, color = site)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Flow Difference vs. Flow During >10% Increases",
    x = "Instantaneous Flow (m³/s, log10 scale)",
    y = "Flow Difference (m³/s, log10 scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())


ggplot(high_flows_10pct, aes(x = Flow_Inst, y = flow_diff, color = site)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Flow Difference vs. Flow During >10% Increases",
    x = "Instantaneous Flow (cfs, log10 scale)",
    y = "Flow Difference (cfs, log10 scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

# Define site-specific flow ranges (cfs)
flow_ranges <- list(
  "Barnum" = c(300, 2000),
  "Kitzmiller" = c(253.760, 1691.73),
  "Barton" = c(55.376, 369.173)
)

# Function to create a plot for each site, with color argument
plot_slice <- function(site_name, flow_min, flow_max, pt_color = "black") {
  ggplot(
    high_flows_10pct %>%
      filter(site == site_name,
             Flow_Inst >= flow_min,
             Flow_Inst <= flow_max),
    aes(x = Flow_Inst, y = flow_diff)
  ) +
    geom_point(color = pt_color, alpha = 0.5, size = 2) +
    scale_x_log10() +
    scale_y_log10() +
    geom_abline(slope = 1, intercept = log10(0.1), linetype = "dashed") +
    labs(
      title = paste0(site_name, ": Flow Difference vs Flow (", 
                     flow_min, "–", flow_max, " cfs)"),
      x = "Instantaneous Flow (cfs, log10 scale)",
      y = "Flow Difference (cfs, log10 scale)"
    ) +
    theme_minimal(base_size = 13)
}

# Generate individual plots
plot_kitzmiller <- plot_slice("Kitzmiller", 253.760, 1691.73, pt_color = "blue")
plot_barnum      <- plot_slice("Barnum", 300, 2000, pt_color = "red")
plot_barton      <- plot_slice("Barton", 55.376, 369.173, pt_color = "green")

# Display them together
library(patchwork)
plot_barnum / plot_kitzmiller / plot_barton


# Clean and summarize
# NB_pot_summary_table <- flow_all_15min %>%
#   filter(!is.na(frac_change), !is.na(Flow_Inst)) %>%
#   left_join(flow_ranges, by = "site") %>%
#   mutate(
#     above_10pct = frac_change > 0.10,
#     within_range = Flow_Inst >= flow_min & Flow_Inst <= flow_max,
#     above_10pct_in_range = above_10pct & within_range
#   ) %>%
#   group_by(site) %>%
#   summarise(
#     total_points = n(),
#     above_10pct = sum(above_10pct),
#     below_10pct = sum(!above_10pct),
#     above_10pct_in_range = sum(above_10pct_in_range)
#   ) %>%
#   mutate(
#     pct_above_10pct = round(100 * above_10pct / total_points, 2),
#     pct_below_10pct = round(100 * below_10pct / total_points, 2),
#     pct_above_in_range = round(100 * above_10pct_in_range / total_points, 2)
#   )

# Function to summarize a site's data
summarize_site <- function(df, site_name, flow_min, flow_max) {
  df <- df %>%
    filter(!is.na(frac_change), !is.na(Flow_Inst)) %>%
    mutate(
      above_10pct = frac_change > 0.10,
      within_range = Flow_Inst >= flow_min & Flow_Inst <= flow_max,
      above_10pct_in_range = above_10pct & within_range
    )
  
  total_points <- nrow(df)
  above_10pct <- sum(df$above_10pct)
  below_10pct <- total_points - above_10pct
  above_10pct_in_range <- sum(df$above_10pct_in_range)
  
  tibble(
    site = site_name,
    total_points = total_points,
    above_10pct = above_10pct,
    below_10pct = below_10pct,
    above_10pct_in_range = above_10pct_in_range,
    pct_above_10pct = round(100 * above_10pct / total_points, 2),
    pct_below_10pct = round(100 * below_10pct / total_points, 2),
    pct_above_in_range = round(100 * above_10pct_in_range / total_points, 2)
  )
}

summary_barnum <- summarize_site(flows_Barnum_15min, "Barnum", 300, 2000)
summary_barton <- summarize_site(flows_Barton_15min, "Barton", 55.376, 369.173)
summary_kitzmiller <- summarize_site(flows_Kitzmiller_15min, "Kitzmiller", 253.76, 1691.73)

NB_plot_summary_table <- bind_rows(summary_barnum, summary_barton, summary_kitzmiller)
NB_plot_summary_table

write.csv(NB_pot_summary_table, "C:/Users/ben/Documents/NB_pot_summary_table.csv", row.names = FALSE)

#Cumulative Density Plot
ggplot(flow_all_15min, aes(x = frac_change, color = site)) +
  stat_ecdf(size = 1) +
  theme_minimal() +
  labs(x = "Fractional Change", y = "Cumulative Probability",
       title = "Cumulative Distribution of Fractional Change by Site")

ggplot(flow_all_15min, aes(x = frac_change)) +
  stat_ecdf(color = "steelblue", size = 1) +
  geom_vline(xintercept = 0.10, linetype = "dashed", color = "red") +
  facet_wrap(~site, scales = "free_y") +
  coord_cartesian(xlim = c(0, 0.2)) +
  theme_minimal() +
  labs(
    x = "Fractional Change",
    y = "Cumulative Probability",
    title = "Cumulative Distribution of Fractional Change by Site",
    caption = "Red dashed line = 10% threshold"
  )

high_flows_10pct %>%
  group_by(site) %>%
  summarise(min = min(frac_change),
            q25 = quantile(frac_change, 0.25),
            median = median(frac_change),
            q75 = quantile(frac_change, 0.75),
            q90 = quantile(frac_change, 0.90),
            max = max(frac_change))


site_limits <- high_flows_10pct %>%
  group_by(site) %>%
  summarise(
    xmin = min(frac_change),
    xmax = quantile(frac_change, 0.9)
  )

# Kitzmiller
ggplot(filter(high_flows_10pct, site == "Kitzmiller"), aes(x = frac_change)) +
  stat_ecdf(color = "blue", size = 1) +
  coord_cartesian(xlim = c(0.1, 0.6)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "Kitzmiller CDF (0.1–0.6 range)"
  )

# Barnum
ggplot(filter(high_flows_10pct, site == "Barnum"), aes(x = frac_change)) +
  stat_ecdf(color = "red", size = 1) +
  coord_cartesian(xlim = c(0.1, 0.6)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "Barnum CDF (0.1–0.6 range)"
  )

# Barton
ggplot(filter(high_flows_10pct, site == "Barton"), aes(x = frac_change)) +
  stat_ecdf(color = "darkgreen", size = 1) +
  coord_cartesian(xlim = c(0.1, 0.3)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "Barton CDF (0.1–0.3 range)"
  )


#CDF of All
ggplot(
  high_flows_10pct %>% 
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "CDF of Fractional Change for All Sites (Linear Scale)",
    color = "Site"
  )

#Log CDF
ggplot(
  high_flows_10pct %>% 
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%) (log scale)",
    y = "Cumulative Probability",
    title = "CDF of Fractional Change for All Sites (Log Scale)",
    color = "Site"
  )



#2D Plots
ggplot(flow_all_15min, aes(x = Flow_Inst, y = flow_diff)) +
  geom_bin2d(bins = 100) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~site) +
  labs(x = "Flow (cfs)", y = "Flow Difference (cfs)",
       title = "2D Density of Flow vs Flow Difference by Site") +
  theme_minimal()

ggplot(flow_all_15min, aes(x = Flow_Inst, y = flow_diff)) +
  geom_density_2d_filled(alpha = 0.7) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~site)

ggplot(flow_all_15min %>%
         filter(!is.na(Flow_Inst), !is.na(flow_diff),
                Flow_Inst > 0, flow_diff > 0),
       aes(x = Flow_Inst, y = flow_diff)) +
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.8) +
  scale_x_log10() + scale_y_log10() +
  coord_cartesian(ylim = c(0.1, 100)) +
  facet_wrap(~site) +
  labs(
    x = "Flow (cfs)",
    y = "Flow Difference (cfs)",
    title = "Smoothed 2D Density of Flow vs Flow Difference by Site (Y ≤ 100)"
  ) +
  theme_minimal()

#Normalizing x-axis

high_flows_norm <- high_flows_10pct %>%
  mutate(norm_change = flow_diff / Flow_Inst)

ggplot(high_flows_norm, aes(x = Flow_Inst, y = norm_change,
                            color = site, fill = site)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0.10, linetype = "dashed") +  # flattened 10% line
  scale_x_log10() +
  labs(
    title = "Normalized Flow Change vs Flow (flow_diff / Flow_Inst)",
    x = "Instantaneous Flow (cfs, log scale)",
    y = "Normalized Change (flow_diff / Flow_Inst)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

ggplot(high_flows_norm, aes(x = norm_change, color = site, fill = site)) +
  geom_density(alpha = 0.25) +
  geom_vline(data = medians, aes(xintercept = med_flow, color = site), linetype = "dashed") +
  labs(
    title = "Distribution of Normalized Flow Changes",
    x = "flow_diff / Flow_Inst",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

ggplot(high_flows_norm, aes(x = norm_change, color = site, fill = site)) +
  geom_density(alpha = 0.25) +
  geom_vline(data = medians, aes(xintercept = med_flow, color = site), linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Distribution of Normalized Flow Changes (log scale)",
    x = "flow_diff / Flow_Inst (log10)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

high_flows_norm <- high_flows_norm %>%
  mutate(norm_change_adj = norm_change + 1e-6)

ggplot(high_flows_norm, aes(x = norm_change_adj, color = site, fill = site)) +
  geom_density(alpha = 0.25) +
  scale_x_log10() +
  labs(
    title = "Distribution of Normalized Flow Changes (log scale)",
    x = "flow_diff / Flow_Inst (log10, adjusted)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)

plot_slice_norm <- function(site_name, flow_min, flow_max, pt_color = "black") {
  
  high_flows_10pct %>%
    filter(site == site_name,
           Flow_Inst >= flow_min,
           Flow_Inst <= flow_max) %>%
    mutate(norm_diff = flow_diff / Flow_Inst) %>%   # <-- NEW y variable
    
    ggplot(aes(x = Flow_Inst, y = norm_diff)) +
    geom_point(color = pt_color, alpha = 0.5, size = 2) +
    scale_x_log10() +
    geom_hline(yintercept = 0.10, linetype = "dashed") +   # flattened 10% line
    labs(
      title = paste0(site_name, ": Normalized Flow Diff vs Flow (",
                     flow_min, "–", flow_max, " cfs)"),
      x = "Instantaneous Flow (cfs, log10 scale)",
      y = "Normalized Diff (flow_diff / Flow_Inst)"
    ) +
    theme_minimal(base_size = 13)
}

plot_kitzmiller <- plot_slice_norm("Kitzmiller", 253.760, 1691.73, "blue") +
  coord_cartesian(ylim = c(0, 2))

plot_barnum <- plot_slice_norm("Barnum", 300, 2000, "red") +
  coord_cartesian(ylim = c(0, 2))

plot_barton <- plot_slice_norm("Barton", 55.376, 369.173, "green") +
  coord_cartesian(ylim = c(0, 2))


library(patchwork)
plot_barnum / plot_kitzmiller / plot_barton

#PDF of Fractional Change
high_flows_norm <- high_flows_10pct %>%
  mutate(frac_change = flow_diff / Flow_Inst)

ggplot(high_flows_norm, aes(x = frac_change, color = site, fill = site)) +
  geom_density(alpha = 0.25) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  labs(
    title = "PDF of Fractional Flow Increases (>10% events) Hourly",
    x = "Fractional Change (flow_diff / Flow_Inst)",
    y = "Density"
  ) +  scale_x_log10()+
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())+ scale_y_continuous(limits = c(0, 4))


#Seasonal cdf:
library(lubridate)

high_flows_10pct <- high_flows_10pct %>%
  mutate(dateTime = parse_date_time(dateTime,
                                    orders = c("ymd HMS", "ymd"),
                                    tz = "UTC"))

high_flows_10pct <- high_flows_10pct %>%
  mutate(
    season = case_when(
      month(dateTime) %in% c(12, 1, 2)  ~ "Winter",
      month(dateTime) %in% c(3, 4, 5)   ~ "Spring",
      month(dateTime) %in% c(6, 7, 8)   ~ "Summer",
      TRUE                              ~ "Fall"
    ),
    season = factor(season,
                    levels = c("Winter", "Spring", "Summer", "Fall"))
  )


ggplot(
  high_flows_10pct %>% 
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  facet_wrap(~ season) +
  coord_cartesian(xlim = c(0, 0.6)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "CDF of Fractional Change by Season and Site",
    color = "Site"
  )


ggplot(
  high_flows_10pct %>% 
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  scale_x_log10() +
  facet_wrap(~ season) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%) (log scale)",
    y = "Cumulative Probability",
    title = "Log-Scale CDF of Fractional Change by Season",
    color = "Site"
  )

# --- Spawning Season Analysis (Mid-Oct to Mid-Nov) ---

# Filter for spawning window: Oct 15 – Nov 15
spawning_high_flows <- high_flows_10pct %>%
  mutate(dateTime = parse_date_time(dateTime,
                                    orders = c("ymd HMS", "ymd"),
                                    tz = "UTC")) %>%
  filter(
    (month(dateTime) == 10 & day(dateTime) >= 15) |
      (month(dateTime) == 11 & day(dateTime) <= 15)
  )

# --- PDF of Fractional Change During Spawning Season ---
ggplot(
  spawning_high_flows %>%
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site, fill = site)
) +
  geom_density(alpha = 0.25) +
  geom_vline(xintercept = 0.10, linetype = "dashed") +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Density",
    title = "PDF of Fractional Flow Increases During Spawning Season (Oct 15 – Nov 15) Hourly",
    color = "Site",
    fill = "Site"
  )+ scale_y_continuous(limits = c(0, 4))

# --- CDF of Fractional Change During Spawning Season (Linear) ---
ggplot(
  spawning_high_flows %>%
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%)",
    y = "Cumulative Probability",
    title = "CDF of Fractional Flow Increases During Spawning Season (Oct 15 – Nov 15)",
    color = "Site"
  )

# --- CDF of Fractional Change During Spawning Season (Log Scale) ---
ggplot(
  spawning_high_flows %>%
    filter(site %in% c("Kitzmiller", "Barnum", "Barton")),
  aes(x = frac_change, color = site)
) +
  stat_ecdf(size = 1) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "Fractional Change (>10%) (log scale)",
    y = "Cumulative Probability",
    title = "Log-Scale CDF of Fractional Flow Increases During Spawning Season (Oct 15 – Nov 15)",
    color = "Site"
  )

# --- Quick summary of spawning high_flow counts per site ---
spawning_high_flows %>%
  group_by(site) %>%
  summarise(
    n = n(),
    median_frac_change = median(frac_change, na.rm = TRUE),
    q90_frac_change    = quantile(frac_change, 0.90, na.rm = TRUE)
  )

#scatter plot with translucent points below 10% threshold
flow_all_15min <- flow_all_15min %>%
  mutate(above_10pct = factor(
    ifelse(is.na(frac_change) | frac_change <= 0.10, "FALSE", "TRUE"),
    levels = c("FALSE", "TRUE")
  ))

# Scatterplot with Translucent Points Below
site_colors <- c(
  "Kitzmiller" = "#6495ED", 
  "Barnum"     = "#B23A48",  
  "Barton"     = "#5E8C61"   
)

ggplot() +
  geom_point(
    data = flow_all_15min %>% filter(flow_diff > 0, Flow_Inst > 0, above_10pct == "FALSE"),
    aes(x = Flow_Inst, y = flow_diff, color = site),
    size = 0.8, alpha = 0.03
  ) +
  geom_point(
    data = flow_all_15min %>% filter(flow_diff > 0, Flow_Inst > 0, above_10pct == "TRUE"),
    aes(x = Flow_Inst, y = flow_diff, color = site),
    size = 0.8, alpha = 0.6
  ) +
  scale_color_manual(values = site_colors) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope = 1, intercept = log10(0.1),
              linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = "Flow Difference vs. Instantaneous Flow (all points)",
    x     = "Instantaneous Flow (cfs, log10 scale)",
    y     = "Flow Difference (cfs, log10 scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title    = element_blank(),
    legend.text     = element_text(size = 14),
    legend.key.size = unit(1.5, "cm")
  )

# Locating hydropeaking event
sample_events <- flow_all_15min %>%
  filter(above_10pct == "TRUE") %>%
  filter(site == "Barnum") %>% 
  arrange(desc(frac_change)) %>%
  slice(1:10)  # top 10 biggest jumps

sample_events %>% select(site, dateTime, Flow_Inst, flow_diff, frac_change)

plot_event_window <- function(df, event_time, site_name, before = 36, after = 36) {
  
  site_data <- df %>% filter(site == site_name) %>% arrange(dateTime)
  
  # Find the index of the event
  event_idx <- which(site_data$dateTime == event_time)
  
  # Grab the window
  window <- site_data %>%
    slice((event_idx - before):(event_idx + after)) %>%
    mutate(is_event = dateTime == event_time)
  
  ggplot(window, aes(x = dateTime, y = Flow_Inst)) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(color = is_event), size = 3) +
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    labs(
      title = paste0(site_name, ": Event at ", event_time),
      subtitle = paste0("Window: ", before*15, " min before to ", after*15, " min after"),
      x = "Time",
      y = "Discharge (cfs)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}

event <- 5

# Plot the event
plot_event_window(flow_all_15min, 
                  event_time = sample_events$dateTime[event],
                  site_name  = sample_events$site[event])

plot_event_window(flow_all_15min,
                  event_time = as.POSIXct("2007-08-15 17:00:00", tz = "UTC"),
                  site_name  = "Barnum",
                  before = 96,   # 24 hours before
                  after  = 96)   # 24 hours after

plot_event_window(flow_all_15min,
                  event_time = as.POSIXct("2019-06-04 15:15:00", tz = "UTC"),
                  site_name  = "Barnum",
                  before = 96,
                  after  = 96)

plot_event_window(flow_all_15min,
                  event_time = as.POSIXct("2007-08-15 17:00:00", tz = "UTC"),
                  site_name  = "Barnum",
                  before = 672,   # 1 week before
                  after  = 672)   # 1 week after
#Better hydropeaking methods
flow_all_15min <- flow_all_15min %>% 
  arrange(site, dateTime) %>% 
  group_by(site) %>% 
  mutate(
    # Rolling max over a window — if current value equals local max, it's a peak
    roll_max = rollapply(Flow_Inst, width = 13, FUN = max, 
                         fill = NA, align = "center"),
    is_peak = Flow_Inst == roll_max,
    
    # Pre and post slopes around each peak
    pre_slope  = Flow_Inst - lag(Flow_Inst, 6),   # rise over previous 90 min
    post_slope = lead(Flow_Inst, 6) - Flow_Inst,  # fall over next 90 min
    
    asymmetry = pre_slope / abs(post_slope)
  )

hydropeak_candidates <- flow_all_15min |>
  filter(
    site == "Barnum",
    is_peak == TRUE,
    pre_slope > 0,        # was rising
    post_slope < 0,       # then falling
    asymmetry > 2         # rose faster than it fell
  )


hydropeak_candidates <- hydropeak_candidates |>
  arrange(site, dateTime) |>
  group_by(site) |>
  mutate(
    time_since_last = as.numeric(difftime(dateTime, lag(dateTime), units = "mins")),
    new_event = is.na(time_since_last) | time_since_last > 180  # 3 hr gap = new event
  ) |>
  filter(new_event)  # keep only the first detection per cluster)

#pdf
pdf("hydropeak_review.pdf", width = 8, height = 4)

for (i in seq_len(nrow(hydropeak_candidates))) {
  
  event     <- hydropeak_candidates[i, ]
  site_data <- flow_all_15min |> filter(site == "Barnum")
  
  window_data <- site_data |>
    filter(dateTime >= event$dateTime - 3*3600,
           dateTime <= event$dateTime + 3*3600)
  
  p <- ggplot(window_data, aes(x = dateTime, y = Flow_Inst)) +
    geom_line() +
    geom_point(size = 1.5, color = "gray40") +
    geom_point(data = event, color = "red", size = 3) +
    labs(
      title = paste0(event$site, ": Event at ", event$dateTime),
      subtitle = paste0("Asymmetry: ", round(event$asymmetry, 2),
                        "  |  Rise: ", round(event$pre_slope, 1), " cfs"),
      x = "Time", y = "Discharge (cfs)"
    ) +
    theme_minimal()
  
  print(p)
}

dev.off()

class(hydropeak_candidates$dateTime)
class(flow_all_15min$dateTime)

attr(hydropeak_candidates$dateTime, "tzone")
attr(flow_all_15min$dateTime, "tzone")

event <- hydropeak_candidates[1, ]
flow_all_15min |>
  filter(site == "Barnum",
         dateTime >= event$dateTime - 3*3600,
         dateTime <= event$dateTime + 3*3600) |>
  nrow()

head(flows_Barnum$dateTime)

#Shiny App

library(shiny)
library(ggplot2)
library(dplyr)

# Initialize a labels column
hydropeak_candidates$label <- NA_character_

ui <- fluidPage(
  titlePanel("Hydropeak Event Reviewer"),
  fluidRow(
    column(8, plotOutput("event_plot", height = "400px")),
    column(4,
           h4(textOutput("progress")),
           verbatimTextOutput("event_info"),
           br(),
           actionButton("btn_hydropeak", "✓ Hydropeaking",  
                        style = "color:white; background-color:#2196F3; width:100%; margin-bottom:8px"),
           actionButton("btn_artifact",  "✗ Artifact/Dropout",
                        style = "color:white; background-color:#f44336; width:100%; margin-bottom:8px"),
           actionButton("btn_flood",     "~ Flood Pulse",
                        style = "color:white; background-color:#4CAF50; width:100%; margin-bottom:8px"),
           actionButton("btn_skip",      "? Skip",
                        style = "width:100%; margin-bottom:8px"),
           br(),
           actionButton("btn_save", "💾 Save Progress")
    )
  )
)

server <- function(input, output, session) {
  
  idx     <- reactiveVal(1)
  labels  <- reactiveVal(rep(NA_character_, nrow(hydropeak_candidates)))
  
  current_event <- reactive({
    hydropeak_candidates[idx(), ]
  })
  
  output$progress <- renderText({
    paste0("Event ", idx(), " of ", nrow(hydropeak_candidates))
  })
  
  output$event_info <- renderText({
    e <- current_event()
    paste0("Site: ", e$site, "\n",
           "Time: ", e$dateTime, "\n",
           "Peak Q: ", round(e$Flow_Inst, 1), " cfs\n",
           "Asymmetry: ", round(e$asymmetry, 2), "\n",
           "Rise: ", round(e$pre_slope, 1), " cfs/90min")
  })
  
  output$event_plot <- renderPlot({
    e <- current_event()
    window_data <- flow_all_15min |>
      filter(site == e$site,
             dateTime >= e$dateTime - 3*3600,
             dateTime <= e$dateTime + 3*3600)
    
    ggplot(window_data, aes(x = dateTime, y = Flow_Inst)) +
      geom_line() +
      geom_point(size = 1.5, color = "gray40") +
      geom_point(data = e, aes(x = dateTime, y = Flow_Inst),
                 color = "red", size = 3) +
      labs(title = paste0(e$site, ": ", e$dateTime),
           subtitle = paste0("Asymmetry: ", round(e$asymmetry, 2)),
           x = "Time", y = "Discharge (cfs)") +
      theme_minimal()
  })
  
  # Button handlers
  label_and_advance <- function(lbl) {
    l <- labels()
    l[idx()] <- lbl
    labels(l)
    if (idx() < nrow(hydropeak_candidates)) idx(idx() + 1)
  }
  
  observeEvent(input$btn_hydropeak, label_and_advance("hydropeaking"))
  observeEvent(input$btn_artifact,  label_and_advance("artifact"))
  observeEvent(input$btn_flood,     label_and_advance("flood"))
  observeEvent(input$btn_skip,      label_and_advance(NA_character_))
  
  observeEvent(input$btn_save, {
    result <- hydropeak_candidates
    result$label <- labels()
    write.csv(result, "labeled_events.csv", row.names = FALSE)
    showNotification("Saved to labeled_events.csv", type = "message")
  })
}

shinyApp(ui, server)

event_time <- as.POSIXct("2006-10-05 16:45:00", tz = "UTC")

window_data <- flow_all_15min |>
  filter(site == "Barnum",
         dateTime >= event_time - 6*3600,
         dateTime <= event_time + 6*3600)

ggplot(window_data, aes(x = dateTime, y = Flow_Inst)) +
  geom_line() +
  geom_point(size = 1.5, color = "gray40") +
  geom_point(data = window_data |> filter(dateTime == event_time),
             color = "red", size = 3) +
  labs(
    title = "Barnum: 2006-10-05 16:45:00",
    subtitle = "Window: 6 hours before and after",
    x = "Time", y = "Discharge (cfs)"
  ) +
  theme_minimal()
