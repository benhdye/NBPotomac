#High Flows PDFs

high_flows_10pct <- read_csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/refs/heads/main/CSVs/high_flows_10pct.csv")

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
