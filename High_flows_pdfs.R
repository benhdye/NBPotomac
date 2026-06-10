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

