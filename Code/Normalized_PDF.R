#Normalized PDF
library(tidyverse)

high_flows_10pct <- read_csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/refs/heads/main/CSVs/high_flows_10pct.csv")

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
