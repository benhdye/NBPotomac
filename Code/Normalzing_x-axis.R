#Normalizng X Axis
library(tidyverse)

high_flows_10pct <- read_csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/refs/heads/main/CSVs/high_flows_10pct.csv")

#Normalizing x-axis

high_flows_norm <- high_flows_10pct %>%
  mutate(norm_change = flow_diff / Flow_Inst)

high_flows_norm <- high_flows_norm %>%
  mutate(norm_change_adj = norm_change + 1e-6)

write.csv(high_flows_norm, "high_flows_norm.csv", row.names = FALSE)

