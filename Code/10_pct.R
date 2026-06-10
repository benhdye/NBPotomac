#10 Percentage
library(tidyverse)

flow_all_15min <- read_csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/refs/heads/main/CSVs/flows_all_15min.csv")

# 10% flows
high_flows_10pct <- flow_all_15min %>%
  filter(frac_change > 0.10) %>%
  drop_na(frac_change, Flow_Inst)
summary(high_flows_10pct)
table(high_flows_10pct$site)# Filter for flow increases greater than 10%

write.csv(high_flows_10pct, "high_flows_10pct.csv", row.names = FALSE)
