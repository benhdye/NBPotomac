#Calculate Fractional Changes

#Source
flows_Barnum <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum_15min.csv")
flows_Barton <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton_15min.csv")
flows_Kitzmiller <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller_15min.csv")

# Calculate Fractional Change
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

#export
write.csv(flows_Kitzmiller_15min, "flows_Kitzmiller_15min.csv", row.names = FALSE)
write.csv(flows_Barton_15min, "flows_Barton_15min.csv", row.names = FALSE)
write.csv(flows_Barnum_15min, "flows_Barnum_15min.csv", row.names = FALSE)
