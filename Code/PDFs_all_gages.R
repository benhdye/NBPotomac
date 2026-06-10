#Generate Probability Density Functions

#PDFs

flows_Barnum <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum.csv")
flows_Barton <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton.csv")
flows_Kitzmiller <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller.csv")

flows_Barnum <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum_15min.csv")
flows_Barton <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton_15min.csv")
flows_Kitzmiller <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller_15min.csv")


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
