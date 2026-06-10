#CDFs

flows_all_15min <- read.csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/main/CSVs/flows_all_15min.csv")

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
