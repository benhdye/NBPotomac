#scatter plot with translucent points below 10% threshold

flows_all_15min <- read.csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/main/CSVs/flows_all_15min.csv")


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
