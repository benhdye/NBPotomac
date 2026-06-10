#Site Characteristic Summary Funciton

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