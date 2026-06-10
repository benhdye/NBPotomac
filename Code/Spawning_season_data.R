#Spawning Season

high_flows_10pct <- read_csv("https://media.githubusercontent.com/media/benhdye/NBPotomac/refs/heads/main/CSVs/high_flows_10pct.csv")

spawning_high_flows <- high_flows_10pct %>%
  mutate(dateTime = parse_date_time(dateTime,
                                    orders = c("ymd HMS", "ymd"),
                                    tz = "UTC")) %>%
  filter(
    (month(dateTime) == 10 & day(dateTime) >= 15) |
      (month(dateTime) == 11 & day(dateTime) <= 15)
  )

write.csv(spawning_high_flows, "spawning_high_flows.csv", row.names = FALSE)
