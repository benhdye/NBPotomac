
flows_Barnum <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum.csv")
flows_Barton <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton.csv")
flows_Kitzmiller <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller.csv")

flows_Barnum_15min <- flows_Barnum %>%
mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
filter(time_diff == 15) %>%
mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

flows_Kitzmiller_15min <- flows_Kitzmiller %>%
mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
filter(time_diff == 15) %>%
mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

flows_Barton_15min <- flows_Barton %>%
mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
filter(time_diff == 15) %>%
mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)

filter_15min_intervals <- function(df) {
df %>%
mutate(time_diff = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
filter(time_diff == 15) %>%
mutate(flow_diff = lead(Flow_Inst) - Flow_Inst)
}
flows_Kitzmiller_15min <- filter_15min_intervals(flows_Kitzmiller)
flows_Barnum_15min     <- filter_15min_intervals(flows_Barnum)
flows_Barton_15min     <- filter_15min_intervals(flows_Barton)

#Write csvs
write.csv(flows_Kitzmiller_15min, "flows_Kitzmiller_15min.csv", row.names = FALSE)
write.csv(flows_Barton_15min, "flows_Barton_15min.csv", row.names = FALSE)
write.csv(flows_Barnum_15min, "flows_Barnum_15min.csv", row.names = FALSE)
write.csv(flow_all_15min, "flows_all_15min.csv", row.names = FALSE)

