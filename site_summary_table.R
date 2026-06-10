#Site Summary

source("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/summarize_site_function.R")
read_csv

flows_Barnum_15min <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum_15min.csv")
flows_Barton_15min <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton_15min.csv")
flows_Kitzmiller_15min <- read_csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller_15min.csv")

summary_barnum <- summarize_site(flows_Barnum_15min, "Barnum", 300, 2000)
summary_barton <- summarize_site(flows_Barton_15min, "Barton", 55.376, 369.173)
summary_kitzmiller <- summarize_site(flows_Kitzmiller_15min, "Kitzmiller", 253.76, 1691.73)

NB_plot_summary_table <- bind_rows(summary_barnum, summary_barton, summary_kitzmiller)
NB_plot_summary_table

write.csv(NB_plot_summary_table, "C:/Users/ben/Documents/NB_plot_summary_table.csv", row.names = FALSE)
