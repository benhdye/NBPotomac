library(grwat)
library(dplyr)
library(tidyr)

flows_Barton_daily <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton_daily.csv") %>% 
  rename(
    Date = Date,
    Flow = X_00060_00003
  )

flows_Barnum_daily <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum_daily.csv") %>% 
  rename(
    Date = Date,
    Flow = X_00060_00003
  )

flows_Kitzmiller_daily <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller_daily.csv") %>% 
  rename(
    Date = Date,
    Flow = X_00060_00003
  )

flows_Kitzmiller_daily$Date <- as.Date(flows_Kitzmiller_daily$Date)

flows_Barton_daily$Date <-as.Date(flows_Barton_daily$Date)


flows_Barnum_daily$Date <- as.Date(flows_Barnum_daily$Date)

flows_Kitzmiller_daily$Site <- "Kitzmiller"
flows_Barnum_daily$Site     <- "Barnum"
flows_Barton_daily$Site     <- "Barton"

#Calculate annual flow

Kitzmiller_annual_flow <- flows_Kitzmiller_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site, Year) %>%
  summarise(
    AAF = mean(Flow, na.rm = TRUE),
    n_days = sum(!is.na(Flow)),
    .groups = "drop"
  )

Barton_annual_flow <- flows_Barton_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site, Year) %>%
  summarise(
    AAF = mean(Flow, na.rm = TRUE),
    n_days = sum(!is.na(Flow)),
    .groups = "drop"
  )

Barnum_annual_flow <- flows_Barnum_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site, Year) %>%
  summarise(
    AAF = mean(Flow, na.rm = TRUE),
    n_days = sum(!is.na(Flow)),
    .groups = "drop"
  )

#Calculate Baseflow

bf_Kitzmiller <- gr_baseflow(
  Q = flows_Kitzmiller_daily$Flow,
  method = "Eckhardt",
)

bf_Barnum <- gr_baseflow(
  Q = flows_Barnum_daily$Flow,
  method = "Eckhardt",
)

bf_Barton <- gr_baseflow(
  Q = flows_Barton_daily$Flow,
  method = "Eckhardt",
)

#attach to DF

flows_Kitzmiller_daily$Baseflow <- bf_Kitzmiller
flows_Barnum_daily$Baseflow     <- bf_Barnum
flows_Barton_daily$Baseflow     <- bf_Barton

flows_Kitzmiller_daily <- flows_Kitzmiller_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  left_join(Kitzmiller_annual_flow, by = c("Site", "Year"))

flows_Barnum_daily <- flows_Barnum_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  left_join(Barnum_annual_flow, by = c("Site", "Year"))

flows_Barton_daily <- flows_Barton_daily %>%
  mutate(Year = lubridate::year(Date)) %>%
  left_join(Barton_annual_flow, by = c("Site", "Year"))

#Create Habitat Classes on BF%

flows_Kitzmiller_daily <- flows_Kitzmiller_daily %>%
  mutate(
    BF_percent_AAF = 100 * Baseflow / AAF,
    Habitat_Class = case_when(
      BF_percent_AAF >= 50 ~ "Great",
      BF_percent_AAF >= 25 ~ "Fair",
      BF_percent_AAF < 25  ~ "Poor",
      TRUE ~ NA_character_
    )
  )

flows_Barnum_daily <- flows_Barnum_daily %>%
  mutate(
    BF_percent_AAF = 100 * Baseflow / AAF,
    Habitat_Class = case_when(
      BF_percent_AAF >= 50 ~ "Great",
      BF_percent_AAF >= 25 ~ "Fair",
      BF_percent_AAF < 25  ~ "Poor",
      TRUE ~ NA_character_
    )
  )

flows_Barton_daily <- flows_Barton_daily %>%
  mutate(
    BF_percent_AAF = 100 * Baseflow / AAF,
    Habitat_Class = case_when(
      BF_percent_AAF >= 50 ~ "Great",
      BF_percent_AAF >= 25 ~ "Fair",
      BF_percent_AAF < 25  ~ "Poor",
      TRUE ~ NA_character_
    )
  )

#Analyze Habitat Classes
flows_all_daily <- bind_rows(
  flows_Kitzmiller_daily,
  flows_Barnum_daily,
  flows_Barton_daily
)

habitat_summary <- flows_all_daily %>%
  group_by(Site, Habitat_Class) %>%
  summarise(
    Days = n(),
    .groups = "drop"
  ) %>%
  group_by(Site) %>%
  mutate(
    Percent = 100 * Days / sum(Days)
  )

flows_all_daily <- flows_all_daily %>%
  mutate(
    Month = lubridate::month(Date),
    Winter_Year = ifelse(Month == 12,
                         lubridate::year(Date) + 1,
                         lubridate::year(Date))
  )

winter_summary <- flows_all_daily %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Site, Habitat_Class) %>%
  summarise(
    Days = n(),
    .groups = "drop"
  ) %>%
  group_by(Site) %>%
  mutate(
    Percent = 100 * Days / sum(Days)
  )

winter_yearly_summary <- flows_all_daily %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Site, Winter_Year, Habitat_Class) %>%
  summarise(Days = n(), .groups = "drop") %>%
  group_by(Site, Winter_Year) %>%
  mutate(
    Percent = 100 * Days / sum(Days)
  )

#Visualization

ggplot(winter_yearly_summary,
       aes(x = Winter_Year,
           y = Percent,
           fill = Habitat_Class)) +
  geom_col() +
  facet_wrap(~ Site, ncol = 1) +
  scale_fill_manual(values = c(
    "Great" = "#2E8B57",
    "Fair"  = "#FDB863",
    "Poor"  = "#D73027"
  )) +
  labs(
    x = "Winter Year",
    y = "Percent of Winter Days",
    fill = "Habitat Class",
    title = "Winter Baseflow Habitat Classification"
  ) +
  theme_minimal()


