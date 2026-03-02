library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

flows_Kitzmiller <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Kitzmiller.csv")

flows_Barnum <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barnum.csv")

flows_Barton <- read.csv("https://raw.githubusercontent.com/benhdye/NBPotomac/refs/heads/main/CSVs/flows_Barton.csv")

flows_Kitzmiller$dateTime <- as.POSIXct(
  flows_Kitzmiller$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
flows_Barton$dateTime <- as.POSIXct(
  flows_Barton$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
flows_Barnum$dateTime <- as.POSIXct(
  flows_Barnum$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

flows_Kitzmiller$Site <- "Kitzmiller"
flows_Barnum$Site     <- "Barnum"
flows_Barton$Site     <- "Barton"

flows_all <- bind_rows(
  flows_Kitzmiller,
  flows_Barnum,
  flows_Barton
)

colors <- c(
  "Kitzmiller" = "#6495ED", 
  "Barnum"     = "#B23A48",  
  "Barton"     = "#5E8C61"   
)

flows_all$dateTime <- as.POSIXct(
  flows_all$dateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

p <- ggplot(flows_all,
            aes(x = dateTime,
                y = Flow_Inst,
                color = Site)) +
  geom_line(linewidth = 0.6, alpha = 0.4) +
  scale_color_manual(values = colors) +
  labs(
    title = "Hydrographs 2003–2025",
    x = "Time",
    y = "Discharge (CFS, Instantaneous)"
  ) +
  theme_minimal()

 p
 
 p1 <- ggplot(flows_Kitzmiller,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Kitzmiller"],
             linewidth = 0.6) +
   labs(title = "Kitzmiller",
        x = NULL,
        y = "Discharge (CFS)") +
   theme_minimal()
 
 p2 <- ggplot(flows_Barnum,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Barnum"],
             linewidth = 0.6) +
   labs(title = "Barnum",
        x = NULL,
        y = "Discharge (CFS)") +
   theme_minimal()
 
 p3 <- ggplot(flows_Barton,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Barton"],
             linewidth = 0.6) +
   labs(title = "Barton",
        x = "Time",
        y = "Discharge (CFS)") +
   theme_minimal()
 
 grid.arrange(p1, p2, p3, ncol = 1)
 
 #Set y-axis same for all
 
 y_max <- max(
   flows_Kitzmiller$Flow_Inst,
   flows_Barnum$Flow_Inst,
   flows_Barton$Flow_Inst,
   na.rm = TRUE
 )
 
 y_min <- min(
   flows_Kitzmiller$Flow_Inst,
   flows_Barnum$Flow_Inst,
   flows_Barton$Flow_Inst,
   na.rm = TRUE
 )
 
 coord_cartesian(ylim = c(y_min, y_max))
 
 p1 <- ggplot(flows_Kitzmiller,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Kitzmiller"], linewidth = 0.6) +
   coord_cartesian(ylim = c(y_min, y_max)) +
   labs(title = "Kitzmiller",
        x = NULL,
        y = "Discharge (CFS)") +
   theme_minimal()
 
 p2 <- ggplot(flows_Barnum,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Barnum"], linewidth = 0.6) +
   coord_cartesian(ylim = c(y_min, y_max)) +
   labs(title = "Barnum",
        x = NULL,
        y = "Discharge (CFS)") +
   theme_minimal()
 
 p3 <- ggplot(flows_Barton,
              aes(x = dateTime, y = Flow_Inst)) +
   geom_line(color = colors["Barton"], linewidth = 0.6) +
   coord_cartesian(ylim = c(y_min, y_max)) +
   labs(title = "Barton",
        x = "Time",
        y = "Discharge (CFS)") +
   theme_minimal()
 
 grid.arrange(p1, p2, p3, ncol = 1)
 