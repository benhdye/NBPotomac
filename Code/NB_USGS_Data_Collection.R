library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(dataRetrieval)

#pullintervaldata(instantaneousvalues)


flows_Kitzmiller<-readNWISuv(
  siteNumbers="01595500",
  parameterCd="00060",
  startDate="2003-10-01",
  endDate="2025-09-17"
  )%>%renameNWISColumns()%>%
  mutate(flow_diff=lead(Flow_Inst)-Flow_Inst)

flows_Barnum<-readNWISuv(
  siteNumbers="01595800",
  parameterCd="00060",
  startDate="2003-10-01",
  endDate="2025-09-17"
  )%>%renameNWISColumns()%>%
  mutate(flow_diff=lead(Flow_Inst)-Flow_Inst)

flows_Barton<-readNWISuv(
  siteNumbers="01596500",
  parameterCd="00060",
  startDate="2003-10-01",
  endDate="2025-09-17"
  )%>%renameNWISColumns()%>%
  mutate(flow_diff=lead(Flow_Inst)-Flow_Inst)

#write as .csv files
write.csv(flows_Kitzmiller, "flows_Kitzmiller.csv", row.names = FALSE)

write.csv(flows_Barton, "flows_Barton.csv", row.names = FALSE)

write.csv(flows_Barnum, "flows_Barnum.csv", row.names = FALSE)
