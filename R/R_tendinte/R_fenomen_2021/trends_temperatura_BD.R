library(dplyr)
library(sf)
library(trend)
library(climatetools)
library(tidyverse)
library(tidyr)
library(zoo)
library(lubridate)
library(seas)

zap_2019 <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))
zap_2019 <- zap_2019 %>% filter(COD != 428632) 
zap_2019 <- zap_2019 %>% filter(COD != 509940) 


cods <- unique(zap_2019$COD)
files <- list.files(paste0(drive_z,"tab"), recursive = T, full.names = T, pattern = "TEMP_")

indice <- c("TEMP_MDL", "TEMP_MDMN","TEMP_MDMX")

for (i in 1:length(indice)){
  
  print(indice[i])
  
  t1 <- read.csv(files[i])
  t1 <- t1%>%filter(COD %in% cods)
  
  t1$DAT <- as.Date(t1$DAT)
  names(t1) <- c("CODGE","DAT","value")
  
  t1.m <- t1 %>% na.omit()%>% mutate(
    seas = mkseas(DAT, width = "DJF"))%>%
    group_by(CODGE,seas) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.temp = sens.slope(value)$estimates *10,
              sign.temp =  mk.test(value)$p.value)
  
  write.csv(t1.m, paste0(drive_z,"tab_export/trend_",indice[i],"_1961-2020.csv"), row.names = F)
  
}



