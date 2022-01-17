library(dplyr)
library(sf)
library(trend)
library(climatetools)
library(tidyverse)
library(tidyr)
library(zoo)
library(lubridate)
library(seas)

files <- list.files("~/Y/climpact/climadapt_filled_1961_2020", recursive = T, full.names = T, pattern = ".csv")
files <- grep("tnm|tmm|txm", files, value = T)
files <- grep("MON", files, value = T)

indice <- c("tnm", "tmm","txm")

for (l in 1:length(indice)){
  
    print(indice[l])
    
    files.sub <- grep(indice[l], files, value =T)
    
    for (i in 1:length(files.sub)){
      
      t1 <- read.csv(files[i], skip = 6)
      nume <- strsplit(files.sub[i],"/|_|.csv")[[1]][10]
      print(nume)
      t1$time <- paste0(t1$time,"-01")
      t1$time <- as.Date(t1$time)
      names(t1)[2] <- "value"
  
      t1.m <- t1 %>%mutate(
        seas = mkseas(time, width = "DJF"))%>%
        group_by(seas) %>% # we group by name and cod to perform the calculation in each station
        summarise(slope.interval = sens.slope(value)$estimates *10,
                  sign.interval =  mk.test(value)$p.value)
      
      t1.df <- data.frame (denst = nume, t1.m)
      
    }
    
    write.csv(t1.df, paste0(drive_z,"tab_export/trend_",indice[l],"_1961-2020.csv"), row.names = F)
    
}
