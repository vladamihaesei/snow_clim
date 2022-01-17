library(openair)
library(tidyverse)
library(dplyr)
library(lubridate)

c <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")

c$dat <- as.Date(c$dat)

c$date <- as.POSIXct(c$dat)

c <- c %>% filter(cod != 413839)

c.ss <- c%>% filter(!is.na(NUME))            

nume <- paste0(unique(c.ss$cod),"-",unique(c.ss$NUME))
cd <- unique(c.ss$cod)
for(b in 1:length(cd)){
  
  print(nume[b])
  c.f <- c %>% filter(cod == cd[b]) %>% na.omit()
  
  ani <- unique(year(c.f$date))
  cale <- paste0("~/D/2021/Date_doctorat/Zapada_doctorat/png/calendar_plot/",nume[b],"/")
  if(!dir.exists(cale)) dir.create(cale)
  
  for(a in 1:length(ani)){
    
    png(filename = paste0(cale,ani[a],".png"), width = 1500, height = 1600,res = 220)
    calendarPlot(c.f, pollutant = "grosz", year = ani[a], 
                       annotate = "value", 
                       lim = 15,
                       cols = "Purples", 
                       col.lim = c("black",  "orange"))
    dev.off()
    
    system(paste0("convert -trim ",cale,ani[a],".png","  ",cale,ani[a],".png"))
    
  }

}


t1 <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")

t1$dat <- as.Date(t1$dat)

t1$date <- as.POSIXct(t1$dat)

statie_cod <- paste(unique(t1$cod), unique(t1$NUME),sep = "_")
statie <- unique(t1$cod)

for (s in 1:length(statie)){
       
  png(filename = paste0("~/D/2021/Date_doctorat/Zapada_doctorat/png/time_series/","timeseries_" ,statie_cod[s],".png"), width = 1500, height = 1600,res = 220)
  
  c.sub <- t1 %>% filter(cod == statie[s])
  c.sub$dat <- as.Date(c.sub$dat)
  c.sub$date <-  as.POSIXct(c.sub$dat)
  
  # if(nrow(c.sub < 10)) break
  tp  <- timePlot(c.sub, pollutant = "grosz", avg.time = "month")
  
  dev.off()
  system(paste0("convert -trim ~/D/2021/Date_doctorat/Zapada_doctorat/png/time_series/timeseries_",statie_cod[s],".png","  ~/D/2021/Date_doctorat/Zapada_doctorat/png/time_series/timeseries_",statie_cod[s],".png"))
  
  
}


