library(openair)
library(tidyverse)
library(dplyr)
library(lubridate)

c <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")

c$dat <- as.Date(c$dat)

c$date <- as.POSIXct(c$dat)

c.f <- c %>%filter(NUME == "Vaslui")

c.f$grosz[c.f$grosz == 0] <- NA
            

ani <- unique(year(c.f$date))

for(a in 1:length(ani)){
  
  png(filename = paste0("~/D/2021/Date_doctorat/Zapada_doctorat/png/calendar_plot/","calendar_" ,ani[a],".png"), width = 1500, height = 1600,res = 220)
  cv <- calendarPlot(c.f, pollutant = "grosz", year = ani[a], 
                     annotate = "value", 
                     lim = 15,
                     cols = "Purples", 
                     col.lim = c("black",  "orange"))
  dev.off()
  
  system(paste0("convert -trim ~/D/2021/Date_doctorat/Zapada_doctorat/png/calendar_plot/calendar_",ani[a],".png","  ~/D/2021/Date_doctorat/Zapada_doctorat/png/calendar_plot/calendar_",ani[a],".png"))
  
}

