library(dplyr)
library(tidyverse)
library(climatetools)
library(magick)
library(RColorBrewer)
library(cowplot)
library(ggthemes)
library(lubridate)

t1 <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")
t1$dat <- as.Date(t1$dat)
ani <- as.numeric(sort(unique(year(t1$dat))))
 
d <- NULL

for ( i in 1:length(ani)){
   
   nr_statii <- t1 %>%filter(year(dat) == ani[i])%>%na.omit()%>%distinct(NUME)%>%nrow()
   t.df <- as.data.frame(nr_statii)
   t.df$Ani <- ani[i]
   d <- rbind(t.df,d)
 }

