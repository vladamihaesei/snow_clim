library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

ws <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/ws_statii_114_koeppen.csv")

cods <- unique(ws$CODGE)

sc1 <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/GROSZ_1961_2020.csv")
sc1$DAT <- as.Date(sc1$DAT)
names(sc1)[1] <- "CODGE"

snow.cover1 <- sc1 %>%filter(CODGE %in% cods)%>% na.omit()%>% 
  mutate(seas = mkseas(DAT, width = "DJF"),ani = year(DAT), caz = ifelse(GROSZ >= 1, 1,0))%>% 
  group_by(seas,ani,CODGE)%>% 
  summarise(Suma = sum(caz)) %>% left_join(ws[c(5,6,8,9,10,13,14,15,16)])


snow.cover2 <- sc1 %>%filter(CODGE %in% cods)%>% na.omit()%>% 
  mutate(seas = "anual",ani = year(DAT),caz = ifelse(GROSZ >= 1, 1,0))%>% 
  group_by(seas,ani,CODGE)%>%
  summarise(Suma = sum(caz))%>%left_join(ws[c(5,6,8,9,10,13,14,15,16)])

scd <- rbind(snow.cover1,snow.cover2)

write.csv(scd,"~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/snow_cover_days_1961-2020_season_anual.csv", row.names = F)




