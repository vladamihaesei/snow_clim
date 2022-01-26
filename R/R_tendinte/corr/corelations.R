library(dplyr)
library(sf)
library(trend)
library(climatetools)
library(tidyverse)
library(tidyr)
library(zoo)
library(lubridate)
library(seas)
library(xlsx)

fen <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_GROSZ_prima_ultima_1961-2020.csv")
temp <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_TEMP_MDL_1961-2020.csv") 

cor <- fen %>% left_join(temp)

cor <- cor %>%  mutate(Criter1 = case_when(Z <= 500~ "≤ 500 m",
                                           Z > 500 & Z <= 1000 ~ "500-1000 m",
                                           Z > 1000 & Z <= 1500~ "1000-1500 m",
                                           Z > 1500 & Z <= 2000~ "1500-2000 m",
                                           Z > 2000 & Z <= 2500~ "2000-2500 m",
                                           Z > 2500~ "> 2500 m"))

h <- cor %>% group_by(seas)%>% summarise(rp = cor(slope.temp,slope.ultima))


# 
ggplot(cor) +
  aes(x = slope.ultima, y = slope.temp) +
  geom_point(colour = "#0c4c8a") +
  geom_smooth(method = lm)+
  theme_minimal()+facet_wrap(~seas)

########################
fen <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/prima_ultima_zi_fen_ZAPADA_each_year_1961-2020.csv")
temp <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/TEMP_MDMN_1961-2020.csv") 
temp$DAT <- as.Date(temp$DAT)

temp.s <- temp %>% na.omit()%>% mutate(
  seas = mkseas(DAT, width = "DJF"), ani = year(DAT) )%>% group_by(ani,seas,COD) %>% summarise(TEMP_MDMN = mean(TEMP_MDMN))

#temp.s <- temp.s%>%filter(seas == "MAM")
names(temp.s)[3] <- "cod"
cor <- fen %>% left_join(temp.s)

cor <- cor %>%  mutate(Criter1 = case_when(Alt <= 500~ "≤ 500 m",
                                           Alt > 500 & Alt <= 1000 ~ "500-1000 m",
                                           Alt > 1000 & Alt <= 1500~ "1000-1500 m",
                                           Alt > 1500 & Alt <= 2000~ "1500-2000 m",
                                           Alt > 2000 & Alt <= 2500~ "2000-2500 m",
                                           Alt > 2500~"> 2500 m"))

h1 <- cor %>% group_by(seas,Criter1)%>% summarise(rp = cor(ultima_zi_jul_decalat,TEMP_MDMN))

h1$rp <- round(h1$rp,3)
h1

write.xlsx(as.data.frame(h1), "~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/excels/corelati.xlsx")

ggplot(cor) +
  aes(x = ultima_zi_jul_corect, y = TEMP_MDMX) +
  geom_point(colour = Criter1) +
  geom_smooth(method = lm)+
  theme_minimal()+facet_wrap(~seas)


