library(dplyr)
library(zoo)
library(tidyverse)
library(tidyr)
library(seas)


zap_2019 <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))
zap_2019 <-zap_2019 %>% filter(COD != 428632) 
zap_2019 <- zap_2019 %>% filter(COD != 509940) 

cods <- unique(zap_2019$COD)


prec <- read.csv(paste0(drive_z, "tab/PREC_TOTL_1961-2020.csv"))
tmed <- read.csv(paste0(drive_z, "tab/TEMP_MDL_1961-2020.csv"))
grosz <- read.csv(paste0(drive_z, "tab/GROSZ_lunare_1961-202107.csv"))

list <- list(prec,tmed,grosz)

for (i in 1:length(list)){
  
  tab <- list[[i]]
  t1.f <- tab%>% filter(COD %in% cods)%>%na.omit()
  t1.f$DAT <- as.Date(t1.f$DAT)
  nume <- names(t1.f)[3]
  names(t1.f) <- c("CODGE", "DAT", "value" )
  p <- t1.f %>% filter(year(DAT) > 1960 && year(DAT) < 2021)%>%left_join(ws.prov[c(3,4,5,6,8,9,10,13,14,15,16)])%>%
    group_by(ex.category, Criter2) %>% summarise(Media = round(mean(value),2))
  
  write.csv(p, paste0(drive_z, "tab_export/",nume, "_aggregate_koeppen_1961-2020.csv"))
  
}

#### find the number of station by elevation and koeppen classification 

statii.f <- ws.prov%>% filter(CODGE %in% cods)

statii_nr  <-  statii.f%>%mutate(frec = case_when(ex.category == "BSk"~1,
                                                                            ex.category == "Cfb"~1,
                                                                            ex.category == "Cfa"~1,
                                                                            ex.category == "Dfc"~1,
                                                                            ex.category == "Dfb"~1,
                                                                            ex.category == "ET"~1
                                                                            ))%>% group_by(ex.category,Criter2)%>% summarise(Nr = sum(frec))
statii_nr










