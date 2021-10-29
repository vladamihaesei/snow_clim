library(ggplot2)
library(dplyr)
library(tidyr)
library(climatetools)

source("R/cale_legenda_vectors.R")
## verificare numar statii 
statii <- read.csv(paste0(drive_z,"tab/statii.csv"))
statii <- statii %>% filter(COD == 428632)

statii_cr <- read.csv(paste0(drive_z,"tab/tavg_ws_inventory_selection.csv"))
statii.j <- ws[c(2,3,4,5,6,7,8,9)]%>% left_join(statii_cr)
###########################################################
###########################################################

tmin <- read.csv(paste0(drive_z,"tab/TMIN_1961_2019.csv")) #temp minima 
tmin <- tmin[1:3]
nebt <- read.csv(paste0(drive_z,"tab/NEBT_ora_6_1961_2019.csv"))
t <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))
names(ws)[4] <- "COD"
t1 <- t %>% left_join(nebt, by = c("COD", "DAT")) %>% left_join(tmin, by = c("COD", "DAT"))%>% left_join(ws[c(2,3,4,5,7,8,9)])
### validare cu nebulozitate
t1$GROSZ[is.na(t1$NEBT)] <- NA # 
t1$GROSZ[t1$NEBT >= 0 & is.na(t1$ZAPADA)] <- 0 # asta e bine
t1.n <- na.omit(t1)
t1.n$DAT <- as.Date(t1.n$DAT)
t1.month <- t1.n %>% group_by(COD,CMR,JU,NUME,Lat,Lon,Z, format(DAT,"%Y-%m")) %>% summarise(
                                                                                           mean_with_zero = mean(GROSZ),
                                                                                           ))

t1.month <- na.omit(t1.month)

t1.n  <- t1.n[c(1,2,3,5,8,9,10,11)] %>% pivot_wider(c(COD, DAT,NUME, Lat, Lon, Z), values_from = c(GROSZ), names_from = "GROSZ" )
t1.n

