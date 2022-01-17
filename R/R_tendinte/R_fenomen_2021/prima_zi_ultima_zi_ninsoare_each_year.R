library(climatetools)
library(dplyr)
source("R/cale_legenda_vectors.R")
#options(warn = 2) 
#nebt <- read.csv("tab/NEBT_daily_1961_2019.csv")# nebulozitate
#colnames(nebt)[1] <- "DAT"
#tmin[4] <- NULL

tmin <- read.csv(paste0(drive_z,"tab/TMIN_1961-2020.csv")) #temp minima 
tmin <- tmin[1:3]
nebt <- read.csv(paste0(drive_z,"tab/CNEBT_1961-2020.csv"))
zap_2019 <- read.csv(paste0(drive_z,"tab/ZAPADA_1961_2019.csv"))
zap_2019 <- zap_2019 %>% filter(COD != 428632)
t <- read.csv(paste0(drive_z,"tab/ZAPADA_1961-2020.csv"))
numeorig <- names(t1)[3]
colnames(t1)[3] <- "ZAPADA"

t1 <- t  %>% left_join(tmin, by = c("COD", "DAT"))

### validare cu nebulozitate
t1$ZAPADA[is.na(t1$CNEBT)] <- NA # 
t1$ZAPADA[t1$CNEBT >= 0 && is.na(t1$ZAPADA)] <- 0 # asta e bine
# pentru validare cu temperatura
t1$ZAPADA[!is.na(t1$TMIN) & t1$TMIN >=5] <- 0
###
head(t1)
summary(t1)
# elimina Fundulea
t1 <- t1 %>% filter(COD != 428632) 
#t1 <- t1[-c(2137439), ] # elimina data de 2012/mai24
t1 <- na.omit(t1)
summary(t1)
t1$DAT <- as.Date(t1$DAT)
cods <- unique(zap_2019$COD)

dww <- NULL

for (i in 1:length(cods)) {
  
  #print(ws$NUME[ws$CODGE %in% cods[i]])
  t2 <- t1[t1$COD == cods[i],]
  ani <- unique(as.integer(format(t2$DAT,"%Y")))
  zfi <- NULL
  
  for (j in 1:(length(ani) - 1)) {
    
    print(ani[j])
    # selecteaza anul decalat
    
    t3 <- t2 %>% dplyr::filter(DAT %in% seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days"))
    # elimina anul daca nu ai suficiente zile
    #if (nrow(t3) < 365) next
    # elimina anul daca nu ai suficiente zile în funcție de an (bisect)
    an.prag <- ifelse(lubridate::leap_year(ani[j]), 366, 365)
    if (nrow(t3) < an.prag) next
    t3 <- t3 %>% mutate(julian = seq(1:length(DAT)))
    
    # nu calcula daca nu ai fenomen in anul respectiv
    if (max(t3$ZAPADA) == 0) next
    
    # alege prima si ultima zi
    prima <- t3[min(which(t3$ZAPADA >0)),]$julian
    ultima <- t3[max(which(t3$ZAPADA >0)),]$julian
    
    cb <- data.frame (
      prima_zi_jul_decalat = prima, 
      prima_zi_jul_corect = as.numeric(format(t3$DAT[t3$julian == prima], "%j")),
      prima_zi_dat = t3$DAT[t3$julian == prima], 
      dif_prima = as.numeric(format(t3$DAT[t3$julian == prima], "%j")) - prima,
      ultima_zi_jul_decalat = ultima, 
      ultima_zi_jul_corect = as.numeric(format(t3$DAT[t3$julian == ultima], "%j")),
      ultima_zi_dat = t3$DAT[t3$julian == ultima],
      dif_ultima = ultima - as.numeric(format(t3$DAT[t3$julian == ultima], "%j")),
      interval_zile = ultima - prima
      
    )
    zfi <- rbind(zfi,cb)
  }
  di <- data.frame(nume = ws$NUME[ws$CODGE %in% cods[i]],
                   cod = cods[i],
                   Alt = ws$Z[ws$CODGE %in% cods[i]], 
                   Lat = ws$Lat[ws$CODGE %in% cods[i]],Lon = ws$Lon[ws$CODGE %in% cods[i]],
                   Start_year = min(ani),End_year = max(ani),Length_year = length(ani),
                   zfi)
  dww <- rbind(dww,di)
  
  
}

dww <- dww[order(dww$nume),]
dww

write.csv(dww,  paste0(drive_z,"tab_export/prima_ultima_zi_fen_",numeorig,"_each_year_1961-2020",".csv"), row.names=F)



