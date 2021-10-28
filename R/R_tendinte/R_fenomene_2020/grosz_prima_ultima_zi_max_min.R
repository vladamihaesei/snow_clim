library(climatetools)
library(dplyr)
#options(warn = 2) 
nebt <- read.csv("tab/NEBT_ora_6_1961_2019.csv")
t1 <- read.csv("tab/GROSZ_1961_2019.csv")
numeorig <- names(t1)[3]
colnames(t1)[3] <- "ZAPADA"
t1 <- t1 %>% left_join(nebt, by = c("COD", "DAT"))
### validare nebulozitate
t1$ZAPADA[is.na(t1$NEBT)] <- NA # 
t1$ZAPADA[t1$NEBT >= 0 && is.na(t1$ZAPADA)] <- 0 # asta e bine

###
head(t1)
summary(t1)
# elimina Fundulea
t1 <- t1 %>% filter(COD != 428632) 
# elimina Sulina pentru strat de zapada
t1 <- t1 %>% filter(COD != 509940) 
t1 <- na.omit(t1)
summary(t1)
t1$DAT <- as.Date(t1$DAT)
cods <- unique(t1$COD)
dww <- NULL
for (i in 1:length(cods)) {
  print(ws$NUME[ws$CODGE %in% cods[i]])
  t2 <- t1[t1$COD == cods[i],]
  ani <- unique(as.integer(format(t2$DAT,"%Y")))
  zfi <- NULL
  for (j in 1:(length(ani) - 1)) {
    
    print(ani[j])
    # selecteaza anul decalat
    
    t3 <- t2 %>% dplyr::filter(DAT %in% seq(as.Date(paste0(ani[j],"-08-01")), as.Date(paste0(ani[j+1],"-07-31")), "days"))
 
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
      dif_ultima = ultima - as.numeric(format(t3$DAT[t3$julian == ultima], "%j"))
      
      
    )
    
    zfi<- rbind(zfi,cb)
  } 
  
  prima.zi.med <- round(mean(zfi$prima_zi_jul_decalat) + (mean(zfi$dif_prima[zfi$dif_prima>0])))
  prima.zi.med.dat <- as.Date(prima.zi.med, origin = "2018-12-31")
  prima.zi.max.dat <- zfi$prima_zi_dat[which.max(zfi$prima_zi_jul_decalat)]
  prima.zi.min.dat <- zfi$prima_zi_dat[which.min(zfi$prima_zi_jul_decalat)]
  
  ultima.zi.med <- round(mean(zfi$ultima_zi_jul_decalat) - (mean(zfi$dif_ultima[zfi$dif_ultima>0])))
  ultima.zi.med.dat <- as.Date(ultima.zi.med, origin = "2018-12-31")
  ultima.zi.max.dat <- zfi$ultima_zi_dat[which.max(zfi$ultima_zi_jul_decalat)]
  ultima.zi.min.dat <- zfi$ultima_zi_dat[which.min(zfi$ultima_zi_jul_decalat)]
  
  di <- data.frame(nume = ws$NUME[ws$CODGE %in% cods[i]], cod = cods[i], 
                   prima.zi.med = format(prima.zi.med.dat, "%b%d"),  prima.zi.max = format(prima.zi.max.dat, "%Y%b%d"),
                   prima.zi.min = format(prima.zi.min.dat, "%Y%b%d"),ultima.zi.med = format(ultima.zi.med.dat, "%b%d"),
                   ultima.zi.max = format(ultima.zi.max.dat, "%Y%b%d"), ultima.zi.min = format(ultima.zi.min.dat, "%Y%b%d"),)
  dww <- rbind(dww,di)
  
}

dww <- dww[order(dww$nume),]
dww
write.csv(dww,  paste0("tab_export/prima_ultima_zi_fen_",numeorig,"_1961_2019_365_zile_v113",".csv"), row.names=F)
