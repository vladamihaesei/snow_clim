library(climatetools)
library(dplyr)
#options(warn = 2) 
tmin <- read.csv("tab/TMIN_1961_2019.csv")
tmin <- tmin[1:3]
#nebt <- read.csv("tab/NEBT_daily_1961_2019.csv")# nebulozitate
nebt <- read.csv("tab/NEBT_ora_6_1961_2019.csv")
#colnames(nebt)[1] <- "DAT"
#tmin[4] <- NULL
t1 <- read.csv("tab/BRUMA_1961_2019.csv")
numeorig <- names(t1)[3]

colnames(t1)[3]<- "ZAPADA"
t1 <- t1 %>% left_join(nebt, by = c("COD", "DAT")) %>% left_join(tmin, by = c("COD", "DAT"))
head(t1)

t1$ZAPADA[is.na(t1$NEBT)] <- NA # asta trebuie corectata
t1$ZAPADA[t1$NEBT >= 0 && is.na(t1$ZAPADA)] <- 0 # asta e bine
# pentru validare cu temperatura
t1$ZAPADA[!is.na(t1$TMIN) & t1$TMIN >=5] <- 0 
head(t1)
summary(t1)
# elimina Fundulea
t1 <- t1 %>% filter(COD != 428632) 
t1 <- na.omit(t1)
summary(t1)
t1$DAT <- as.Date(t1$DAT)

cods <- unique(t1$COD)
dww <- NULL

for (i in 1:length(cods)) {
  
  # i <- which(cods %in% ws$CODGE[ws$NUME == "Piatra Neamt"])
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
    
    t3 <- t3 %>%  mutate(julian = format(DAT, "%j"))
    
    # nu calcula daca nu ai fenomen in anul respectiv
    if (max(t3$ZAPADA) == 0) next
    
    # alege prima si ultima zi
    prima <- t3 %>% filter(format(DAT, "%m") >= "08") %>% 
      summarise(prima.zi = ifelse(max(ZAPADA) == 0, NA, as.character(DAT[min(which(ZAPADA > 0))])),
                prima.zi.julian = ifelse(max(ZAPADA) == 0, NA, julian[min(which(ZAPADA > 0))]))
    
    ultima <- t3 %>% filter(format(DAT, "%m") < "08") %>% 
      summarise(ultima.zi = ifelse(max(ZAPADA) == 0, NA, as.character(DAT[max(which(ZAPADA > 0))])),
                ultima.zi.julian = ifelse(max(ZAPADA) == 0, NA, julian[max(which(ZAPADA > 0))]))
    
    cb <-  cbind(prima, ultima)

    zfi <- rbind(zfi,cb)
  } 
  
  
  prima.zi.med <- zfi %>% mutate(prima.zi.julian = as.numeric(prima.zi.julian)) %>% summarize(prima.med = mean(prima.zi.julian, na.rm = T)) %>%
    unlist() %>% round()
  prima.zi.med.dat <- as.Date(prima.zi.med, origin = "2018-12-31")
  prima.zi.max.dat <- zfi$prima.zi[which.max(zfi$prima.zi.julian)] %>% as.Date()
  prima.zi.min.dat <- zfi$prima.zi[which.min(zfi$prima.zi.julian)] %>% as.Date()

  ultima.zi.med <- zfi %>% mutate(ultima.zi.julian = as.numeric(ultima.zi.julian)) %>% summarize(ulitma.med = mean(ultima.zi.julian, na.rm = T)) %>%
                    unlist() %>% round()
  ultima.zi.med.dat <- as.Date(ultima.zi.med, origin = "2018-12-31")
  ultima.zi.max.dat <- zfi$ultima.zi[which.max(zfi$ultima.zi.julian)] %>% as.Date()
  ultima.zi.min.dat <- zfi$ultima.zi[which.min(zfi$ultima.zi.julian)] %>% as.Date()
  
  di <- data.frame(nume = ws$NUME[ws$CODGE %in% cods[i]], cod = cods[i], 
                   prima.zi.med = format(prima.zi.med.dat, "%b%d"),  prima.zi.max = format(prima.zi.max.dat, "%Y%b%d"),
                   prima.zi.min = format(prima.zi.min.dat, "%Y%b%d"),ultima.zi.med = format(ultima.zi.med.dat, "%b%d"),
                   ultima.zi.max = format(ultima.zi.max.dat, "%Y%b%d"), ultima.zi.min = format(ultima.zi.min.dat, "%Y%b%d"),)
  dww <- rbind(dww,di)
  
}

dww <- dww[order(dww$nume),]
dww
#write.csv(dww,  paste0("tabs/bruma_prima_ultima_zi_1961_2019_365_zile.csv"), row.names=F)
write.csv(dww,  paste0("tab_export/prima_ultima_zi_fen_",numeorig,"_1961_2019_365_zile_v112",".csv"), row.names=F)
