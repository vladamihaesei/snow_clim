library(dplyr)
library(tidyverse)
library(climatetools)

zap_2019 <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))

zap_2019 <-zap_2019 %>% filter(COD != 428632) 
zap_2019 <- zap_2019 %>% filter(COD != 509940) 

vf <- read.csv(paste0( drive_z,"tab/statii_zapada_plus.csv"))
vf$CODGE[is.na(vf$CODGE)] <- 552548
vf <- vf %>%filter(Verif  %in% c("da", "incert"))
cods.1 <- unique(vf$CODGE)
cods <- unique(zap_2019$COD)
cods.t <- c(cods,cods.1)

t <- read.csv(paste0(drive_z,"tab/GROSZ_1880_2020.csv"))
d <- NULL
anit <- seq(1961,2021,by = 1)

for ( i in 1:length(cods.t)){
  
  t1.f <- t%>% filter(cod == cods.t[i])%>%na.omit()

  t1.f$dat <- as.Date(t1.f$dat)
  t1.f <- t1.f %>% filter(year(dat) > 1960)
  
  ani <- unique(year(t1.f$dat))
  an.mis <- anit[!anit %in% ani]
  an.miss <- paste(an.mis[1],an.mis[2],an.mis[3],an.mis[4],an.mis[5],
                   an.mis[6],an.mis[7],an.mis[8],an.mis[9],an.mis[10], 
                   an.mis[11],an.mis[12],an.mis[13],an.mis[14],an.mis[15],sep = ",")

  df <- data.frame(nume = unique(t1.f$denst), cod = unique(t1.f$cod), start_year = min(ani), end_year = max(ani),
                    nr_ani_lipsa = 61-length(ani),ani.lipsa = an.miss, lungime = length(ani))
  d <- rbind(d,df)
}

d.f <- d%>%filter(lungime >=60)


t <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/GROSZ_lunare_1961-202107.csv")
colnames(t) <- tolower(colnames(t))

d1 <- NULL
anit <- seq(1961,2021,by = 1)

for ( i in 1:length(cods.t)){
  
  t1.f <- t%>% filter(cod == cods.t[i])%>%na.omit()
  colnames(t1.f)[1] <- "CODGE"
  t1.ff <- t1.f%>%left_join(ws[4:5])
  t1.ff$dat <- as.Date(t1.ff$dat)
  t1.ff <- t1.ff %>% filter(year(dat) > 1960)
  
  ani <- unique(year(t1.ff$dat))
  an.mis <- anit[!anit %in% ani]
  an.miss <- paste(an.mis[1],an.mis[2],an.mis[3],an.mis[4],an.mis[5],
                   an.mis[6],an.mis[7],an.mis[8],an.mis[9],an.mis[10], 
                   an.mis[11],an.mis[12],an.mis[13],an.mis[14],an.mis[15],sep = ",")
  
  df <- data.frame( nume = unique(t1.ff$NUME), cod = unique(t1.ff$CODGE), start_year = min(ani),
                    end_year = max(ani),ani_lipsa = an.miss,nr_ani_lipsa = 61-length(ani), lungime = length(ani))
  d1 <- rbind(d1,df)
  
}

d1.f <- d1%>%filter(lungime >=60)




