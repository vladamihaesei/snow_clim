library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot)

prec <- read.csv(paste0(drive_z, "tab/PREC_TOTL_1961-2020.csv"))
tmed <- read.csv(paste0(drive_z, "tab/TEMP_MDL_1961-2020.csv"))
grosz <- read.csv(paste0(drive_z, "tab/GROSZ_lunare_1961-202107.csv"))
statii_all <- read.csv(paste0(drive_z,"tab/ws_statii_koeppen.csv"))
statii_114 <-read.csv(paste0(drive_z,"tab/ws_statii_114_koeppen.csv"))
cods <- unique(statii_114$CODGE)
#### precipitatiii

prec.f <- prec%>% filter(COD %in% cods)%>%na.omit()
prec.f$DAT <- as.Date(prec.f$DAT)
nume <- names(prec.f)[3]
names(prec.f) <- c("CODGE", "DAT", "value" )
pp <- prec.f %>% filter(year(DAT) > 1960 && year(DAT) < 2021)%>%left_join(statii_all[c(5,6,8,9,10,13,14,15,16)])%>%
  mutate(season = mkseas(DAT, width = "DJF"), ani = year(DAT))%>%group_by(CODGE, NUME, ani, season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Suma = round(sum(value),2))
pp.m <- pp %>% group_by(CODGE, NUME,season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Media = mean(Suma))
head(pp.m)

pp.m <- pp.m %>% filter(season != "JJA")

p3 <- ggplot(pp.m, aes(x=Media, y=Z, colour = ex.category)) + 
  geom_point()+
  scale_colour_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_wrap(~factor(season, levels = c("SON", "DJF", "MAM")))+
  xlab("Precipitation amount [mm]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank())
png(filename = paste0(drive_z,"png/articol1/fig2_precipitation_vs_elevation.png"), width = 1400, height = 730, res = 220)
p3
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig2_precipitation_vs_elevation.png  ", drive_z, "png/articol1/fig2_precipitation_vs_elevation.png"))


## temperatura medie 
tmed.f <- tmed%>% filter(COD %in% cods)%>%na.omit()
tmed.f$DAT <- as.Date(tmed.f$DAT)
nume <- names(tmed.f)[3]
names(tmed.f) <- c("CODGE", "DAT", "value" )
tt <- tmed.f %>% filter(year(DAT) > 1960 && year(DAT) < 2021)%>%left_join(statii_all[c(5,6,8,9,10,13,14,15,16)])%>%
  mutate(season = mkseas(DAT, width = "DJF"), ani = year(DAT))%>%group_by(CODGE, NUME, ani, season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Med = round(mean(value),2))
tt.m <- tt %>% group_by(CODGE, NUME,season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Media = mean(Med))
head(tt.m)

tt.m <- tt.m %>% filter(season != "JJA")

p4 <- ggplot(tt.m, aes(x=Media, y=Z, colour = ex.category)) + 
  geom_point()+
  scale_colour_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_wrap(~factor(season, levels = c("SON", "DJF", "MAM")))+
  xlab("Air temperatire [°C]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank())
png(filename = paste0(drive_z,"png/articol1/fig2_temperature_vs_elevation.png"),width = 1400, height = 730, res = 220)
p4
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig2_temperature_vs_elevation.png  ", drive_z, "png/articol1/fig2_temperature_vs_elevation.png"))

## grosz 
grosz.f <- grosz%>% filter(COD %in% cods)%>%na.omit()
grosz.f$DAT <- as.Date(grosz.f$DAT)
nume <- names(grosz.f)[3]
names(grosz.f) <- c("CODGE", "DAT", "value" )
grz <- grosz.f %>% filter(year(DAT) > 1960 && year(DAT) < 2021)%>%left_join(statii_all[c(5,6,8,9,10,13,14,15,16)])%>%
  mutate(season = mkseas(DAT, width = "DJF"), ani = year(DAT))%>%group_by(CODGE, NUME, ani, season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Med = round(mean(value),2))
grz.m <- grz %>% group_by(CODGE, NUME,season, Lat, Lon, Z, ex.category, Criter2) %>% summarise(Media = mean(Med))
head(grz.m)

grz.m <- grz.m %>% filter(season != "JJA")

p5 <- ggplot(grz.m, aes(x=Media, y=Z, colour = ex.category)) + 
  geom_point()+
  scale_colour_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_wrap(~factor(season, levels = c("SON", "DJF", "MAM")))+
  xlab("Snow depth [cm]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank())
png(filename = paste0(drive_z,"png/articol1/fig2_snowdepth_vs_elevation.png"), width = 1400, height = 730, res = 220)
p5
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig2_snowdepth_vs_elevation.png  ", drive_z, "png/articol1/fig2_snowdepth_vs_elevation.png"))


