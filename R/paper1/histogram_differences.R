library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)

t <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/prima_ultima_zi_fen_GROSZ_each_year_1961-2020_koeppen.csv")
t$prima_zi_dat <- as.Date(t$prima_zi_dat)

t.f <- t%>% filter(nume == "Botoșani")

tt <- t %>% mutate(Ani = year(prima_zi_dat))%>% group_by(nume, cod, Alt, Lat, Lon,Category,Criter1,Criter2,Criter3)%>%
  summarise(pzi.med1 = mean(prima_zi_jul_decalat[Ani>= 1961 &Ani<=1991]),
            pzi.med2 = mean(prima_zi_jul_decalat[Ani>= 1991 &Ani<=2020]),
            FSC = pzi.med2 - pzi.med1,
            uzi.med1 = mean(ultima_zi_jul_decalat[Ani>= 1961 &Ani<=1991]),
            uzi.med2 = mean(ultima_zi_jul_decalat[Ani>= 1991 &Ani<=2020]),
            LSC = uzi.med2 - uzi.med1,
            int.med1 = mean(interval_zile[Ani>= 1961 &Ani<=1991]),
            int.med2 = mean(interval_zile[Ani>= 1991 &Ani<=2020]),
            SCD = int.med2 - int.med1)
tt.l <- tt %>% pivot_longer(-c(nume,cod,Alt,Lat,Lon,Category,Criter1,Criter2,Criter3,pzi.med1,pzi.med2,uzi.med1,uzi.med2,int.med1,int.med2), names_to = "indicator")


p <- ggplot(tt.l, aes(value)) +
  geom_histogram(
    breaks = seq(-10, 16, by = 2.), 
   fill = "#66aad7", color = "black", size = 0.4, show.legend = F) +
  geom_vline(xintercept = 0, color = "black", size = 1 )+
  scale_x_continuous(breaks = seq(-10, 20, by=5))+
 
  annotate(geom="text", x=-8, y=24, label="Early",
           color="black", size =4)+
  annotate(geom="text", x=12, y=24, label="Late",
           color="black", size = 4)+
  theme_bw()+
  xlab("days") +ylab("Number of stations") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11))+
  facet_wrap(~indicator)

png(filename = paste0(drive_z,"png/articol1/histogram_fsc_lsc_scd_diferences.png"), width = 1600, height = 1050, res = 250)
p
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/histogram_fsc_lsc_scd_diferences.png  ", drive_z, "png/articol1/histogram_fsc_lsc_scd_diferences.png"))

#### FSC 
fsc <- ggplot(tt, aes(FSC)) +
  geom_histogram(
    breaks = seq(-10, 16, by = 2.), 
    fill = "#66aad7", color = "black", size = 0.4, show.legend = F) +
  geom_vline(xintercept = 0, color = "black", size = 1 )+
  scale_x_continuous(breaks = seq(-10, 20, by=5))+
  
  annotate(geom="text", x=-8, y=22, label="Early",
           color="#2067ac", size =4)+ #"#2067ac"
  annotate(geom="text", x=12, y=22, label="Late",
           color="#b1182c", size = 4)+ #"#b1182c"
  theme_bw()+
  xlab("days") +ylab("Number of stations") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11))

png(filename = paste0(drive_z,"png/articol1/histogram_fsc_diferences.png"), width = 1400, height = 1250, res = 250)
fsc
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/histogram_fsc_diferences.png  ", drive_z, "png/articol1/histogram_fsc_diferences.png"))
#### LSC 
lsc <- ggplot(tt, aes(LSC)) +
  geom_histogram(
    breaks = seq(-10, 16, by = 2.), 
    fill = "#66aad7", color = "black", size = 0.4, show.legend = F) +
  geom_vline(xintercept = 0, color = "black", size = 1 )+
  scale_x_continuous(breaks = seq(-10, 20, by=5))+
  
  annotate(geom="text", x=-8, y=22, label="Early",
           color="#b1182c", size =4)+ #"#2067ac"
  annotate(geom="text", x=12, y=22, label="Late",
           color="#2067ac", size = 4)+ #"#b1182c"
  theme_bw()+
  xlab("days") +ylab("Number of stations") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11))

png(filename = paste0(drive_z,"png/articol1/histogram_lsc_diferences.png"), width = 1400, height = 1250, res = 250)
lsc
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/histogram_lsc_diferences.png  ", drive_z, "png/articol1/histogram_lsc_diferences.png"))

#### SCD

scd <- ggplot(tt, aes(SCD)) +
  geom_histogram(
    breaks = seq(-10, 16, by = 2.), 
    fill = "#66aad7", color = "black", size = 0.4, show.legend = F) +
  geom_vline(xintercept = 0, color = "black", size = 1 )+
  scale_x_continuous(breaks = seq(-10, 20, by=5))+
  
  annotate(geom="text", x=-8, y=22, label="Shorter",
           color="#2067ac", size =4)+ #"#2067ac"
  annotate(geom="text", x=12, y=22, label="Longer",
           color="#b1182c", size = 4)+ #"#b1182c"
  theme_bw()+
  xlab("days") +ylab("Number of stations") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11))
png(filename = paste0(drive_z,"png/articol1/histogram_scd_diferences.png"), width = 1400, height = 1250, res = 250)
scd
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/histogram_scd_diferences.png  ", drive_z, "png/articol1/histogram_scd_diferences.png"))


