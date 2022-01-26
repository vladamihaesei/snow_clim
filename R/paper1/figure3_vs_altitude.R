library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot)

pu <- read.csv(paste0(drive_z, "tab_export/prima_ultima_zi_fen_GROSZ_eaach_year_1961-2020.csv"))

statii_all <- read.csv(paste0(drive_z,"tab/ws_statii_koeppen.csv"))
statii_114 <-read.csv(paste0(drive_z,"tab/ws_statii_114_koeppen.csv"))
cods <- unique(statii_114$CODGE)

#### precipitatiii
pu.f <- pu%>% filter(cod %in% cods)%>%na.omit()
names(pu.f)[2] <- "CODGE"
pu <- pu.f%>%left_join(statii_all[c(5,6,8,9,10,13,14,15,16)])%>%
           group_by(CODGE, nume, Alt, Lat,Lon,ex.category, Criter2) %>% summarise(FSD = round(mean(prima_zi_jul_decalat),0),
                                                                                  FSD.corect = round(mean(prima_zi_jul_corect),0),
                                                                                  LSD = round(mean(ultima_zi_jul_decalat),0),
                                                                                  LSD.corect = round(mean(ultima_zi_jul_corect),0),
                                                                                  SCD = round(mean(interval_zile),0))


#write.csv(pu, paste0(drive_z, "tab_export/fsd_lsd_scd_1961-2020_koeppen.csv"), row.names = F )
pu.l <- pu %>% pivot_longer(-c(CODGE,nume,Alt,Lat,Lon,ex.category, Criter2,FSD.corect, LSD.corect),
                            names_to = "indicator")
     
pui <- ggplot(pu.l, aes(x=value, y=Alt, colour = ex.category)) + 
  geom_point()+
  scale_colour_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_wrap(~factor(indicator, levels = c("FSD", "LSD", "SCD")), ncol = 2)+
  xlab("Julian Days")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = c(.82,.2))
png(filename = paste0(drive_z,"png/articol1/fig3_trends_vs_elevation.png"), width = 1600, height = 1050, res = 250)
pui
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig3_trends_vs_elevation.png  ", drive_z, "png/articol1/fig3_trends_vs_elevation.png"))

