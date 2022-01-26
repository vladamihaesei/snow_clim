library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

ws <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/ws_statii_114_koeppen.csv")

cods <- unique(ws$CODGE)

sc1 <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/GROSZ_1961_2020.csv")
sc1$DAT <- as.Date(sc1$DAT)
names(sc1)[1] <- "CODGE"
snow.cover1 <- sc1 %>%filter(CODGE %in% cods)%>% na.omit()%>% 
  mutate(seas = mkseas(DAT, width = "DJF"), ani = year(DAT), luni = month(DAT), caz = ifelse(GROSZ >= 1, 1,0))%>% 
  group_by(ani,seas,CODGE)%>% 
  summarise(Suma = sum(caz)) %>% left_join(ws[c(5,6,8,9,10,13,14,15,16)])

snow.cover1

write.csv(snow.cover1,"~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/snow_cover_days_1961-2020.csv", row.names = F)

#########

scd <- snow.cover1%>% filter(seas != "JJA") %>% 
     group_by(seas,CODGE, NUME, Lat,Lon,Z,ex.category,Criter1,Criter2,Criter3)%>%
     summarise(Mean = mean(Suma))
      
#write.csv(scd,"~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/scd_1961-2020_koeppen.csv", row.names = F)

p.sc <- ggplot(scd, aes(x=Mean, y=Z, colour = ex.category)) + 
  geom_point()+
  scale_colour_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_wrap(~factor(seas, levels = c("SON", "DJF", "MAM")), ncol = 2)+
  xlab("Number of days")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = c(.82,.2))
png(filename = paste0(drive_z,"png/articol1/scd_vs_elevation.png"), width = 1600, height = 1050, res = 250)
p.sc
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/scd_vs_elevation.png  ", drive_z, "png/articol1/scd_vs_elevation.png"))





