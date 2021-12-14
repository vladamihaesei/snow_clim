library(dplyr)
library(tidyverse)
library(climatetools)
library(magick)
library(RColorBrewer)
library(cowplot)
library(ggthemes)
library(lubridate)

t <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")
t$dat <- as.Date(t$dat)
t$grosz[t$grosz == 0] <- NA


tm <- t %>%na.omit()%>% filter(year(dat) > 1960)%>%group_by(year(dat),cod,Provincie,NUME, Lat,Lon,Z,criter1)%>%summarise(groszm = mean(grosz))

colnames(tm)[1] <- "Ani"

t1.anom <- tm%>%group_by(criter1,Provincie)%>%mutate(zpd_61_90 = mean(groszm[Ani>= 1961 & Ani<=1990]),
                                        zpd_91_20 = mean(groszm[Ani>= 1991 & Ani<=2020]),
                                        groszm = groszm,
                                        anom = groszm - zpd_61_90)

t1.anom.m <- t1.anom %>%group_by(Ani,criter1,Provincie)%>% summarise(zpd61.90 = mean(zpd_61_90),
                                                                   zpd61.90 = mean(zpd_61_90),
                                                                   groszm = mean(groszm))
 
 
                                                                   
                                                                   
t1.anom.m <- t1.anom.m %>% mutate(criter1 = ifelse(criter1 == "<1000","Statii sub 1000 m","Statii peste 1000 m"))                                                                 
t1.anom.m <- t1.anom.m%>%filter(criter1 == "Statii peste 1000 m")

t1.anom.m$data <- as.Date(paste0(t1.anom.m$Ani,"-01-01"))

p <- ggplot(t1.anom.m, aes(Ani, groszm)) +
  geom_bar(colour = "grey",fill = "blue", stat = "identity")+
  scale_x_continuous(breaks = seq(1960,2021,5))+
  theme_dark() +
  theme(
        text = element_text(size = 14),
        title = element_text(size= 15),
        legend.title = element_text( size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title =   element_text(size = 13, face = "bold"),
        #axis.text = element_text(size = 15),
        #panel.grid.major.x = element_line(colour = "white", linetype = "dashed"),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.ontop = TRUE,
        plot.margin = margin(r = 1, unit = "cm"))+
  
  labs(y = "[cm]",
       x = "",
       fill = "",
       title = "",
       caption = "") +
  # guides(fill = guide_colorbar(barwidth = 10,
  #                               barheight = .5,
  #                              label.position = "top",
  #                             ticks = FALSE)) +
  coord_cartesian(expand = FALSE)+
  facet_grid(Provincie~criter1 )
p
logo_file <- image_read("~/D/2019/vldZ/R_meteo/Sigle/meteo_romania.jpg")

my_plot_3 <- ggdraw() +
  draw_plot(p)+
  #draw_image(logo_file, x= 0.06,hjust = 0.06, halign= 0.06, valign= 0.03, scale = 0.093) 
  draw_image(logo_file, x= 0.045,y =0.05, hjust = 0.04, vjust= 0.99, halign= 0.04, valign= .99, scale = 0.057) 

png("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_time_series_1000m_munte.png", width =2000, height = 1700, res =200 )
my_plot_3
dev.off()
system(paste0("convert -trim ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_munte.png  ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_munte.png"))




