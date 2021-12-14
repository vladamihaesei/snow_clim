library(dplyr)
library(tidyverse)
library(climatetools)
library(magick)
library(RColorBrewer)
library(cowplot)
library(xlsx)
library(ggthemes)

t <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/GROSZ_lunare_1961-202107.csv")
colnames(t) <- tolower(colnames(t))
### 1949/3/22 Brasov eroare 200 
### elimina sulina 
t <- t %>%filter(cod!= 511849)



###
t$dat <- as.Date(t$dat)


tm <- t%>%na.omit()%>%filter(year(dat)>1960)%>%group_by(cod,year(dat))%>%summarise(groszm = mean(grosz))

colnames(tm)[2] <- "Ani"

t1.anom <- tm%>%group_by(cod)%>% mutate(zpd_61_90 = mean(groszm[Ani>= 1961 & Ani<=1990]),
                                        zpd_91_20 = mean(groszm[Ani>= 1991 & Ani<=2020]),
                                        anom_61_90 = groszm - zpd_61_90,
                                        anom_91_20 = groszm - zpd_91_20)


statii <- read.csv(file = "~/Y/monitorizare/MMOISE/extreme_zapada/ws_climatetools/ws_climatetools_provincii_NAomit.csv")
names(statii)[5] <- "cod"
t1.anom <- t1.anom %>%left_join(statii[c(3,5,6,8,9,10)])

t1.anom$den_alt <- paste(t1.anom$NUME,"(",round(t1.anom$Z,0),"m)",sep = "")

t1.anom.sub  <- t1.anom%>%filter(Z<1000)

t1.anom.sub <- t1.anom.sub%>%filter(NUME %in% c( "Calafat", "Craiova", "Giurgiu","Constanța","Drobeta-Turnu Severin","Pitești",
                                                "Ploiești","București-Filaret","Buzău","Tecuci","Tulcea","Arad","Cluj-Napoca","Piatra Neamț","Iași",
                                                "Oradea","Brașov","Bacău","Sibiu","Timișoara","Bistrița","Baia Mare","Suceava","Miercurea Ciuc",
                                                "Satu Mare","Alexandria","Vaslui","Râmnicu Sărat","Brăila", "Zimnicea" ,"Slatina"))



#
write.xlsx(as.data.frame(t1.anom.sub), "/home/vlad/Y/monitorizare/MMOISE/extreme_zapada/outputs/08_dec_2021/anomalii_1000m_campie.xlsx",row.names = F)
nume <- unique(t1.anom.sub%>%filter(Provincie == "Muntenia"))
nume <- unique(nume$den_alt)
nume1 <- unique(t1.anom.sub%>%filter(Provincie == "Oltenia"))
nume1 <- unique(nume1$den_alt)
nume2 <- unique(t1.anom.sub%>%filter(Provincie == "Moldova"))
nume2 <- unique(nume2$den_alt)

nume3 <- unique(t1.anom.sub%>%filter(Provincie == "Transilvania"))
nume3 <- unique(nume3$den_alt)

nume4 <- unique(t1.anom.sub%>%filter(Provincie == "Banat"))
nume4 <- unique(nume4$den_alt)
nume5 <- unique(t1.anom.sub%>%filter(Provincie == "Crișana"))
nume5 <- unique(nume5$den_alt)
nume6 <- unique(t1.anom.sub%>%filter(Provincie == "Maramureș"))
nume6 <- unique(nume6$den_alt)
nume7 <- unique(t1.anom.sub%>%filter(Provincie == "Dobrogea"))
nume7 <- unique(nume7$den_alt)

nume.tot <- rev(c(sort(nume),sort(nume1),sort(nume2),sort(nume3),sort(nume4),sort(nume5),sort(nume6),sort(nume7)))

#t1.anom.sub[order(unique(t1.anom.sub$NUME)),]

###
rmean <- colorRampPalette(brewer.pal(9,"RdBu"), interpolate="linear")
brks.mean <- seq(-16,16, 2)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-18, 18)
den <- "cm"

g <- ggplot(t1.anom.sub, 
            aes(Ani,factor(den_alt,levels = nume.tot), fill = anom)) +
  geom_tile( size = .35,color = "darkgrey") +
  # geom_tile(data= filter(t1.join.sub,mx =="mx" ),
  #           aes(Ani,NUME),
  #           size=1,
  #           colour = "darkgreen",
  #           fill = "transparent"
  # )+
  # geom_tile(data= filter(t1.join.sub,mn =="mn" ),
  #           aes(Ani,NUME),
  #           size=1,
  #           colour = "darkred",
  #           fill = "transparent"
# )+
# geom_text(data = t1.join.sub, aes(Ani,NUME, label = round(anom,1),
#                          fontface = "bold","plain",size = 2))+
scale_x_continuous(breaks = seq(1960,2020,by = 5), name = "")+
  # scale_fill_gradientn(name = "°C",
  #                      #limits = c(min(t2$anom),max(t2$anom)),
  #                      breaks = brks,
  #                      colors = cols)+
  
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "white",
  #   high = "red",
  #   midpoint = 0,
  #   space = "Lab",
#   breaks = c(-40,-20,0,20, 40),
#   labels = c("Foarte redus","Redus", "Normal", "Abundent", "Foarte abundent"),
#   na.value = "grey50",
#   guide = "colourbar"

#)+

scale_fill_stepsn(colours = cols.mean, name = den,
                  breaks = brks.mean,
                  limits =lim.mean) +
  guides(fill = guide_colourbar(barwidth = 40.0, barheight = 0.8, title.position = "right",
                                label.theme = element_text(size = 13.))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        title = element_text(size= 13),
        legend.title = element_text( size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.title =   element_text(size = 12, face = "bold"),
        #axis.text = element_text(size = 15),
        #panel.grid.major.x = element_line(colour = "white", linetype = "dashed"),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.ontop = TRUE,
        plot.margin = margin(r = 1, unit = "cm"))+
  
  labs(y = "",
       x = "",
       fill = "",
       title = "",
       caption = "") +
  # guides(fill = guide_colorbar(barwidth = 10,
  #                               barheight = .5,
  #                              label.position = "top",
  #                             ticks = FALSE)) +
  coord_cartesian(expand = FALSE)

logo_file <- image_read("~/D/2019/vldZ/R_meteo/Sigle/meteo_romania.jpg")
my_plot_3 <- ggdraw() +
  draw_plot(g)+
  #draw_image(logo_file, x= 0.06,hjust = 0.06, halign= 0.06, valign= 0.03, scale = 0.093) 
  draw_image(logo_file, x= 0.12,y =0.0899, hjust = 0.04, vjust= 0.99, halign= 0.04, valign= .99, scale = 0.061) 

png("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie_complete_61_90.png", width =2800, height = 2200, res =220 )
my_plot_3
dev.off()
system(paste0("convert -trim ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie_complete_61_90.png  ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie_complete_61_90.png"))



##############
t1.anom.sub <- t1.anom%>%filter(Z>1000)

unique(t1.anom.sub$den_alt)

nume.munte <- rev(c( "Iezer(1785m)","Vf. Omu(2504m)","Lăcăuți(1776m)",
                "Sinaia 1500(1510m)","Fundata(1384m)","Predeal(1090m)","Parâng(1548m)",
                "Păltiniș(1453m)","Țarcu(2180m)","Cuntu(1456m)","Semenic(1432m)","Vlădeasa 1800(1836m)","Băișoara(1360m)","Călimani(2022m)" ,"Bucin(1282m)","Penteleu(1632m)","Bâlea Lac(2070m)","Roșia Montană(1196m)","Stâna de Vale(1108m)", "Ceahlău Toaca(1897m)"
                ))

write.xlsx(as.data.frame(t1.anom.sub), "/home/vlad/Y/monitorizare/MMOISE/extreme_zapada/outputs/08_dec_2021/anomalii_1000m_munte.xlsx",row.names = F)

t1.anom.sub <- t1.anom.sub%>%filter(den_alt %in% c(nume.munte))
###"Călimani(2022m)" ,"Bucin(1282m)""Penteleu(1632m)","Bâlea Lac(2070m)",Roșia Montană(1196m)","Stâna de Vale(1108m)""Obârșia Lotrului(1348m)", "Ceahlău Toaca(1897m)",
###
rmean <- colorRampPalette(brewer.pal(9,"RdBu"), interpolate="linear")
brks.mean <- seq(-40,40, 5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-45, 45)
den <- "cm"

g <- ggplot(t1.anom.sub, 
            aes(Ani,factor(den_alt,levels = nume.munte), fill = anom1)) +
  geom_tile( size = .35,color = "darkgrey") +
  # geom_tile(data= filter(t1.join.sub,mx =="mx" ),
  #           aes(Ani,NUME),
  #           size=1,
  #           colour = "darkgreen",
  #           fill = "transparent"
  # )+
  # geom_tile(data= filter(t1.join.sub,mn =="mn" ),
  #           aes(Ani,NUME),
  #           size=1,
  #           colour = "darkred",
  #           fill = "transparent"
# )+
# geom_text(data = t1.join.sub, aes(Ani,NUME, label = round(anom,1),
#                          fontface = "bold","plain",size = 2))+
scale_x_continuous(breaks = seq(1960,2020,by = 5), name = "")+
  # scale_fill_gradientn(name = "°C",
  #                      #limits = c(min(t2$anom),max(t2$anom)),
  #                      breaks = brks,
  #                      colors = cols)+
  
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "white",
  #   high = "red",
  #   midpoint = 0,
  #   space = "Lab",
#   breaks = c(-40,-20,0,20, 40),
#   labels = c("Foarte redus","Redus", "Normal", "Abundent", "Foarte abundent"),
#   na.value = "grey50",
#   guide = "colourbar"

#)+

scale_fill_stepsn(colours = cols.mean, name = den,
                  breaks = brks.mean,
                  limits =lim.mean) +
  guides(fill = guide_colourbar(barwidth = 40.0, barheight = 0.8, title.position = "right",
                                label.theme = element_text(size = 13.))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        title = element_text(size= 13),
        legend.title = element_text( size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.title =   element_text(size = 12, face = "bold"),
        #axis.text = element_text(size = 15),
        #panel.grid.major.x = element_line(colour = "white", linetype = "dashed"),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.ontop = TRUE,
        plot.margin = margin(r = 1, unit = "cm"))+
  
  labs(y = "",
       x = "",
       fill = "",
       title = "",
       caption = "") +
  # guides(fill = guide_colorbar(barwidth = 10,
  #                               barheight = .5,
  #                              label.position = "top",
  #                             ticks = FALSE)) +
  coord_cartesian(expand = FALSE)

logo_file <- image_read("~/D/2019/vldZ/R_meteo/Sigle/meteo_romania.jpg")
my_plot_3 <- ggdraw() +
  draw_plot(g)+
  #draw_image(logo_file, x= 0.06,hjust = 0.06, halign= 0.06, valign= 0.03, scale = 0.093) 
  draw_image(logo_file, x= 0.1,y =0.1, hjust = 0.04, vjust= 0.99, halign= 0.04, valign= .99, scale = 0.065) 
png("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_munte_v2_91_20.png", width =2400, height = 1200, res =200 )
my_plot_3
dev.off()
system(paste0("convert -trim ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_munte_v2_91_20.png  ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_munte_v2_91_20.png"))








