library(dplyr)
library(tidyverse)
library(climatetools)
library(magick)
library(RColorBrewer)
library(cowplot)
library(ggthemes)

t <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")
# elimina Fundulea si Sulina


###
t$dat <- as.Date(t$dat)


t1 <- t %>%na.omit()%>% filter(year(dat) > 1960)%>%group_by(cod,year(dat))%>%summarise(groszm = mean(grosz))



statii <- read.csv(file = "~/Y/monitorizare/MMOISE/extreme_zapada/ws_climatetools/ws_climatetools_provincii_NAomit.csv")
names(statii)[5] <- "cod"
t1.join <- t1.anom %>%left_join(statii[c(3,5,6,8,9,10)])
colnames(t1.join)[2] <- "Ani"


colnames(t1)[2] <- "Ani"
t1.anom <- t1%>%group_by(cod)%>% mutate(zpd_61_90 = mean(groszm[Ani>= 1961 & Ani<=1990]),
                                        zpd_91_20 = mean(groszm[Ani>= 1991 & Ani<=2020]),
                                        anom = groszm - zpd_61_91)


###
rmean <- colorRampPalette(rev(brewer.pal(9,"RdBu")), interpolate="linear")
brks.mean <- seq(-35,35, 5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-40, 40)
den <- "cm"


g <- ggplot(t1.join.sub, 
            aes(Ani,den_alt, fill = anom)) +
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
        text = element_text(size = 13),
        title = element_text(size= 13),
        legend.title = element_text( size = 12),
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
       title = "Anomalia grosimii medii a stratului de zăpadă (cm)",
       caption = "Data: MeteoRomania\nPerioada de referință 1961-1991") +
  # guides(fill = guide_colorbar(barwidth = 10,
  #                               barheight = .5,
  #                              label.position = "top",
  #                             ticks = FALSE)) +
  coord_cartesian(expand = FALSE)


logo_file <- image_read("~/D/2019/vldZ/R_meteo/Sigle/meteo_romania.jpg")
my_plot_3 <- ggdraw() +
  draw_plot(g)+
  #draw_image(logo_file, x= 0.06,hjust = 0.06, halign= 0.06, valign= 0.03, scale = 0.093) 
  draw_image(logo_file, x= 0.065,y =0.13, hjust = 0.04, vjust= 0.99, halign= 0.04, valign= .99, scale = 0.077) 
png("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie.png", width =2700, height = 4800, res =220 )
my_plot_3
dev.off()
system(paste0("convert -trim ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie.png  ~/Y/monitorizare/MMOISE/extreme_zapada/outputs/graph_anomalii_1000m_campie.png"))








