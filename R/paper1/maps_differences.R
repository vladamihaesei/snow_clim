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

tt <- as.data.frame(tt)

rmean <- colorRampPalette(brewer.pal(11,"RdBu"), interpolate="linear")
brks.mean <- seq(-11,11, by = 1)
cols.mean <- rmean1(length(brks.mean) - 1)
cols.meanv1 <- c(cols.mean[1:(length(cols.mean)/2) -1],"white") 
cols.meanv2 <- c("white",cols.mean[13:length(cols.mean)])
cols.meant <- c(cols.meanv1,cols.meanv2)
lim.mean <- c(-12,12)

rmean1 <- colorRampPalette(brewer.pal(11,"RdBu"), interpolate="linear")
brks.mean1 <- seq(-11,11, by = 1)
cols.mean1 <- rev(rmean1(length(brks.mean1) - 1))
cols.mean1v1 <- c(cols.mean1[1:(length(cols.mean1)/2) -1],"white") 
cols.mean1v2 <- c("white",cols.mean1[13:length(cols.mean1)])
cols.meant1 <- c(cols.mean1v1,cols.mean1v2)
lim.mean1 <- c(-12,12)

#### FSC 
fsc <- ggplot() + 
  
  geom_tile(data = hill_df, aes(x = x, y = y, fill = layer), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_tile(data = dem.df, aes(x = x, y = y, fill = meters), alpha = 0.45, show.legend = F) +
  scale_fill_gradientn(colours = terrain.colors(8)) +
  new_scale_fill() +
  #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)+
  #scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.15)+
  #geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
  geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ciawf ), size = 3.8 ,fontface="italic")+
  geom_sf(fill = "transparent", data = rom, color = "black", lwd = 0.4)+
  
  geom_point(aes(x = Lon-.01, y = Lat,color = FSC), data = tt, pch = 19 ,size = 2.5, show.legend = T)+# to get outline
  scale_color_stepsn(colours = cols.meant1, name = den,
                    breaks = brks.mean1,
                    limits =lim.mean1) + 

  guides(color = guide_colourbar(barwidth = 27.0, barheight = 0.7, title.position = "right",
                                label.theme = element_text(size = 10)),) +
 
  coord_sf(xlim = c(20.2,29.9), ylim = c(43.5, 48.3), expand = F)+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.50,"cm"),
        legend.background = element_rect(fill = "transparent"),
      
        axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12))+
  #       legend.text = element_text(color = "white"),
  #       legend.direction = "vertical",panel.background = element_rect(fill = "white"),
  #       legend.key = element_rect(color = "gray", fill = "black"),
  #       #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
  #       panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks")
png(filename = paste0(drive_z,"png/articol1/map_fsc_diferences.png"), width = 1600, height = 1350, res = 250)
fsc
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/map_fsc_diferences.png  ", drive_z, "png/articol1/map_fsc_diferences.png"))

#### LSC 
lsc <- ggplot() + 
  
  geom_tile(data = hill_df, aes(x = x, y = y, fill = layer), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_tile(data = dem.df, aes(x = x, y = y, fill = meters), alpha = 0.45, show.legend = F) +
  scale_fill_gradientn(colours = terrain.colors(8)) +
  new_scale_fill() +
  #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)+
  #scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.15)+
  #geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
  geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ciawf ), size = 3.8 ,fontface="italic")+
  geom_sf(fill = "transparent", data = rom, color = "black", lwd = 0.4)+
  
  geom_point(aes(x = Lon-.01, y = Lat,color = LSC), data = tt, pch = 19 ,size = 2.5, show.legend = T)+# to get outline
  scale_color_stepsn(colours = cols.meant, name = den,
                     breaks = brks.mean,
                     limits =lim.mean) + 
  
  guides(color = guide_colourbar(barwidth = 27.0, barheight = 0.7, title.position = "right",
                                 label.theme = element_text(size = 10)),) +
  
  coord_sf(xlim = c(20.2,29.9), ylim = c(43.5, 48.3), expand = F)+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.50,"cm"),
        legend.background = element_rect(fill = "transparent"),
        
        axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12))+
  #       legend.text = element_text(color = "white"),
  #       legend.direction = "vertical",panel.background = element_rect(fill = "white"),
  #       legend.key = element_rect(color = "gray", fill = "black"),
  #       #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
  #       panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks")
png(filename = paste0(drive_z,"png/articol1/map_lsc_diferences.png"), width = 1600, height = 1350, res = 250)
lsc
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/map_lsc_diferences.png  ", drive_z, "png/articol1/map_lsc_diferences.png"))

#### SCD
scd <- ggplot() + 
  
  geom_tile(data = hill_df, aes(x = x, y = y, fill = layer), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_tile(data = dem.df, aes(x = x, y = y, fill = meters), alpha = 0.45, show.legend = F) +
  scale_fill_gradientn(colours = terrain.colors(8)) +
  new_scale_fill() +
  #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)+
  #scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.15)+
  #geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
  geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ciawf ), size = 3.8 ,fontface="italic")+
  geom_sf(fill = "transparent", data = rom, color = "black", lwd = 0.4)+
  
  geom_point(aes(x = Lon-.01, y = Lat,color = SCD), data = tt, pch = 19 ,size = 2.5, show.legend = T)+# to get outline
  scale_color_stepsn(colours = cols.meant, name = den,
                     breaks = brks.mean,
                     limits =lim.mean1) + 
  
  guides(color = guide_colourbar(barwidth = 27.0, barheight = 0.7, title.position = "right",
                                 label.theme = element_text(size = 10)),) +
  
  coord_sf(xlim = c(20.2,29.9), ylim = c(43.5, 48.3), expand = F)+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.50,"cm"),
        legend.background = element_rect(fill = "transparent"),
        
        axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12))+
  #       legend.text = element_text(color = "white"),
  #       legend.direction = "vertical",panel.background = element_rect(fill = "white"),
  #       legend.key = element_rect(color = "gray", fill = "black"),
  #       #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
  #       panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks")
png(filename = paste0(drive_z,"png/articol1/map_scd_diferences.png"), width = 1600, height = 1350, res = 250)
scd
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/map_scd_diferences.png  ", drive_z, "png/articol1/map_scd_diferences.png"))




