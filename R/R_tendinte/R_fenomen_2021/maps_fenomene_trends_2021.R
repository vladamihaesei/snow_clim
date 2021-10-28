library(dplyr)
library(tidyverse)
library(lubridate)
library(metR)
library(trend)
library(rgdal)
library(rgeos)
library(raster)
library(elevatr)
library(rnaturalearth)
library(climatetools)
library(sf)
library(ggrepel)
library(ggnewscale)
library(RColorBrewer)


source("legenda_vectors.R")

tabs <- list.files(path = "tab_export/",pattern = "_years.csv", full.names = T)
tabs <- grep("BRUMA",tabs, invert = T, value = T )

rmean <- colorRampPalette(brewer.pal(11,"RdBu"), interpolate="linear")
brks.mean <- seq(-20.0,20.0, by = 5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-25.0,25.0)
den <- "zile/10 ani"

rmean1 <- colorRampPalette(brewer.pal(11,"RdBu"), interpolate="linear")
brks.mean1 <- seq(-8.,8.0, by = 1)
cols.mean1 <- rmean1(length(brks.mean1) - 1)
lim.mean1 <- c(-9.0,9.0)
den1 <- "zile/10 ani"

for (n in 1:length(tabs)){
  
  print(tabs[n])
  nume <- strsplit(tabs[n],"/|_")[[1]][8]
  nume
  t <- read.csv(tabs[n])
  t <- as.data.frame(t)
  t <- na.omit(t)
  head(t)
  
  #### BRUMA &GROSZ & NINSOARE
  
  t_trend <- t %>%
    group_by(cod,nume) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.prima = sens.slope(prima_zi_jul_corect)$estimates *10,
              sign.prima = mk.test(prima_zi_jul_corect)$p.value,
              slope.ultima = sens.slope(ultima_zi_jul_corect)$estimates *10,
              sign.ultima = mk.test(ultima_zi_jul_corect)$p.value)
  
  t_tr <- merge(x = t_trend, y = ws[5:8], by.x = "nume",by.y = "NUME", all.x = TRUE)
  
  t_tr <- st_as_sf(t_tr, coords = c('Lon', 'Lat'), crs = 4326)
  
  pu <- c("prima","prima","prima","ultima")

  kl <- ggplot() + 
      
    geom_sf(data = reg,color = "#4B4B4B", fill = "transparent", size= 0.55)+
    geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
    #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = 3.5, show.legend = F)+
    geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = 1.9, show.legend = F)+
    geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
    geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
    # make title bold and add space
    
    geom_point(aes(x=Lon, y=Lat, color= slope.prima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
    geom_point(aes(x=Lon, y=Lat, color=slope.prima), data=t_tr, alpha=1, size=2)+
    scale_colour_gradientn(colours=c( "blue","white","red"),name = "Nr. zile/\n10 ani",limits = limits = c(-20,20))+ # change color scale
   
    guides(fill = guide_colourbar(barwidth = 0.25, barheight = 8.6, title.position="top"))+
    
    # 
     annotation_scale(location = "bl", style = "ticks")+
     labs(x = "",y= "")+
    # 
     new_scale_colour()+
     geom_point(data = filter(t_tr, sign.prima > 0.1),aes(x = Lon, y = Lat, color = color = "Nesemnif.\n p-value > 0.1"),
                size = 0.8,pch = 4, show.legend = T)+
     scale_color_manual(values = c("black"), name = "")+
    # 
    # 
     theme(legend.position = c(.9,.75),
           panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
           panel.border = element_rect(colour = "black", fill = "transparent"),
           legend.background = element_rect(fill = "white")
    )
     
    png(paste0("png/harta_trend_prima",nume,".png"), height = 1500, width = 1800, res = 240)
    print(kl)
    dev.off()
    system(paste0("convert -trim png/harta_trend_prima",nume,".png  png/harta_trend_prima",nume,".png",sep = ""))
    
  kl1 <- ggplot() + 
      
    geom_sf(data = reg,color = "#4B4B4B", fill = "transparent", size= 0.55)+
    geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
    #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = 3.5, show.legend = F)+
    geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = 1.9, show.legend = F)+
    geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
    geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
    # make title bold and add space
    geom_point(aes(x=Lon, y=Lat, color= slope.ultima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
    geom_point(aes(x=Lon, y=Lat, color=slope.ultima), data=t_tr, alpha=1, size=2)+
    scale_colour_gradientn(colours=c( "blue","white","red"),name = "Nr. zile/\n10 ani",limits = c(-20,20))+ # change color scale
    
    new_scale_colour()+
    geom_point(data = filter(t_tr, sign.ultima > 0.1),aes(x = Lon, y = Lat, color = color = "Nesemnif.\n p-value > 0.1"),
                 size = 0.8,pch = 4, show.legend = T)+
    scale_color_manual(values = c("black"), name = "")+
    coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
                   
    guides(fill = guide_colourbar(barwidth = 0.25, barheight = 8.6, title.position="top"))+
  
    annotation_scale(location = "bl", style = "ticks")+
    labs(x = "",y= "")+
    theme(legend.position = c(.9,.75),
          panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"),
          legend.background = element_rect(fill = "white"))
  
    png(paste0("png/harta_trend_ultima",nume,".png"), height = 1500, width = 1800, res = 200)
    print(kl1)
    dev.off()
    system(paste0("convert -trim png/harta_trend_ultima",nume,".png  png/harta_trend_ultima",nume,".png",sep = ""))
 
       
}
  






