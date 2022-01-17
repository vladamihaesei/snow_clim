library(dplyr)
library(trend)
library(climatetools)
library(ggrepel)
library(ggnewscale)
library(ggspatial)
library(RColorBrewer)
library(ggnewscale)
source("R/hillshade.R")
ws.prov <- read.csv(paste0(drive_z,"tab/ws_climatetools_provincii_NAomit.csv"))

tabs <- list.files(path = paste0(drive_z,"tab_export"),pattern = "1961-2020.csv", full.names = T)
tabs <- grep("BRUMA|zile_",tabs, invert = T, value = T )
tabs <- tabs[1:2]

for (n in 1:length(tabs)){
  
  print(tabs[n])
  nume <- strsplit(tabs[n],"/|_|.csv")[[1]][16]
  print(nume)
  t <- read.csv(tabs[n])
  t <- as.data.frame(t)
  t <- na.omit(t)
  
  head(t)
  
  t_trend <- t %>%
    group_by(cod,nume) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.interval = sens.slope(interval_zile)$estimates *10,
              sign.interval = mk.test(interval_zile)$p.value)
  
  names(t_trend)[1] <- "CODGE"
  t_tr <- t_trend %>% left_join(ws.prov[c(3,4,5,6,8,9,10)])
  
  write.csv(t_tr, paste0(drive_z,"tab_export/trend_",nume,"_prima_ultima_interval_1961-2020.csv"), row.names = F)
  
  kl <- ggplot() + 
    
    geom_tile(data = hill_df, aes(x = x, y = y, fill = layer), show.legend = F) +
    scale_fill_gradient(low = "black", high = "white") +
    new_scale_fill() +
    geom_tile(data = dem.df, aes(x = x, y = y, fill = meters), alpha = 0.45) +
    scale_fill_gradientn(colours = terrain.colors(8)) +
    new_scale_fill() +
    
    #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)+
    #scale_fill_gradientn(colours = rev(terrain.colors(10)))+
    
    geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
    geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.15)+
    #geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
    geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ciawf ), size = 3.8 ,fontface="italic")+
    geom_sf(fill = "transparent", data = rom, color = "black", lwd = 0.4)+
    
    geom_point(aes(x = Lon-.01, y = Lat, size = slope.interval*100), data = filter(t_tr, slope.interval > 0),pch = -as.hexmode("2B06"), alpha =2,  color =  "red", show.legend = F)+# to get outline
    geom_point(aes(x = Lon-.01, y = Lat, size = slope.interval*100), data = filter(t_tr, slope.interval < 0),pch = -as.hexmode("2B07"), alpha =2,  color =  "blue", show.legend = F)+# to get outline
    geom_point(aes(x = Lon-.01, y = Lat), data = filter(t_tr, slope.interval == 0), pch = 19 , color =  "grey" ,size = 3., show.legend = F)+# to get outline
    
    geom_point(data = filter(t_tr, sign.interval >= 0.05),aes(x = Lon-.01, y = Lat),
               size = 3.5,pch = 4, color = "black",  show.legend = F)+
    
    coord_sf(xlim = c(20.2,29.9), ylim = c(43.5, 48.3), expand = F)+
    labs(x = "",y= "",colour = NULL)+
    theme_bw()+
     theme(legend.position = c(.92,.87), text= element_text(face = "bold", size = 10.8),
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
  
  
  png(paste0(drive_z,"png/harta_trend_",nume,"_interval_1961-2020.png"), height = 1500, width = 1800, res = 250)
  print(kl)
  dev.off()
  system(paste0("convert -trim " ,drive_z,"png/harta_trend_",nume,"_interval_1961-2020.png ", drive_z ,"png/harta_trend_",nume,"_interval_1961-2020.png",sep = ""))
  
}




