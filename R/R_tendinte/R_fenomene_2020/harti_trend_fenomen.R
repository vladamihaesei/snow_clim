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
dem <- raster("~/Documente/vlad_R/mnt_ro/mnt_ro_fill_no_data.tif")
rom <- readOGR("/Volumes/Backup Plus/D/2019/vldZ/R_meteo/shp/ROU_adm_diva/ROU_adm0.shp")
rom <- st_as_sf(rom)
dem <- mask(dem,rom)



tabs <- list.files(path = "tab_export/",pattern = "_years.csv", full.names = T)
tabs

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
              sign.ultima = mk.test(ultima_zi_jul_corect)$p.value)%>%
    mutate(lab_col = ifelse(slope.prima < 0, "darkblue", "darkred"))
              
  
  t_tr <- merge(x = t_trend, y = ws[5:8], by.x = "nume",by.y = "NUME", all.x = TRUE)

  
  
    # kl <- ggplot() +
    # 
    #   geom_sf(fill="transparent", data = rom)+
    #   # make title bold and add space
    #   geom_point(aes(x=Lon, y=Lat, color= slope.prima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
    #   geom_point(aes(x=Lon, y=Lat, color=slope.prima), data=t_tr, alpha=1, size=2)+
    #   scale_colour_gradientn(colours=c( "blue","white","red"),name = "days/\ndecade",limits = c(-40,40))+ # change color scale
    #   new_scale_colour()+
    #   geom_point(data = filter(t_tr, sign.prima > 0.1),aes(x = Lon, y = Lat, color = "Unsign. trend \n p-value > 0.1"),
    #              size = 0.8,pch = 4, show.legend = T)+
    #   scale_color_manual(values = c("black"), name = "")+
    #   #geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value)) +
    #   #scale_fill_gradient(low = "black", high = "white") +
    #   #new_scale_fill() +
    #   #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)
    # 
    #   geom_text_repel(data = t_tr, aes(x = Lon, y = Lat, label = nume),
    #                   fontface = "bold",colour = t_tr$lab_col, size = 2.4)+
    #   labs(title = paste("Tendință decenală a primei zile cu ",nume),
    #        subtitle = "(1961-2019)",
    #        caption = "Database: @MeteoRomania")+
    #   theme_bw() +
    #   guides(fill = guide_colourbar(barwidth = 0.6, barheight = 6.6, title.position="top"))+
    #   theme( legend.position = c(.9,.7))
    # 
    # 
    # png(paste0("png/harta_trend_prima",nume,".png"), height = 1500, width = 1800, res = 200)
    # print(kl)
    # dev.off()
    # system(paste0("convert -trim png/harta_trend_prima",nume,".png  png/harta_trend_prima",nume,".png",sep = ""))
  }
   
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
              sign.ultima = mk.test(ultima_zi_jul_corect)$p.value)%>%
  mutate(lab_col = ifelse(slope.ultima < 0, "darkblue", "darkred"))
  t_tr <- merge(x = t_trend, y = ws[5:8], by.x = "nume",by.y = "NUME", all.x = TRUE)
 
 
  kl1 <- ggplot() + 
    
    geom_sf(fill="transparent", data = rom)+
    # make title bold and add space
    geom_point(aes(x=Lon, y=Lat, color= slope.ultima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
    geom_point(aes(x=Lon, y=Lat, color=slope.ultima), data=t_tr, alpha=1, size=2)+
    scale_colour_gradientn(colours=c( "blue","white","red"),name = "days/\ndecade",limits = c(-40,40))+ # change color scale
    new_scale_colour()+
    geom_point(data = filter(t_tr, sign.ultima > 0.1),aes(x = Lon, y = Lat, color = "Unsign. trend \n p-value > 0.1"),
               size = 0.8,pch = 4, show.legend = T)+
    scale_color_manual(values = c("black"), name = "")+
    #geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value)) + 
    #scale_fill_gradient(low = "black", high = "white") + 
    #new_scale_fill() + 
    #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)
    geom_text_repel(data = t_tr, aes(x = Lon, y = Lat, label = nume), 
                    fontface = "bold",colour = t_tr$lab_col, size = 2.4)+
    labs(title = paste("Tendință decenală a ultimei zile cu ",nume), 
         subtitle = "(1961-2019)",
         caption = "Database: @MeteoRomania")+
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 0.6, barheight = 6.6, title.position="top"))+
    theme( legend.position = c(.9,.7))
  
  png(paste0("png/harta_trend_ultima",nume,".png"), height = 1500, width = 1800, res = 200)
  print(kl1)
  dev.off()
  system(paste0("convert -trim png/harta_trend_ultima",nume,".png  png/harta_trend_ultima",nume,".png",sep = ""))
  
  }


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
  pu <- c("prima","prima","prima","ultima")
  
  if(names(t_tr)[3] == "slope.prima"){
    kl <- ggplot() + 
      
      geom_sf(fill="transparent", data = rom)+
      # make title bold and add space
      geom_point(aes(x=Lon, y=Lat, color= slope.prima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
      geom_point(aes(x=Lon, y=Lat, color=slope.prima), data=t_tr, alpha=1, size=2)+
      scale_colour_gradientn(colours=c( "blue","white","red"),name = "No.of days/\n10years",limits = c(min(t_tr$slope.prima),max(t_tr$slope.prima)))+ # change color scale
      new_scale_colour()+
      geom_point(data = filter(t_tr, sign.prima > 0.1),aes(x = Lon, y = Lat, color = "Unsign. trend \n p-value > 0.1"),
                 size = 0.8,pch = 4, show.legend = T)+
      scale_color_manual(values = c("black"), name = "")+
      #geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value)) + 
      #scale_fill_gradient(low = "black", high = "white") + 
      #new_scale_fill() + 
      #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)
      
      labs(title = paste("Tendință decenală a primei zile cu ",nume), 
           subtitle = "(1961-2019)",
           caption = "Database: @MeteoRomania")+
      theme_bw() +
      guides(fill = guide_colourbar(barwidth = 0.6, barheight = 6.6, title.position="top"))+
      theme( legend.position = c(.9,.7))
    
    
    png(paste0("png/harta_trend_prima",nume,".png"), height = 1500, width = 1800, res = 200)
    print(kl)
    dev.off()
    system(paste0("convert -trim png/harta_trend_prima",nume,".png  png/harta_trend_prima",nume,".png",sep = ""))
    
  }else if(names(t_tr)[5] == "slope.ultima"){
    kl1 <- ggplot() + 
      
      geom_sf(fill="transparent", data = rom)+
      # make title bold and add space
      geom_point(aes(x=Lon, y=Lat, color= slope.ultima), data=t_tr, alpha=1, size=3, color="grey20")+# to get outline
      geom_point(aes(x=Lon, y=Lat, color=slope.ultima), data=t_tr, alpha=1, size=2)+
      scale_colour_gradientn(colours=c( "blue","white","red"),name = "No.of days/\n10years",limits = c(min(t_tr$slope.ultima),max(t_tr$slope.ultima)))+ # change color scale
      new_scale_colour()+
      geom_point(data = filter(t_tr, sign.ultima > 0.1),aes(x = Lon, y = Lat, color = "Unsign. trend \n p-value > 0.1"),
                 size = 0.8,pch = 4, show.legend = T)+
      scale_color_manual(values = c("black"), name = "")+
      #geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value)) + 
      #scale_fill_gradient(low = "black", high = "white") + 
      #new_scale_fill() + 
      #geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4)
      
      labs(title = paste("Tendință decenală a ultimei zile cu ",nume), 
           subtitle = "(1961-2019)",
           caption = "Database: @MeteoRomania")+
      theme_bw() +
      guides(fill = guide_colourbar(barwidth = 0.6, barheight = 6.6, title.position="top"))+
      theme( legend.position = c(.9,.7))
    
    png(paste0("png/harta_trend_ultima",nume,".png"), height = 1500, width = 1800, res = 200)
    print(kl1)
    dev.off()
    system(paste0("convert -trim png/harta_trend_ultima",nume,".png  png/harta_trend_ultima",nume,".png",sep = ""))
    
  }
  
}





