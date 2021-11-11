library(dplyr)
library(sf)
library(trend)
library(rnaturalearth)
library(climatetools)
library(ggrepel)
library(ggnewscale)
library(ggspatial)
library(RColorBrewer)


source("R/cale_legenda_vectors.R")

tabs <- list.files(path = paste0(drive_z,"tab_export"),pattern = "_2021.csv", full.names = T)
tabs <- grep("BRUMA",tabs, invert = T, value = T )

for (n in 1:length(tabs)){
  

  print(tabs[n])
  nume <- strsplit(tabs[n],"/|_|.csv")[[1]][14]
  print(nume)
  t <- read.csv(tabs[n])
  t <- as.data.frame(t)
  t <- na.omit(t)
  head(t)
  
  #### BRUMA &GROSZ & NINSOARE

  t_trend <- t %>%
    group_by(cod,nume) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.prima = sens.slope(prima_zi_jul_decalat)$estimates *10,
              sign.prima = mk.test(prima_zi_jul_decalat)$p.value,
              slope.ultima = sens.slope(ultima_zi_jul_decalat)$estimates *10,
              sign.ultima = mk.test(ultima_zi_jul_decalat)$p.value)
  #t_trend$slope.prima_plus <- t_trend$slope.prima
  #t_trend$slope.ultima_plus <- t_trend$slope.ultima
  t_tr <- merge(x = t_trend, y = ws[5:8], by.x = "nume",by.y = "NUME", all.x = TRUE)
 
  
  #t_tr <- st_as_sf(t_tr, coords = c('Lon', 'Lat'), crs = 4326)
  
  pu <- c("prima","prima","prima","ultima")

  kl <- ggplot() + 
      
    geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
    geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
    geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
    geom_sf(fill = "transparent", data = rom, color = "grey", lwd = 0.4)+
    
    geom_point(aes(x = Lon-.01, y = Lat, size = slope.prima*10), data = filter(t_tr, slope.prima > 0),pch = -as.hexmode("2B06"), alpha =2,  color =  "red", show.legend = F)+# to get outline
    geom_point(aes(x = Lon-.01, y = Lat, size = slope.prima*10), data = filter(t_tr, slope.prima < 0),pch = -as.hexmode("2B07"), alpha =2,  color =  "blue", show.legend = F)+# to get outline
    geom_point(aes(x = Lon-.01, y = Lat), data = filter(t_tr, slope.prima == 0),pch = 19 , color =  "grey" ,size = 3., show.legend = F)+# to get outline
    

    geom_point(aes(x = Lon+.08, y = Lat, size = slope.ultima*10), data = filter(t_tr, slope.ultima > 0),pch = -as.hexmode("2B06"), alpha =2,  color =  "red", show.legend = F)+# to get outline
    geom_point(aes(x = Lon+.08, y = Lat, size = slope.ultima*10), data = filter(t_tr, slope.ultima < 0),pch = -as.hexmode("2B07"), alpha =2,  color =  "blue", show.legend = F)+# to get outline
    geom_point(aes(x = Lon+.08, y = Lat), data = filter(t_tr, slope.ultima == 0),pch = 19 , color =  "grey" ,size = 3.,  show.legend = F)+# to get outline

  
    geom_point(data = filter(t_tr, sign.prima > 0.1),aes(x = Lon-.01, y = Lat),
               size = 1,pch = 4, color = "black",  show.legend = F)+
    geom_point(data = filter(t_tr, sign.ultima > 0.1),aes(x = Lon+.08, y = Lat),
               size = 1,pch = 4, color = "black",  show.legend = F)+
    
   coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
    labs(x = "",y= "",colour = NULL)+
    theme_bw()+
    # theme(legend.position = c(.92,.8), text= element_text(face = "bold", size = 9.8),
    #       legend.key.size = unit(0.325,"cm"),
    #       legend.background = element_rect(fill = "gray"),
    #       legend.text = element_text(color = "white"),
    #       legend.direction = "vertical",panel.background = element_rect(fill = "white"),
    #       legend.key = element_rect(color = "gray", fill = "black"),
    #       #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
    #       panel.border = element_rect(colour = "black", fill = "transparent"))+
    annotation_scale(location = "bl", style = "ticks")+
    annotation_custom(grob)
  
    png(paste0(drive_z,"png/harta_trend_decalat_",nume,".png"), height = 1500, width = 1800, res = 200)
    print(kl)
    dev.off()
    system(paste0("convert -trim " ,drive_z,"png/harta_trend_decalat_",nume,".png ", drive_z ,"png/harta_trend_decalat_",nume,".png",sep = ""))
 
    
}
  

############## verificare t 

t.f <- t%>% filter(nume == "Radauti")
t.f



