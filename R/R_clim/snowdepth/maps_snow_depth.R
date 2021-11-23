#load packages
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(hrbrthemes)
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(hrbrthemes)
library(ggspatial)
library(png)
library(cowplot)
library(magick)
library(tidyr)
library(terra)
library(tidyr)

source("R/cale_legenda_vectors.R")


#### pentru legenda
### media multianual
rmean <- colorRampPalette(c(rev(brewer.pal(3,"PuBuGn")),brewer.pal(9,"RdPu")), interpolate="linear")
brks.mean <- c(1,2,3,4,5,6,10,15,30,50,80,110)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(0, 130)
den <- "cm"


##########listare rastere
rs <- list.files( paste0(drive_z,"grids_export/snowdepth"),recursive = T, full.names = T)

for (i in 1:length(rs)){
  
  
  indice <- strsplit(rs[i], "/|_")[[1]][12]
  
  per  <- strsplit(rs[i], "/|_|.nc")[[1]][14]
  
  r <- terra::rast(rs[i])
  
  crs(r) <- "+init=epsg:3844"
  
  newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  rr <- terra::project(r, newproj1)
  
  rr <- terra::mask(rr, vect(judete))
  r.m <- mean(rr, na.rm = F)
  
  #plot(r.m, col = "veridis")
  
  plm <- as.data.frame(r.m,xy=T)
  names(plm)[3] <- "grosz" 
  
  
  path <- paste0(drive_z,"png/",indice,"/")
  
  g <- ggplot() +
    
    geom_raster(data= plm, aes(x = x, y = y, fill= grosz), interpolate = F)+
    
    geom_sf(data = judete ,color = "grey", fill = "transparent", size= 0.45)+
    
    
    geom_sf(data = granite, color = "black", fill = "grey", size = 0.1)+
    
    geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
    #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
    geom_sf(aes(geometry = geometry), data = loc,pch = 20,  size = 2.6, show.legend = F)+
    
    #geom_sf(aes(geometry = geometry), data = filter(loc, NUMELOC %notin% c("BUCURESTI", "Ploiești")), pch = 20, bg = "black", size = 1.9, show.legend = F)+
    
    geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc, NUMELOC != "Ploiești"), nudge_x = -.1, nudge_y = .089, size = 3.2)+
    
    geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc,NUMELOC == "Ploiești"), nudge_x = -.1, nudge_y = -.089, size = 3.2)+
    coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_stepsn(colours = cols.mean, name = den,
                      breaks = brks.mean,
                      limits =lim.mean) +
    #scale_fill_continuous(type = "viridis")+
    #scale_colour_gradientn(colours=c("red","white","blue"),name = "cm",limits = c(0,110))+
    guides(fill = guide_colourbar(barwidth = 48.0, barheight = 0.99, title.position = "right",
                                  label.theme = element_text(size = 12.))) +
    
    scale_linetype_manual(values=c("twodash")) +
    
    labs(x = "",y= "")+
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"),
          legend.background = element_rect(fill = "white"),
          strip.background = element_rect(colour = "black", fill = "white"),
          legend.title=element_text(size=14),
          legend.text=element_text(size=15),text = element_text(size=14.5)
    )+annotation_custom(grob) +annotation_scale(location = "bl", style = "ticks")
  ##+annotation_custom(logo.g,xmin = 20.22,xmax = 20.85,ymin = 47.65,ymax = 48.35)   
  
  png(paste0(path,indice,"_",per, ".png"), height = 2200 , width = 2200,units = "px", res = 220)
  print(g)
  dev.off()
  system(paste0("convert -trim ",path,indice,"_",per,".png  ",path,indice,"_",per,".png",sep = ""))
  
  
  
}

### monttlhy plot 



rs <- list.files( paste0(drive_z,"grids_export/snowdepth"),recursive = T, full.names = T)

rs <- grep("DJF|MA", rs,invert = T, value = T)

ad <- stack()

for (i in 1:length(rs)){
  
  indice <- strsplit(rs[i], "/|_")[[1]][12]
  
  per  <- strsplit(rs[i], "/|_|.nc")[[1]][14]
  
  print(per)
  
  
  r <- terra::rast(rs[i])
  
  crs(r) <- "+init=epsg:3844"
  
  newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  rr <- terra::project(r, newproj1)
  
  rr <- terra::mask(rr, vect(judete))
  r.m <- mean(rr, na.rm = F)
  r.mb <- brick(r.m)
  names(r.mb) <- per
  ad <- stack(r.mb,ad)
  
  #plot(r.m, col = "veridis")
}


plm <- as.data.frame(ad,xy=T)
plm.l <- plm %>% pivot_longer(-c(x,y), names_to = "luna") %>% na.omit()

path <- paste0(drive_z,"png/",indice,"/")

rmean <- colorRampPalette(c(rev(brewer.pal(3,"PuBuGn")),brewer.pal(9,"RdPu")), interpolate="linear")
brks.mean <- c(1,2,3,4,5,6,10,15,20,30,50,80,110)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(0, 140)
den <- "cm"


g <- ggplot() +
  
  geom_raster(data= plm.l, aes(x = x, y = y, fill= value), interpolate = F)+
  
  geom_sf(data = judete ,color = "grey", fill = "transparent", size= 0.45)+
  
  
  geom_sf(data = granite, color = "black", fill = "grey", size = 0.1)+
  
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(aes(geometry = geometry), data = loc,pch = 20,  size = 1.4, show.legend = F)+
  
  #geom_sf(aes(geometry = geometry), data = filter(loc, NUMELOC %notin% c("BUCURESTI", "Ploiești")), pch = 20, bg = "black", size = 1.9, show.legend = F)+
  
  geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc, NUMELOC != "Ploiești"), nudge_x = -.1, nudge_y = .089, size = 2.)+
  
  geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc,NUMELOC == "Ploiești"), nudge_x = -.1, nudge_y = -.089, size = 2.)+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_stepsn(colours = cols.mean, name = den,
                    breaks = brks.mean,
                    limits =lim.mean) +
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_gradientn(colours=c("red","white","blue"),name = "cm",limits = c(0,110))+
  guides(fill = guide_colourbar(barwidth = 53.0, barheight = 0.99, title.position = "right",
                                label.theme = element_text(size = 10.))) +
  
  scale_linetype_manual(values=c("twodash")) +
  
  labs(x = "",y= "")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"),
        legend.background = element_rect(fill = "white"),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=15),text = element_text(size=14.5)
  ) +annotation_scale(location = "bl", style = "ticks")+facet_wrap(~factor(luna, levels = c("Noiembrie", "Decembrie","Ianuarie","Februarie","Martie","Aprilie")))
##+annotation_custom(logo.g,xmin = 20.22,xmax = 20.85,ymin = 47.65,ymax = 48.35)   

png(paste0(path,indice,"_","nov-aprilie", ".png"), height = 2500 , width = 2800,units = "px", res = 220)
print(g)
dev.off()
system(paste0("convert -trim ",path,indice,"_","nov-aprilie",".png  ",path,indice,"_","nov-aprilie",".png",sep = ""))


