library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(RColorBrewer)
library(tidyr)

source("R/cale_legenda_vectors.R")

rr <- list.files(paste0(drive_z,"grids_export/decadal"), pattern = ".tif", full.names = T)
rr <- grep("prima", rr, value = T)
rr <- grep("strat", rr, value = T)

  
rst <- terra::rast(rr)
names(rst) <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010","2011-2020")

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

judete <- read_sf(paste0(drive_z, "shp/ROU_adm/ROU_adm0.shp")) %>% st_transform(4326)
rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- plt%>% pivot_longer(-c(x,y), names_to = "indicator")
plt <- na.omit(plt)
names(plt)[4] <- "tt"

### legenda 

rmean <- colorRampPalette(brewer.pal(9,"PuBuGn"), interpolate="linear")
brks.mean <- c("<= 30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","31 Dec", ">= 1 Ian")
cols.mean <- rmean(length(brks.mean) )

### pentru legenda labels
plt$tt1[ plt$tt <= 61 ] <- "<= 30 Sep"
plt$tt1[plt$tt > 61 & plt$tt <= 71] <- "10 Oct"
plt$tt1[plt$tt > 71 & plt$tt <= 81] <- "20 Oct"
plt$tt1[plt$tt > 81 & plt$tt <= 92] <- "31 Oct"
plt$tt1[plt$tt > 92 & plt$tt <= 112] <- "10 Nov"
plt$tt1[plt$tt > 112 & plt$tt <= 122] <- "20 Nov"
plt$tt1[plt$tt > 122 & plt$tt <= 132] <- "30 Nov"
plt$tt1[plt$tt > 132 & plt$tt <= 142] <- "10 Dec"
plt$tt1[plt$tt > 142 & plt$tt <= 152] <- "20 Dec"
plt$tt1[plt$tt > 152 & plt$tt <= 163] <- "31 Dec"
plt$tt1[plt$tt > 163] <- ">= 1 Ian"

### start the ploting  the map  using  ggplot 
pc1 <- ggplot() +  
  geom_tile(data= plt, aes(x=x, y=y,
                           fill= factor(tt1,level = c("<= 30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","31 Dec", ">= 1 Ian"),
                                        labels =  c("<= 30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","31 Dec", ">= 1 Ian"))))+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.2)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
  
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = .5, show.legend = F)+
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = .2, show.legend = F)+
  geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 1.)+
  
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "")+
  #geom_sf_text(aes(label = JUDET),colour = "red",size = 3.15,data = judete)+
  scale_fill_manual(values = cols.mean,labels = c("<= 30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","31 Dec", ">= 1 Ian"),  name = "")+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = "bottom", text= element_text(face = "bold", size = 7.8),
        legend.key.size = unit(0.275,"cm"),
        legend.background = element_rect(fill = "gray"),
        legend.text = element_text(color = "white"),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(color = "gray", fill = "black"),
        #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks", size = 1)+
  #annotation_custom(grob)+
  facet_wrap(~indicator, nrow =3, ncol = 3)

png(paste0(drive_z,"png/prima_zi_medie_strat_zapada_decenial_1961-2020",".png"), width =1800, height = 1400, res =220 )
pc1
dev.off()
system(paste0("convert -trim"," " ,drive_z,"/png/prima_zi_medie_strat_zapada_decenial_1961-2020",".png " ,drive_z, "/png/prima_zi_medie_strat_zapada_decenial_1961-2020",".png"))

################################################################################################
################################################################################################
################################################################################################
################################################################################################
############################## ultima zi grosime strat #########################################

rr <- list.files(paste0(drive_z,"grids_export/decadal"), pattern = ".tif", full.names = T)
rr <- grep("ultima", rr, value = T)
rr <- grep("strat", rr, value = T)

rst <- terra::rast(rr)
names(rst) <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010","2011-2020")

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

judete <- read_sf(paste0(drive_z, "shp/ROU_adm/ROU_adm0.shp")) %>% st_transform(4326)

rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- plt%>% pivot_longer(-c(x,y), names_to = "indicator")
plt <- na.omit(plt)
names(plt)[4] <- "tt"

### pentru legenda labels
plt$tt1[plt$tt <= 185] <- "<= 1 Feb"
plt$tt1[plt$tt > 185 & plt$tt <= 205] <- "20 Feb"
plt$tt1[plt$tt > 204 & plt$tt <= 214] <- "1 Mar"
plt$tt1[plt$tt > 214 & plt$tt <= 234] <- "20 Mar"
plt$tt1[plt$tt > 234 & plt$tt <= 244] <-   "1 Apr"
plt$tt1[plt$tt > 244 & plt$tt <= 254] <- "10 Apr"
plt$tt1[plt$tt > 254 & plt$tt <= 264] <- "20 Apr"
plt$tt1[plt$tt > 264 & plt$tt <= 274] <- "30 Apr"
plt$tt1[plt$tt > 274 & plt$tt <= 284] <- "10 Mai"
plt$tt1[plt$tt > 284 & plt$tt <= 294] <- "20 Mai"
plt$tt1[plt$tt > 294 & plt$tt <= 304] <- "30 Mai"
plt$tt1[plt$tt > 304 & plt$tt <= 314] <- "10 Iun"
plt$tt1[plt$tt > 314 & plt$tt <= 324] <- "20 Iun"
plt$tt1[plt$tt > 324 ] <- "> 20 Iun"
##### legenda 

rmean <- colorRampPalette(brewer.pal(9,"PuBuGn"), interpolate="linear")
brks.mean <- c("<= 1 Feb","20 Feb", "1 Mar","20 Mar","1 Apr","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","20 Iun","> 20 Iun")
cols.mean <- rmean(length(brks.mean))

### start the ploting  the map  using  ggplot 
pc1 <- ggplot() +  
  geom_tile(data= plt, aes(x=x, y=y,
                           fill= factor(tt1,level = c("<= 1 Feb","20 Feb", "1 Mar","20 Mar","1 Apr","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","20 Iun","> 20 Iun"),
                                        labels =c("<= 1 Feb","20 Feb", "1 Mar","20 Mar","1 Apr","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","20 Iun","> 20 Iun"))))+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.2)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
  
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = .5, show.legend = F)+
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = .2, show.legend = F)+
  geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 1.)+
  
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = cols.mean,labels = c("<= 1 Feb","20 Feb", "1 Mar","20 Mar","1 Apr","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","20 Iun","> 20 Iun"),  name = "")+
  annotation_scale(location = "bl", style = "ticks", size = .5)+
  labs(x = "",y= "")+
  
  theme_bw()+
  theme(legend.position ="bottom", text= element_text(face = "bold", size = 7.8),
        legend.key.size = unit(0.35,"cm"),
        legend.background = element_rect(fill = "gray"),
        legend.text = element_text(color = "white"),
        legend.direction = "horizontal",panel.background = element_rect(fill = "white"),
        legend.key = element_rect(color = "gray", fill = "black"),
        #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks", size = .5)+
  annotation_custom(grob)+
  facet_wrap(~indicator, nrow =3, ncol = 3)


  png(paste0(drive_z,"png/ultima_zi_medie_strat_zapada_decadal_1961-2020.png"), width =1800, height = 1400, res =220 )
  pc1
  dev.off()
  system(paste0("convert -trim"," " ,drive_z,"/png/ultima_zi_medie_strat_zapada_decadal_1961-2020.png " ,drive_z, "/png/ultima_zi_medie_strat_zapada_decadal_1961-2020.png"))



