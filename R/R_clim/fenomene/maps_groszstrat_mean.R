library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(RColorBrewer)

source("R/cale_legenda_vectors.R")

rst <- terra::rast("grids_export/prima_zi_medie_strat_cu_zapada.tif")

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- na.omit(plt)
names(plt)[3] <- "tt"

### legenda 

rmean <- colorRampPalette(brewer.pal(9,"PuBuGn"), interpolate="linear")
brks.mean <- rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec"))
cols.mean <- rmean(length(brks.mean) - 1)


### pentru legenda labels
plt$tt1[plt$tt <= 263] <- "<= 20 Sep"
plt$tt1[plt$tt > 263 & plt$tt <= 273] <- "30 Sep"
plt$tt1[plt$tt > 273 & plt$tt <= 283] <- "10 Oct"
plt$tt1[plt$tt > 283 & plt$tt <= 293] <- "20 Oct"
plt$tt1[plt$tt > 293 & plt$tt <= 304] <- "31 Oct"
plt$tt1[plt$tt > 304 & plt$tt <= 314] <- "10 Nov"
plt$tt1[plt$tt > 314 & plt$tt <= 324] <- "20 Nov"
plt$tt1[plt$tt > 324 & plt$tt <= 334] <- "30 Nov"
plt$tt1[plt$tt > 334 & plt$tt <= 344] <- "10 Dec"
plt$tt1[plt$tt > 344 & plt$tt <= 354] <- "20 Dec"
plt$tt1[plt$tt > 354 ] <- "=> 20 Dec"


### start the ploting  the map  using  ggplot 
pc1 <- ggplot() +  
  geom_tile(data= plt, aes(x=x, y=y,
                           fill= factor(tt1,level = rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec")),
                                        labels =  rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec")))))+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "")+
  
  #geom_sf_text(aes(label = JUDET),colour = "red",size = 3.15,data = judete)+
  scale_fill_manual(values = cols.mean,labels = rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec")),  name = F)+
  
  annotation_scale(location = "bl", width_hint = 0.24) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()+
  theme(legend.position = c(.9,.8),legend.title = element_text( face = "plain", size= 11), text= element_text(face = "bold", size = 9),
        legend.key.size = unit(0.4,"cm"),
        legend.background = element_rect(fill = "gray", color = NA),
        legend.text = element_text(color = "white"),
        legend.direction = "vertical",panel.background = element_rect(fill = "white"),
        legend.key = element_rect(color = "gray", fill = "black"))+
  labs(#title = "Șansele unui Revelion+1 Alb (1981-2019)",fontface = "italic", color = "grey22", size = 9,
    caption = " ©Administratia Națională de Meteorologie")

png("png/prima_zi_medie_strat_zapada.png", width =1800, height = 1400, res =220 )
pc1
dev.off()
system(paste0("convert -trim png/prima_zi_medie_strat_zapada.png  png/ultima_zi_strat_zapada.png"))

################################################################################################
################################################################################################
################################################################################################
################################################################################################
############################## ultima zi grosime strat #########################################

rst <- terra::rast("grids_export/ultima_zi_medie_strat_cu_zapada.tif")
tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- na.omit(plt)
names(plt)[3] <- "tt"

### pentru legenda labels
plt$tt1[plt$tt <= 60] <- "<= 1 Mar"
plt$tt1[plt$tt > 60 & plt$tt <= 69] <- "10 Mar"
plt$tt1[plt$tt > 69 & plt$tt <= 79] <- "20 Mar"
plt$tt1[plt$tt > 79 & plt$tt <= 90] <- "31 Mar"
plt$tt1[plt$tt > 90 & plt$tt <= 100] <- "10 Apr"
plt$tt1[plt$tt > 100 & plt$tt <= 110] <- "20 Apr"
plt$tt1[plt$tt > 110 & plt$tt <= 120] <- "30 Apr"
plt$tt1[plt$tt > 120 & plt$tt <= 130] <- "10 Mai"
plt$tt1[plt$tt > 130 & plt$tt <= 140] <- "20 Mai"
plt$tt1[plt$tt > 140 & plt$tt <= 150] <- "30 Mai"
plt$tt1[plt$tt > 150 & plt$tt <= 161] <- "10 Iun"
plt$tt1[plt$tt > 161 ] <- " > 10 Iun"
##### legenda 

rmean <- colorRampPalette(brewer.pal(9,"PuBuGn"), interpolate="linear")
brks.mean <- rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Jun"))
cols.mean <- rmean(length(brks.mean))

### start the ploting  the map  using  ggplot 
pc1 <- ggplot() +  
  geom_tile(data= plt, aes(x=x, y=y,
                           fill= factor(tt1,level = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Jun")),
                                        labels = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Jun")))))+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  
  scale_fill_manual(values = cols.mean,labels = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Jun")), name = NULL)+
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = c(.94,.82), text= element_text(face = "bold", size = 9.8),
        legend.key.size = unit(0.325,"cm"),
        legend.background = element_rect(fill = "gray"),
        legend.text = element_text(color = "white"),
        legend.direction = "vertical",panel.background = element_rect(fill = "white"),
        legend.key = element_rect(color = "gray", fill = "black"),
        #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks")+
  annotation_custom(grob)

png("png/ultima_zi_medie_strat_zapada.png", width =1800, height = 1400, res =220 )
pc1
dev.off()
system(paste0("convert -trim png/ultima_zi_medie_strat_zapada.png  png/ultima_zi_medie_strat_zapada.png"))




