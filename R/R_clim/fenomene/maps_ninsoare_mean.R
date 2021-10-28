library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(RColorBrewer)

source("R/cale_legenda_vectors.R")

rst <- terra::rast(paste0(drive_z,"grids_export/prima_zi_medie_ninsoare.tif"))

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

judete <- read_sf(paste0(drive_z, "shp/ROU_adm/judete.shp")) %>% st_transform(4326)
rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- na.omit(plt)
names(plt)[3] <- "tt"
summary(plt)
### legenda 

rmean <- colorRampPalette(rev(brewer.pal(9,"PuBuGn")), interpolate="linear")
brks.mean <- rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec"))
cols.mean <- rmean(length(brks.mean) )

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
  
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = 3.5, show.legend = F)+
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = 1.9, show.legend = F)+
  geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
  
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "")+
  #geom_sf_text(aes(label = JUDET),colour = "red",size = 3.15,data = judete)+
  scale_fill_manual(values = cols.mean,labels = rev(c("<= 20 Sep","30 Sep","10 Oct","20 Oct","31 Oct","10 Nov","20 Nov","30 Nov","10 Dec","20 Dec","=> 20 Dec")),  name = "")+
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "",colour = NULL)+
  theme_bw()+
  theme(legend.position = c(.92,.8), text= element_text(face = "bold", size = 9.8),
        legend.key.size = unit(0.325,"cm"),
        legend.background = element_rect(fill = "gray"),
        legend.text = element_text(color = "white"),
        legend.direction = "vertical",panel.background = element_rect(fill = "white"),
        legend.key = element_rect(color = "gray", fill = "black"),
        #panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"))+
  annotation_scale(location = "bl", style = "ticks")+
  annotation_custom(grob)
png(paste0(drive_z,"png/prima_zi_medie_ninsoare.png"), width =1800, height = 1400, res =220 )
pc1
dev.off()
system(paste0("convert -trim"," " ,drive_z,"/png/prima_zi_medie_ninsoare.png " ,drive_z, "/png/prima_zi_medie_ninsoare.png"))

################################################################################################
################################################################################################
################################################################################################
################################################################################################
############################## ultima zi grosime strat #########################################

rst <- terra::rast("/Volumes/Z/Mac_book/Teza_doctorat/Zapada_doctorat/grids_export/ultima_zi_medie_ninsoare.tif")
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
brks.mean <- rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Iun"))
cols.mean <- rmean(length(brks.mean))

### start the ploting  the map  using  ggplot 
pc1 <- ggplot() +  
  geom_tile(data= plt, aes(x=x, y=y,
                           fill= factor(tt1,level = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Iun")),
                                        labels = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Iun")))))+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.1)+
  
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = 3.5, show.legend = F)+
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = 1.9, show.legend = F)+
  geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 3.3)+
  
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = cols.mean,labels = rev(c("<= 1 Mar","10 Mar","20 Mar","31 Mar","10 Apr","20 Apr","30 Apr","10 Mai","20 Mai","30 Mai","10 Iun","> 10 Iun")), name = NULL)+
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

png(paste0(drive_z,"png/ultima_zi_medie_ninsoare.png"), width =1800, height = 1400, res =220 )
pc1
dev.off()
system(paste0("convert -trim"," " ,drive_z,"/png/ultima_zi_medie_ninsoare.png " ,drive_z, "/png/ultima_zi_medie_ninsoare.png"))



