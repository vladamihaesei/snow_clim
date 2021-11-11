library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(RColorBrewer)
source("R/cale_legenda_vectors.R")


rr <- list.files(paste0(drive_z,"grids_export/decadal"), pattern = ".tif", full.names = T)
rr <- grep("nr_zile_strat_zapada", rr, value = T)
rr <- grep("anual", rr, value = T)

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


rmean <- colorRampPalette(rev(brewer.pal(9,"BuPu")), interpolate="linear")
brks.mean <- seq(20.0,200.0, by = 20)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(0.0,220.0)
den <- "nr. zile"

pc1 <- ggplot() + 
  
  geom_raster(data= plt, aes(x = x, y = y, fill= tt), interpolate = F) +
  
  scale_fill_stepsn(colours = cols.mean, name = den,
                    breaks = brks.mean,
                    limits =lim.mean) +
  #annotation_scale(location = "bl", style = "ticks", size = .5)+
  geom_sf(fill="transparent", data = judete)+
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.2)+
  #geom_sf(data = ctrs, color = "black", fill = "lightgrey", size = 0.1)+
  geom_sf(data = granite, color = "black", fill = "lightgrey", size = 0.2)+
  
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC %in% c("BUCURESTI")),pch = 20,  size = 1.5, show.legend = F)+
  geom_sf(aes(geometry = geometry), data = filter(loc,NUMELOC!="BUCURESTI"),pch = 20, bg = "black", size = 1.2, show.legend = F)+
  geom_sf_text(mapping = aes(label = NUMELOC), data = loc, nudge_x = -.1, nudge_y = .089, size = 2.)+
  
  #geom_sf_text(data = filter(ctrs,name_ro !="Slovacia"), aes(label = name_ro), size = 3.5 ,fontface="italic")+
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  
  labs(x = "",y= "")+
  guides(fill = guide_colourbar(barwidth = 38.0, barheight = 0.8, title.position = "right",
                                label.theme = element_text(size = 10))) +
  theme_bw()+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"),
        legend.background = element_rect(fill = "white"))+
  #annotation_scale(location = "bl", style = "ticks", size = .5)+
  #annotation_custom(grob)+
  facet_wrap(~indicator, nrow =3, ncol = 3)

png(paste0(drive_z,"png/zile_strat_zapada_decenial_anual_1961-2020.png"), width =2000, height = 1600, res =220 )
pc1
dev.off()
system(paste0("convert -trim"," " ,drive_z,"/png/zile_strat_zapada_decenial_anual_1961-2020.png " ,drive_z, "/png/zile_strat_zapada_decenial_anual_1961-2020.png"))



