library(sf) # Simple Features for R
library(here) # A Simpler Way to Find Your Files
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(RColorBrewer)
#library(hrbrthemes)
library(ggspatial)
library(png)
library(cowplot)
library(magick)
library(tidyr)
library(terra)
library(trend)
library(raster)
library(ggpattern)

source("R/cale_legenda_vectors.R")
tmm <- list.files(paste0(drive_z,"/tab_export/snowdepth"), full.names = T)

tabs <- rbind(read.csv(tmm[1]),read.csv(tmm[2]), read.csv(tmm[4]),read.csv(tmm[5]),read.csv(tmm[7]),read.csv(tmm[9])) %>% na.omit()

head(tabs)
indice <- "snowdepth"
#### incepe resample
sezon <- c("Noiembrie","Decembrie","Ianuarie","Februarie", "Martie","Aprilie")
ex <- NULL

for( t in 1:length(sezon)){
  
  seas <- sezon[t]
  
  dd <- tabs %>% filter(sez== seas)
  rs.pval <- rasterFromXYZ(dd[c(1,2,4)],)
  sr <- raster(nrow = 100, ncol = 100)
  extent(sr) <- extent(rs.pval)
  sr <- resample(rs.pval, sr, method = 'bilinear') # resample output
  
  ##########################
  df.s <- as.data.frame(sr, xy =T)%>%na.omit()%>%mutate(sez = seas)
  ex <- rbind(df.s,ex)
  
}

#### pentru legenda
rmean <- colorRampPalette(brewer.pal(11,"RdBu"), interpolate="linear")
brks.mean <- seq(-10,10.5, by = .5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-10.5,11)
den <- "cm/10 ani"

path1 <- paste0(drive_z,"/png/",indice,"/")
if (!dir.exists(path1)) dir.create(path1, recursive = T)


####
#`%notin%` <- Negate(`%in%`)

g <- ggplot() +
  
  geom_raster(data= tabs, aes(x = x, y = y, fill= slope), interpolate = F)+
  
  geom_point(data = filter(ex, sign <= 0.05),aes(x = x, y = y),color = "#81FC07",
             size = 0.3,pch = 4, alpha = 0.55, show.legend = F)+
  
  geom_sf(data = judete ,color = "grey", fill = "transparent", size= 0.45)+
  
  geom_sf(data = masca, color = "black", fill = "lightgrey", size = 0.1)+
  
  geom_sf(data = granite, color = "black", fill = "grey", size = 0.1)+
  
  geom_sf(fill = "#a4b9b9", data = sea, color = "lightgrey", lwd = 0.4)+
  
  #new_scale_colour()+
  
  #scale_color_manual(values = c("8BFD02"), name = "")+
  
  geom_sf(aes(geometry = geometry), data = loc,pch = 20,  size = 2.6, show.legend = F)+
  
  #geom_sf(aes(geometry = geometry), data = filter(loc, NUMELOC %notin% c("BUCURESTI", "Ploiești")), pch = 20, bg = "black", size = 1.9, show.legend = F)+
  
  #geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc, NUMELOC != "Ploiești"), nudge_x = -.1, nudge_y = .089, size = 3.2)+
  
  #geom_sf_text(mapping = aes(label = NUMELOC), data = filter(loc,NUMELOC == "Ploiești"), nudge_x = -.1, nudge_y = -.089, size = 3.2)+
  
  
  coord_sf(xlim = c(20,29.9), ylim = c(43.5, 48.3), expand = F)+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  
  scale_fill_stepsn(colours = cols.mean, name = den,
                    breaks = brks.mean,
                    limits =lim.mean) +                   
  guides(fill = guide_colourbar(barwidth = 64.0, barheight = 0.9, title.position = "right",
                                label.theme = element_text(size = 10.5))) +
  
  scale_linetype_manual(values=c("twodash")) +
  annotation_scale(location = "bl", style = "ticks")+
  labs(x = "",y= "")+
  theme(legend.position = "bottom",
        
        strip.background = element_rect(colour = "black", fill = "white"),
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"),
        legend.background = element_rect(fill = "white"),
        legend.title=element_text(size=14.5), 
        legend.text=element_text(size=17.5),text = element_text(size=17.5)
  )+facet_wrap(~factor(sez, levels = c("Noiembrie","Decembrie","Ianuarie","Februarie", "Martie","Aprilie")), nrow  = 2, ncol = 3)


#+annotation_custom(logo.g,xmin = 20.22,xmax = 20.85,ymin = 47.65,ymax = 48.35)
# my_plot_2 <- ggdraw() +
#   draw_plot(g)+
#   draw_image(logo1, x= 0.18,y = -0.15, hjust = 0.29, vjust= 0.21, halign= 0.19, valign= .43, scale = 0.057)

png(paste0(path1,indice,"_","sezon_trends_1961-2020",".png"), height = 3800 , width = 3400,units = "px", res = 230)
g
dev.off()
system(paste0("convert -trim ",path1,indice,"_","sezon_trends_1961-2020",".png  ",path1,indice,"_","sezon_trends_1961-2020",".png",sep = ""))

