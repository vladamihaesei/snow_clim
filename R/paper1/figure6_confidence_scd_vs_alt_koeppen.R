library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)

tabs <- list.files("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export", pattern = "_scd_1961-2020_", full.names = T)
annual <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_anual_scd_1961-2020.csv")%>%mutate(seas= "Annual")
djf <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_DJF_scd_1961-2020.csv")%>%mutate(seas= "DJF")
mam <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_MAM_scd_1961-2020.csv")%>%mutate(seas= "MAM")
son <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_SON_scd_1961-2020.csv")%>%mutate(seas= "SON")

pu <- rbind(annual, djf,mam,son)

pu.f <- pu%>%filter(ex.category!= "BSk")

tvs <- pu.f%>%ggplot(aes(slope.scd, Z, xmin =mn, xmax = mx, colour = ex.category))+
  geom_vline(xintercept = 0)+
  geom_pointrange(size = 0.12, fatten = 0.8)+
  scale_colour_manual(values=c("#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+ # "#fed966"
  
  facet_grid(factor(ex.category, levels = c("ET","Dfc","Dfb","Cfb","Cfa"))~factor(seas, levels = c("SON","DJF","MAM","Annual")),scales = "free", space = "free")+
  
  xlab("Linear trend in number of days [days per decade]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "none")

png(filename = paste0(drive_z,"png/articol1/fig5_trends_scd_vs_elevation_grouping_koeppen.png"), width = 1500, height = 1850, res = 250)
tvs
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig5_trends_scd_vs_elevation_grouping_koeppen.png  ", drive_z, "png/articol1/fig5_trends_scd_vs_elevation_grouping_koeppen.png"))

pu.f2 <- pu%>%filter(ex.category == "BSk")

tvs1 <- pu.f2%>%ggplot(aes(slope.scd, Z, xmin =mn, xmax = mx, colour = ex.category))+
  geom_vline(xintercept = 0)+
  geom_pointrange(size = 0.12, fatten = 0.8)+
  scale_colour_manual(values=c("#fed966"))+ # "#fed966"
  
  facet_grid(factor(ex.category, levels = c("BSk"))~factor(seas, levels = c("SON","DJF","MAM","Annual")),scales = "free", space = "free")+
  
  xlab("Linear trend in number of days [days per decade]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "none")

png(filename = paste0(drive_z,"png/articol1/fig5_trends_scd_vs_elevation_grouping_BSk.png"), width = 1200, height = 750, res = 200)
tvs1
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig5_trends_scd_vs_elevation_grouping_BSk.png  ", drive_z, "png/articol1/fig5_trends_scd_vs_elevation_grouping_BSk.png"))

