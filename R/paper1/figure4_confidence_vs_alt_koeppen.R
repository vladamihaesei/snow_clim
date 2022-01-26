library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)

pu <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/trend_GROSZ_prima_ultima_interval_1961-2020.csv")

pu.l <- pu%>%pivot_longer(
  cols = starts_with(c("slope")),
  names_to = c("ind"),
  names_prefix = c("wk"),
  values_to = c("value"),
  values_drop_na = TRUE)
pu.l1 <- pu.l%>%pivot_longer(
  cols = starts_with(c("sign")),
  names_to = c("ind1"),
  names_prefix = c("wk1"),
  values_to = c("value1"),
  values_drop_na = TRUE)

pu.l2 <- pu.l1%>%pivot_longer(
  cols = ends_with(c("mx")),
  names_to = c("ind2"),
  names_prefix = c("wk2"),
  values_to = c("value2"),
  values_drop_na = TRUE)

pu.l3 <- pu.l2%>%pivot_longer(
  cols = ends_with(c("mn")),
  names_to = c("ind3"),
  names_prefix = c("wk3"),
  values_to = c("value3"),
  values_drop_na = TRUE)

pu.l3 <- pu.l3%>%mutate(ind1 = case_when(ind == "slope.prima"~"FSD",
                                        ind == "slope.ultima"~"LSD",
                                        ind == "slope.interval"~"SCD"))

pu.l3.f <- pu.l3%>%filter(ex.category!= "BSk")

tvs <- pu.l3.f%>%ggplot(aes(value, Z, xmin =value2*10, xmax = value3*10, colour = ex.category))+
  geom_vline(xintercept = 0)+
  geom_pointrange(size = 0.12, fatten = 0.8)+
  scale_colour_manual(values=c("#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+ # "#fed966"
  
  facet_grid(factor(ex.category, levels = c("ET","Dfc","Dfb","Cfb","Cfa"))~factor(ind1, levels = c("FSD","LSD","SCD")),scales = "free", space = "free")+
  
  xlab("Linear trend in number of days [days per decade]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "none")

png(filename = paste0(drive_z,"png/articol1/fig4_trends_vs_elevation_grouping_koeppen.png"), width = 1500, height = 1850, res = 250)
tvs
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig4_trends_vs_elevation_grouping_koeppen.png  ", drive_z, "png/articol1/fig4_trends_vs_elevation_grouping_koeppen.png"))

pu.l3.f2 <- pu.l3%>%filter(ex.category == "BSk")

tvs1 <- pu.l3.f2%>%ggplot(aes(value, Z, xmin =value2*10, xmax = value3*10, colour = ex.category))+
  geom_vline(xintercept = 0)+
  geom_pointrange(size = 0.12, fatten = 0.8)+
  scale_colour_manual(values=c("#fed966"))+ # "#fed966"
  
  facet_grid(factor(ex.category, levels = c("BSk"))~factor(ind1, levels = c("FSD","LSD","SCD")),scales = "free", space = "free")+
  
  xlab("Linear trend in number of days [days per decade]")+ylab("Elevation [m]") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "none")

png(filename = paste0(drive_z,"png/articol1/fig4_trends_vs_elevation_grouping_BSk.png"), width = 1200, height = 750, res = 200)
tvs1
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig4_trends_vs_elevation_grouping_BSk.png  ", drive_z, "png/articol1/fig4_trends_vs_elevation_grouping_BSk.png"))

