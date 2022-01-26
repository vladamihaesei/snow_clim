library(dplyr)
library(ggplot2)
library(lubridate)

t1 <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/prima_ultima_zi_fen_GROSZ_each_year_1961-2020_koeppen.csv")

t1.l <- t1 %>% pivot_longer(-c("nume","cod","Alt", "Lat", "Lon","Category",
                                          "Criter1","Criter2","Criter3","Start_year","End_year","Length_year",
                                          "prima_zi_jul_corect", "prima_zi_dat","dif_prima","ultima_zi_jul_corect",
                                          "ultima_zi_dat","dif_ultima"),names_to = "indicator")
t1.l <- t1.l%>% mutate(indicator = case_when(indicator == "prima_zi_jul_decalat"~ "FSD",
                                              indicator == "ultima_zi_jul_decalat"~ "LSD",
                                              indicator == "interval_zile"~ "SCD"))

png(filename = paste0(drive_z,"png/articol1/fig1_boxplots.png"), width = 1700, height = 1800, res = 250)
p2 <- ggplot(t1.l, aes(x=Category, y=value, fill = Category)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_grid(factor(Criter2, levels = rev(c("≤ 500 m", "500-1000 m", "1000-1500 m", "1500-2000 m", "2000-2500 m", "> 2500 m")))~factor(indicator, levels = c("FSD","LSD","SCD")), scale = "free")+
  xlab( "")+ylab("Julian days") +
  theme_bw()+
 theme(axis.text.x = element_text(size= 9),
       axis.text.y = element_text(size = 12),
       axis.title.y = element_text(size = 13),
       legend.title = element_blank())
p2
dev.off()
system(paste0("convert -trim ",drive_z, "png/articol1/fig1_boxplots.png  ", drive_z, "png/articol1/fig1_boxplots.png"))


####
t2 <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab_export/snow_cover_days_1961-2020.csv")
t1.sea <- t2%>% group_by(seas,ani,CODGE, NUME, Lat,Lon, Z, ex.category, Criter1, Criter2,Criter3)%>% summarise(Suma = sum(Suma))
t1.m.sea <- t1.sea%>% group_by(seas, CODGE, NUME, Lat,Lon,Z,ex.category, Criter1, Criter2, Criter3)%>%summarise(Media= mean(Suma))


t1.sea.kp <- t1.sea %>% group_by(seas, ani,ex.category,Criter2)%>%summarise(Mean = mean(Suma))%>%filter(seas != "JJA")

########### plot lines anual series time 
png(filename = paste0(drive_z,"png/articol1/fig_timeseries_scd.png"), width = 1700, height = 1800, res = 250)

p2 <- ggplot(t1.sea.kp, aes(x=ani, y=Mean, color = ex.category)) + 
  geom_line()+
  scale_color_manual(values=c("#fed966","#c8fd4e","#68fe31","#39c5fe","#027d81","#b1b2b1"))+
  facet_grid(factor(Criter2, levels = rev(c("≤ 500 m", "500-1000 m", "1000-1500 m", "1500-2000 m", "2000-2500 m", "> 2500 m")))~factor(seas, levels = c("SON","DJF","MAM")), scale = "free")+
  xlab("")+ylab("Number of days") +
  theme_bw()+
  theme(axis.text.x = element_text(size= 9),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13), 
        legend.title = element_blank())

p2
dev.off()

system(paste0("convert -trim ",drive_z, "png/articol1/fig_timeseries_scd.png  ", drive_z, "png/articol1/fig_timeseries_scd.png"))








