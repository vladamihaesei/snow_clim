library(dplyr)
statii <- read.csv("tavg_ws_inventory_selection.csv")
t1 <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))
numeorig <- names(t1)[3]
colnames(t1)[3] <- "ZAPADA"
t1 <- t1 %>% left_join(nebt, by = c("COD", "DAT")) %>% left_join(tmin, by = c("COD", "DAT"))
### validare cu nebulozitate
t1$ZAPADA[is.na(t1$NEBT)] <- NA # 
t1$ZAPADA[t1$NEBT >= 0 && is.na(t1$ZAPADA)] <- 0 # asta e bine
# pentru validare cu temperatura
t1$ZAPADA[!is.na(t1$TMIN) & t1$TMIN >=5] <- 0
###
head(t1)
summary(t1)
# elimina Fundulea
t1 <- t1 %>% filter(COD != 428632) 
t1 <- t1 %>% filter(COD !=  428307)
#t1.sub <- t1[c(2137439),] # elimina data de 2012/mai24
t1 <- na.omit(t1)
names(t1)[1] <- "CODGE"
summary(t1)

t1$DAT <- as.Date(t1$DAT)

ninsoare <- statii[c(2,3,4,5,6,8,9,10,11)] %>% left_join(t1[c(1,2,3,5)], by = c("CODGE")) ### precipitatii solide

ninsoare <- ninsoare%>%left_join(t1.cluster)%>% na.omit()

ninsoare.an <- ninsoare %>% mutate(caz = ifelse(ZAPADA >0,1,0))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(anual = sum(caz))
write.csv(ninsoare.an, paste0(drive_z,"tab_export/nr_zile_strat_zapada.csv"))
ninsoare.11 <- ninsoare %>%filter(format(DAT,"%m") == "11")%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(noiembrie = sum(caz))

ninsoare.12 <- ninsoare %>%filter(format(DAT,"%m") == "12") %>%mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(decembrie = sum(caz))

ninsoare.01 <- ninsoare %>%filter(format(DAT,"%m") == "01")%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(ianuarie = sum(caz))

ninsoare.02 <- ninsoare %>%filter(format(DAT,"%m") == "02")%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(februarie = sum(caz))

ninsoare.03 <- ninsoare %>%filter(format(DAT,"%m") == "03")%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(martie = sum(caz))
ninsoare.04 <- ninsoare %>%filter(format(DAT,"%m") == "04")%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(aprilie = sum(caz))



ninsoare.djf <- ninsoare %>%filter(format(DAT,"%m") %in% c("12","01","02"))%>% mutate(caz = ifelse(ZAPADA >0,1,0),
                                                                                      luna = format(DAT,"%m"))%>%group_by(format(DAT,"%Y"),CODGE, NUME,Z, Lat,Lon) %>% summarise(DJF = sum(caz))


ninsoare.acum <- ninsoare.an %>%left_join(ninsoare.11)%>%left_join(ninsoare.12)%>%left_join(ninsoare.01)%>%left_join(ninsoare.02)%>%left_join(ninsoare.03)%>%left_join(ninsoare.04)%>%left_join(ninsoare.djf)
names(ninsoare.acum)[1] <- "Data"

write.csv(ninsoare.acum, paste0(drive_z,"tab_export/zile_strat_zapada_1961-2020.csv"),row.names = F)
ninsoare.l <- ninsoare.acum %>%pivot_longer(-c(Data,CODGE,NUME,Z,Lat,Lon),names_to = "indicator")%>%na.omit()%>%group_by(Data,indicator)%>%summarise(med.statii = mean(value))
ninsoare.l <- ninsoare.l%>%filter(Data != "1961")
write.csv(ninsoare.l, paste0(drive_z,"tab_export/zile_strat_zapada_total_longer.csv"))

`%notin%` <- Negate(`%in%`)

png(paste0(drive_z,"png/temporal_evolution_strat_zapada_monthly_1961_2020.png"), width = 2400, height = 1600, res = 200)
ggplot(data = filter(ninsoare.l,indicator %notin% c("DJF","anual")), aes(x= as.numeric(Data), y = med.statii)) + 
  geom_line(size=1) + geom_smooth(method = "lm")+theme_bw()+
  scale_x_continuous(name="", breaks = seq(1961,2021,by = 10)) + scale_y_continuous(name = "zile")+
  #annotate("rect", xmin = 2011, xmax = 2017, ymin = 8.23, ymax = 9.24, fill="grey", colour="blue") +
  #annotate("text", x= 2014, y=9, label = "R^2 == 0.025",size= 4.6, parse=T) + annotate("text", x= 2014, size= 4.6,y=8.6, label = "f(x) = -0.0268 + 2.0849", parse=F) +
  facet_wrap(~factor(indicator, levels = c("noiembrie","decembrie","ianuarie","februarie","martie", "aprilie")))
dev.off()

png(paste0(drive_z,"png/temporal_evolution_strat_zapada_season_annual_1961_2020.png"), width = 2200, height = 1600, res = 200)
ggplot(data = filter(ninsoare.l,indicator %in% c("DJF","anual")), aes(x= as.numeric(Data), y = med.statii)) + 
  geom_line(size=1) + geom_smooth(method = "lm")+theme_bw()+
  scale_x_continuous(name="", breaks = seq(1961,2021,by = 10)) + scale_y_continuous(name = "zile")+
  #annotate("rect", xmin = 2011, xmax = 2017, ymin = 8.23, ymax = 9.24, fill="grey", colour="blue") +
  #annotate("text", x= 2014, y=9, label = "R^2 == 0.025",size= 4.6, parse=T) + annotate("text", x= 2014, size= 4.6,y=8.6, label = "f(x) = -0.0268 + 2.0849", parse=F) +
  facet_wrap(~factor(indicator))
dev.off()





