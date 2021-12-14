#1949/3/22 Brasov eroare 200
library(dplyr)
library(tidyverse)
library(climatetools)
library(magick)
library(RColorBrewer)
library(cowplot)
library(ggthemes)
library(lubridate)


df.statii <- read.csv("~/Y/monitorizare/MMOISE/extreme_zapada/outputs/zilnice_grosz_provincie_1880-2020.csv")

df.statii <- df.statii %>%filter(dat!= "1949-03-22 00"|NUME!="Brașov")

df.statii <- df.statii %>%filter(dat!= "1929-01-14 00"|NUME!="Cotnari")
df.statii <- df.statii %>%filter(dat!= "1959-11-09 00"|NUME!="Botoșani")
df.statii <- df.statii %>%filter(dat!= "1941-01-29 00"|NUME!="Câmpina")
df.statii <- df.statii %>%filter(dat!= "1954-02-24 00"|NUME!="București-Băneasa")

df.statii <- df.statii %>%mutate(criter1 = ifelse(Z>1000,">1000","<1000"))


t.tara <- df.statii%>%na.omit()%>% group_by(cod)%>%filter(grosz == max(grosz))
t.tara <-t.tara%>%filter(grosz >0)

t.tara <- t.tara%>%arrange(desc(grosz))
t.tara$dat <- as.Date(t.tara$dat)
t.tara <- t.tara %>% mutate(An = year(dat), Luna = month(dat), Zi = day(dat))

t.tara <- t.tara[c(3,2,7,6,12,13,14,5,8,9,10)]

write.csv(t.tara,"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_tara.csv", row.names = F)

### pentru primele 5 valori
res <- df.statii%>%na.omit%>%group_by(cod)%>%arrange(desc(grosz))%>%slice(1:5)%>%distinct(grosz,Provincie,NUME)
res1 <- df.statii%>%na.omit%>%group_by(cod)%>%arrange(desc(grosz))%>%slice(1:15)%>%distinct()

write.csv(res,"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_5_valori_tara.csv", row.names = F)
write.csv(res1,"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_5_valori_tara_varianta2.csv", row.names = F)

#### max absolute pe regiuni istorice si altitudine 
df.statii <- df.statii %>%mutate(criter1 = ifelse(Z>1000,">1000","<1000"))

t10 <- df.statii%>% na.omit()%>% group_by(Provincie,criter1) %>% filter(grosz == max(grosz))

t10 <- t10%>%arrange(desc(Provincie))
t10$dat <- as.Date(t10$dat)
t10 <- t10 %>% mutate(An = year(dat), Luna = month(dat), Zi = day(dat))

t10 <- t10[c(3,2,7,6,12,13,14,5,8,9,10)]

write.csv(t10, "~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_provincii_altitudine.csv",row.names = F)

### primele 5 valori 
prim5 <- df.statii%>%na.omit%>%group_by(Provincie,criter1)%>%arrange(desc(grosz))%>%slice(1:15)%>%distinct(grosz,NUME)

prim5 <- prim5%>%arrange(desc(Provincie,criter1))
prim5
write.csv(prim5, "~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_5_valori_provincii_altitudine.csv",row.names = F)


prim51 <- df.statii%>%na.omit%>%group_by(Provincie,criter1)%>%arrange(desc(grosz))%>%slice(1:15)%>%distinct()

write.csv(prim51[3:11],"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_5_valori_provincie_altitudine_varianta2.csv", row.names = F)

############

t100 <- df.statii%>% na.omit()%>% group_by(Provincie,criter1) %>% filter(grosz >= 100 & Z<1000)

t100 <- t100%>%arrange(desc(Provincie))
t100$dat <- as.Date(t100$dat)
t100 <- t100 %>% mutate(An = year(dat), Luna = month(dat), Zi = day(dat))

t100 <- t100[c(6,7,10,5,12,13,14)]

t100$Provincie <- toupper(t100$Provincie)
t100$NUME <- toupper(t100$NUME)

write.csv(t100,"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_100cm_campie.csv", row.names = F)


t250 <- df.statii%>% na.omit()%>% group_by(Provincie,criter1) %>% filter(grosz >= 300 & Z>1000)

t250 <- t250%>%arrange(desc(Provincie))
t250$dat <- as.Date(t250$dat)
t250 <- t250 %>% mutate(An = year(dat), Luna = month(dat), Zi = day(dat))

t250 <- t250[c(6,7,10,5,12,13,14)]

t250$Provincie <- toupper(t250$Provincie)
t250$NUME <- toupper(t250$NUME)

write.csv(t250,"~/Y/monitorizare/MMOISE/extreme_zapada/outputs/maxime_absolute_300cm_munte.csv", row.names = F)



