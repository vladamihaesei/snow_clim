library(RODBC)
library(climatetools)

dat1 <- "1999-01-01"
dat2 <- "2015-12-31" 

channel<- odbcConnect(dsn="ORACLE2",believeNRows=F)

channel <- odbcConnect("ORACLE2", uid = "dezvoltare", pwd = "zed#$345")
t1 <- paste("SELECT CLIMAZIS.COD, CLIMAZIS.DAT, CLIMAZIS.TMED FROM CLIMA.CLIMAZIS CLIMAZIS WHERE  (CLIMAZIS.DAT>={ts '",dat1," 00:00:00'} And CLIMAZIS.DAT<={ts '",dat2,"23:00:00'})ORDER BY CLIMAZIS.DAT")
t1 <- sqlQuery(channel,t1,as.is = T)

odbcClose(channel)
### fundulea 
t1.s <- t1%>%filter(COD == 428632)


write.csv(t1.s,"tab_export/tmed_fundulea_zilnice_1999-2015.csv", row.names = F)

write.csv(t1,"~/Y/monitorizare/MMOISE/extreme_zapada/GROSZ_1961-2020.csv", row.names = F)


f <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/ECHIVZAP_1961-2020.csv")
f$ECHIVZAP <- as.numeric(f$ECHIVZAP)
f$ECHIVZAP[f$ECHIVZAP== 0] <- NA
f <- na.omit(f)
summary(f)



dat1 <- "1961-01-01 00:00:00"
dat2 <- "2021-07-31 23:00:00" 

channel<- odbcConnect(dsn="ORACLE2",believeNRows=F)
t1 <-paste("SELECT CLIMALU.COD,CLIMALU.DAT,  CLIMALU.GROSZ FROM CLIMA.CLIMALU CLIMALU WHERE  ((CLIMALU.DAT>={ts '", dat1,"'} And CLIMALU.DAT<={ts '",dat2,"'})) ORDER BY  CLIMALU.DAT")
t1 <- sqlQuery(channel,t1,as.is = T)
odbcClose(channel)
#t1 <- read.csv("Tab/Agraasigurari_202007/1-75_Precipitatiilunare2015.csv")

write.csv(t1,"~/Y/monitorizare/MMOISE/extreme_zapada/GROSZ_lunare_1961-202107.csv", row.names = F)

