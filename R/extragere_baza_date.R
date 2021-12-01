library(RODBC)

dat1 <- "1961-01-01"
dat2 <- "2021-07-31" 

channel<- odbcConnect(dsn="ORACLE2",believeNRows=F)

channel <- odbcConnect("ORACLE2", uid = "dezvoltare", pwd = "zed#$345")
t1 <- paste("SELECT CLIMAZIS.COD, CLIMAZIS.DAT, CLIMAZIS.GROSZ FROM CLIMA.CLIMAZIS CLIMAZIS WHERE  (CLIMAZIS.DAT>={ts '",dat1," 00:00:00'} And CLIMAZIS.DAT<={ts '",dat2,"23:00:00'})ORDER BY CLIMAZIS.DAT")
t1 <- sqlQuery(channel,t1,as.is = T)

odbcClose(channel)

write.csv(t1,"~/D/2021/Date_doctorat/Zapada_doctorat/tab/GROSZ_1961-2020.csv", row.names = F)

write.csv(t1,"~/Y/monitorizare/MMOISE/extreme_zapada/GROSZ_1961-2020.csv", row.names = F)


f <- read.csv("~/D/2021/Date_doctorat/Zapada_doctorat/tab/ECHIVZAP_1961-2020.csv")
f$ECHIVZAP <- as.numeric(f$ECHIVZAP)
f$ECHIVZAP[f$ECHIVZAP== 0] <- NA
f <- na.omit(f)
summary(f)
