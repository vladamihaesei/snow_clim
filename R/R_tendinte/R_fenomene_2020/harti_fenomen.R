###
library(dplyr)
library(climatetools)
library(RColorBrewer)
library(raster,sp)
library(rgdal)
library(lubridate)
library(ncdf4)
library(sf)
source("~/D/Script_ANUAR/Anuar/harti_lunare_an/R/krige1_functii.R")
data(ws)
dem = readGDAL("~/D/Script_ANUAR/Anuar/harti_lunare_an/grids/dem.tif")
proj4string(dem) <- CRS("+init=epsg:3844")
rom <- readOGR("~/D/2019/vldZ/R_meteo/shp/ROU_adm/ROU_adm0.shp")
rom2 <- readOGR("~/D/2019/vldZ/R_meteo/shp/judet.shp")
statii <-read.csv( "~/D/Script_ANUAR/Anuar/harti_lunare_an/tabele/statii.csv")

##incepe citirea
tz <- read.csv("tab_export/prima_ultima_zi_fen_GROSZ_1961_2019_365_zile_v113.csv")
cale <- "tab_export/prima_ultima_zi_fen_GROSZ_1961_2019_365_zile_v113.csv"       
files.split <- do.call(rbind, strsplit(cale, "\\/|_|.csv"))
nume <- files.split[,7]
tzmnmx <- tz[,-c(grep(".med",colnames(tz)))]
tzmed <- tz[,c(grep(".med",colnames(tz)))]


### metoda 1 
tzmed.col <- NULL 
tzmnmx.col <- NULL
for (i in 1:ncol(tzmed)){
  
  print(names(tzmed[i])) 
  clmed <- as.Date(tzmed[,i],"%b%d")
  clmed <- format(clmed, "%j")
  clmed <- as.numeric(clmed)
  tzmed.col <- cbind(tzmed.col,clmed)

}
colnames(tzmed.col) <- colnames(tzmed)
for (j in 3:ncol(tzmnmx)){
  print(names(tzmnmx[j])) 
  clmnmx <- as.Date(tzmnmx[,j],"%Y%b%d")
  clmnmx <- format(clmnmx, "%j")
  clmnmx <- as.numeric(clmnmx)
  tzmnmx.col <- cbind(tzmnmx.col,clmnmx)
 }
colnames(tzmnmx.col) <- colnames(tzmnmx[3:6])
tz.rst <- as.data.frame(cbind(tz[1:2],tzmed.col,tzmnmx.col))

# 
# ### metoda 2 
# 
# sapply(1:ncol(tzmnmx), function(k) {
#   
#   tzmnmx[, k] <<- as.Date(tzmnmx[,k],"%Y%b%d")
#   tzmnmx[,k]<<- format(tzmnmx[,k], "%j")
# 
# })
# ###
# sapply(1:ncol(tzmed), function(k) {
#   
#   tzmed[, k] <<- as.Date(tzmed[,k],"%Y%b%d")
#   tzmed[,k]<<- format(tzmed[,k], "%j")
#   
# })

tz.cord <- merge(tz.rst,statii, by.x = "cod", by.y = "COD" )
coordinates(tz.cord) <- c("X","Y")
proj4string(tz.cord) <- CRS("+init=epsg:3844")#3844 4326

rbf1<-krige1(utlima.zi.med~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf1
gf1<-dem['a']
gf1 <- raster(gf1)
#gf1 <- crop(gf1,rom2)

rbf2<-krige1(utlima.zi.max~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf2
gf2<-dem['a']
gf2 <- raster(gf2)
#gf2 <- crop(gf2,rom2)

rbf3<-krige1(utlima.zi.min~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf3
gf3<-dem['a']
gf3 <- raster(gf3)
#gf3 <- crop(gf3,rom2)

rbf4<-krige1(prima.zi.med~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf4
gf4<-dem['a']
gf4 <- raster(gf4)
#gf4 <- crop(gf4,rom2)


rbf5<-krige1(prima.zi.max~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf5
gf5<-dem['a']
gf5 <- raster(gf5)
#gf5 <- crop(gf5,rom2)

rbf6<-krige1(prima.zi.min~1,tz.cord,dem,model=v)
dem@data[,'a']<-rbf6
gf6<-dem['a']
gf6 <- raster(gf6)
#gf6 <- crop(gf6,rom2)

st <- stack(gf1,gf2,gf3,gf4,gf5,gf6)

brks<-c(0,10,25,50,75,100,125,150,175,200,225,250,275,300,325,350,365,380)
cols <- c(brewer.pal(9,"YlOrRd"),rev(brewer.pal(9,"PuRd")))
#cols <-  c(brewer.pal(11,"Spectral"))
nb <- length(brks)-1
brk2<-seq(2,(length(brks)-1),1)
brk2<-brks[brk2]
arg <- list(at=brk2, labels=brk2)
# ##### 


tz.name.m <- merge(tz.rst,statii, by.x = "cod", by.y = "COD" ) 
tz.cord.df <- as.data.frame(tz.cord)
tz.cord.df <- merge(tz.cord.df,tz,by = "cod" )
tz.cord.df <- tz.cord.df[15:20]


names(st) <- c("ultima_zi_media","cea_mai_tarzie_ultima_zi", "cea_mai_timpurie_ultima_zi", "prima_zi_medie", "cea_mai_tarzie_prima_zi", "cea_mai_timpurie_prima_zi")
nrs <- names(st)

for (l in 1:nlayers(st)){
  print(l)
  sf <- st[[l]]
  nrss <- nrs[l]
  png(paste0("pngv6/",nume,"_365zile_v10",nrss,".png",sep=""), width = 2780, height = 2080, units = "px",res=200)
  plot(sf,breaks=brks, col=cols, lab.breaks=brks, horizontal=F,axes=F,axis.args=arg,interpolate=T,main=paste(nrss))
  plot(tz.cord,pch=19,cex=0.3,add=T)
  plot(rom2,add=T)
  text(coordinates(tz.cord)[,1],coordinates(tz.cord)[,2],as.character(tz.cord.df[,l]),1,cex=0.7,pos=3,offset=0.3)
  text(coordinates(tz.cord)[,1],coordinates(tz.cord)[,2],gsub(" "," \n ",tz.cord$nume),cex=0.55,pos=1,offset=0.2)
  dev.off()
  system(paste0("convert -trim ~/D/2020/Fenomene/pngv6/",nume,"_365zile_v10",nrss,".png ~/D/2020/Fenomene/pngv6/",nume,"_365zile_v10",nrss,".png",sep = ""))
}





