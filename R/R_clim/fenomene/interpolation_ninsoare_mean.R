###
library(raster,sp)
library(rgdal)


source("R/krige1_functii.R")
source("R/source_rasters_kriging.R")


tabs <- list.files(paste0(drive_z,"tab_export/decadal"), pattern = ".csv", full.names = T)
tabs <- grep("ZAPADA",tabs, value = T)

##incepe citirea

 for( i in 1:length(tabs)){
  
  per1 <- paste0(strsplit(tabs[i],"/|_")[[1]][18],"-",strsplit(tabs[i],"/|_")[[1]][19])
  print(per1) 
  t <- read.csv(tabs[i])

  t <- t[,c(grep(".decalat|nume|cod",colnames(t)))]
  t <- na.omit(t)
  ######### ###############prima zi############################## 

  tt.co <- merge(statii,t, by.x="COD", by.y="cod")

  coordinates(tt.co) = c("X", "Y")
  proj4string(tt.co) = CRS("+init=epsg:3844")
  ov<-over(tt.co,dem)

  #ds.co$ds_sat<-ov$ds_sat
  tt.co$alt<-ov$alt
  tt.co$lon<-ov$lon
  tt.co$lat<-ov$lat
  tt.co$sea_dist<-ov$sea_dist
  tt.co$twi<-ov$twi
  tt.co$focal_mean<-ov$focal_mean
  tt.co$focal_min<-ov$focal_min
  tt.co$adri_w_dist<-ov$adri_w_dist
  tt.co$adri_dist<-ov$adri_dist

  lm<-lm(prima.zi.decalat~alt+lon+lat+sea_dist+focal_mean+focal_min+adri_w_dist+adri_dist,tt.co)
  s.lm<-step(lm)
  summary(s.lm)$adj.r.squared
  print(paste(s.lm$call$formula)[3])

  dem2 <- brick(dem)
  r <- predict(dem2,s.lm)
  tt_reg <- as(r,"SpatialGridDataFrame")

  tt.co$res <- s.lm$residuals

  rbf_tt <- krige1(res~1,tt.co, dem, model=v)
  tt_reg@data[,'res'] <- rbf_tt[,1]
  tt_reg@data[,"tt"]<-tt_reg@data[,"layer"]+tt_reg@data[,'res']
  stfg<-tt.co[ tt.co$COD!= 431937,]
  max.tt <- max(stfg$prima.zi.decalat, na.rm=TRUE)
  min.tt <- min(stfg$prima.zi.decalat, na.rm=TRUE)
  tt_reg$tt <- ifelse(tt_reg$tt> max.tt, max.tt, tt_reg$tt )
  tt_reg$tt <- ifelse(tt_reg$tt < min.tt, min.tt, tt_reg$tt )

  rst <- raster(tt_reg["tt"])

  tt_reg_prj <- projectRaster(rst, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  writeRaster(tt_reg_prj,paste0(drive_z,"grids_export/decadal/prima_zi_medie_ninsoare_", per1, ".tif"))
 

###############################################################################################
###############################################################################################
###############################################################################################
####################################### ultima zi #############################################

tt.co <- merge(statii,t, by.x="COD", by.y="cod")
coordinates(tt.co) = c("X", "Y")
proj4string(tt.co) = CRS("+init=epsg:3844")
ov<-over(tt.co,dem)

#ds.co$ds_sat<-ov$ds_sat
tt.co$alt<-ov$alt
tt.co$lon<-ov$lon
tt.co$lat<-ov$lat
tt.co$sea_dist<-ov$sea_dist
tt.co$twi<-ov$twi
tt.co$focal_mean<-ov$focal_mean
tt.co$focal_min<-ov$focal_min
tt.co$adri_w_dist<-ov$adri_w_dist
tt.co$adri_dist<-ov$adri_dist

lm<-lm(ultima.zi.decalat~alt+lon+lat+sea_dist+focal_mean+focal_min+adri_w_dist+adri_dist,tt.co)
s.lm<-step(lm)
summary(s.lm)$adj.r.squared
print(paste(s.lm$call$formula)[3])

dem2 <- brick(dem)
r <- predict(dem2,s.lm)
tt_reg <- as(r,"SpatialGridDataFrame")

tt.co$res <- s.lm$residuals

rbf_tt <- krige1(res~1,tt.co, dem, model=v)
tt_reg@data[,'res'] <- rbf_tt[,1]
tt_reg@data[,"tt"]<-tt_reg@data[,"layer"]+tt_reg@data[,'res']
stfg<-tt.co[ tt.co$COD!= 431937,]
max.tt <- max(stfg$ultima.zi.decalat, na.rm=TRUE)
min.tt <- min(stfg$ultima.zi.decalat, na.rm=TRUE)
tt_reg$tt <- ifelse(tt_reg$tt> max.tt, max.tt, tt_reg$tt )
tt_reg$tt <- ifelse(tt_reg$tt < min.tt, min.tt, tt_reg$tt )

rst <- raster(tt_reg["tt"])

tt_reg_prj <- projectRaster(rst, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

writeRaster(tt_reg_prj,paste0(drive_z,"grids_export/decadal/ultima_zi_medie_ninsoare_",per1,".tif"))


}
