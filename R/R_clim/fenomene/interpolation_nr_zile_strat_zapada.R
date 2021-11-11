library(dplyr)

t1 <- read.csv(paste0(drive_z,"tab_export/zile_strat_zapada_1961-2020.csv"))
t1 <- t1 %>% pivot_longer(-c(Data,CODGE,NUME,Z,Lat,Lon), names_to = "indicator")
st <- c(1961,1971,1981,1991,2001,2011)
end <- c(1970,1980,1990,2000,2010,2019)

for( k in 1:length(st)){
  
  t1.med <- t1%>% filter(Data >= st[k] & Data <= end[k]) %>% group_by(CODGE,NUME,Z,Lat,Lon,indicator)%>% summarise(med.an = mean(value))
  write.csv(t1.med, paste0(drive_z,"tab_export/nr_zile_strat_zapada_",st[k],"-", end[k], ".csv"), row.names = F)
  
  
}


t1.med <- t1 %>% group_by(CODGE,NUME,Z,Lat,Lon,indicator) %>% summarise(med.an = mean(value))
write.csv(t1.med, paste0(drive_z,"tab_export/nr_zile_ninsoare_",st[k],"-", end[k], ".csv"), row.names = F)


tabs <- list.files(paste0(drive_z,"tab_export"), pattern = ".csv", full.names = T)
tabs <- grep("nr_zile_strat_zapada", tabs, value = T)
tabs <- tabs[1:6]
tabs
ind <- unique(t1.med$indicator)
for( i in 1:length(tabs)){
  
  per1 <- strsplit(tabs[i],"/|_|.csv")[[1]][16]
  print(per1) 
  t <- read.csv(tabs[i])
  
  for( j in 1:length(ind)){
    
    t.f <- t %>% filter(indicator == ind[j])
    t.f <- na.omit(t.f)
    
    tt.co <- left_join(statii,t.f)
    tt.co <- na.omit(tt.co)
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
    
    lm<-lm(med.an~alt+lon+lat+sea_dist+focal_mean+focal_min+adri_w_dist+adri_dist,tt.co)
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
    stfg<-tt.co[ tt.co$CODGE!= 431937,]
    max.tt <- max(stfg$med.an, na.rm=TRUE)
    min.tt <- min(stfg$med.an, na.rm=TRUE)
    tt_reg$tt <- ifelse(tt_reg$tt> max.tt, max.tt, tt_reg$tt )
    tt_reg$tt <- ifelse(tt_reg$tt < min.tt, min.tt, tt_reg$tt )
    
    rst <- raster(tt_reg["tt"])
    
    tt_reg_prj <- projectRaster(rst, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    writeRaster(tt_reg_prj,paste0(drive_z,"grids_export/decadal/nr_zile_strat_zapada_",ind[j],"_", per1, ".tif"))
    
  }
  
}

