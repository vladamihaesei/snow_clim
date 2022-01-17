library(dplyr)
library(raster)
library(terra)
library(sf)
library(trend)

t1 <- read.csv( "~/D/2021/Date_doctorat/Zapada_doctorat/tab/ws_climatetools_provincii_NAomit.csv")

kpn <- terra:: rast("grids/rocada_kp_1991-2020_proj_stereo.tif")
crs(kpn) <- "+init=epsg:3844"

newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"

kpn.pr <- terra::project(kpn, newproj1)

coordinates(t1) <- ~Lon+Lat
t1.sf <- st_as_sf(t1)

#t1$ex <- exactextractr::exact_extract(kpn.pr,t1)
t1.sf$ex <-terra::extract(kpn.pr,vect(t1.sf))
tt <- as.data.frame(t1.sf)


tt <- tt%>%mutate(Criter1 = case_when(Z <= 200~ "≤ 200 m",
                                  Z > 200 & Z <= 500~ "200-500 m",
                                  Z > 500 & Z <= 800~ "500-800 m",
                                  Z > 800 & Z <= 1100~ "800-1100 m",
                                  Z > 1100 & Z <= 1500~ "1100-1500 m",
                                  Z > 1500 & Z <= 2000~ "1500-2000 m",
                                  Z > 2000~ "> 2000 m"
),
Criter2 = case_when(Z <= 500~ "≤ 500 m",
                 Z > 500 & Z <= 1000 ~ "500-1000 m",
                 Z > 1000 & Z <= 1500~ "1000-1500 m",
                 Z > 1500 & Z <= 2000~ "1500-2000 m",
                 Z > 2000 & Z <= 1500~ "2000-2500 m",
                 Z > 2500~ "> 2500 m"
                 
),
Criter3 = case_when(Z <= 300~ "≤ 300 m",
                 Z > 300 & Z <= 500 ~ "300-500 m", 
                 Z > 500 & Z <= 800 ~ "500-800 m",
                 Z > 800 & Z <= 1200~ "800-1200 m",
                 Z > 1200 & Z <= 1500~ "1200-1500 m",
                 Z > 1500 & Z <= 1800~ "1500-1800 m",
                 Z > 1800 & Z <= 2200~ "1500-2200 m",
                 Z > 2200~ "> 2200 m")
)

tt[12:15]<- NULL

write.csv(tt,paste0(drive_z,"tab_export/ws_statii_koeppen.csv"))




                  
                  
