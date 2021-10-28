library(rnaturalearth)
library(raster,sp)
library(rgdal)
library(sf)

statii<-read.table("tab/statii.csv", header=TRUE, sep=",", na.strings="NA", strip.white=TRUE)
dem= readGDAL("grids1/dem.tif")
dem$alt=dem$band1
dem$focal_mean <- readGDAL("grids1/focal_mean.tif")$band1
dem$focal_min <- readGDAL("grids1/focal_min.tif")$band1
dem$lat <- readGDAL("grids1/lat.tif")$band1
dem$lon <- readGDAL("grids1/lon.tif")$band1
dem$sea_dist <- readGDAL("grids1/sea_dist.tif")$band1
dem$twi<-readGDAL("grids1/TWI.asc")$band1
dem$adri_w_dist<-readGDAL("grids1/adri_w_dist.asc")$band1
dem$adri_dist<-readGDAL("grids1/adri_dist.asc")$band1
dem$band1<-NULL
proj4string(dem) = CRS("+init=epsg:3844")


romania <- rnaturalearth::ne_countries(scale = 10,type = "countries", country = c("Romania"),returnclass = "sf")
judete <- readOGR("shp/ROU_adm/Judete.shp")
judete <- spTransform(judete, CRSobj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
judete <- st_as_sf(judete)

