library(raster)
library(terra)
library(sf)


dem <-terra::rast(paste0(drive_z,"grids1/dem.tif"))
crs(dem) <- "+init=epsg:3844"
newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"

dem <- terra::project(dem, newproj1)

dem <- terra::mask(dem,terra::vect(rom))
dem <- raster(dem)
dem.df <- as.data.frame(dem, xy =T)
colnames(dem.df) <- c("x","y","meters")
tabs <-  read.csv(paste0(drive_z,"tab_export/prima_ultima_zi_fen_ZAPADA_each_year_1961-2020.csv"))

slope = raster::terrain(dem, opt='slope')
aspect = raster::terrain(dem, opt='aspect')
hill = raster::hillShade(slope, aspect, 40, 270)

hill_df <- as.data.frame(hill, xy = T)%>%na.omit()