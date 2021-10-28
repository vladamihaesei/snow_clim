library(rgdal)
library(rgeos)
library(raster)
library(elevatr)


dem <- raster("~/Documente/vlad_R/mnt_ro/mnt_ro_fill_no_data.tif")
rom <- readOGR("/Volumes/Backup Plus/D/2019/vldZ/R_meteo/shp/ROU_adm_diva/ROU_adm0.shp")
rom <- st_as_sf(rom)
dem <- mask(dem,rom)


locations <- data.frame(X1 = c(20.5,29.4),
                        X2 = c(48, 44))

# get gem
dem <- get_elev_raster(locations = locations, prj = sf::st_crs(4326), z = 7, clip = "bbox")
plot(dem)
# create slope and hillshade
slope = terrain(dem, opt='slope')
aspect = terrain(dem, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
plot(hill)
dem_spdf <- as(dem, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("value", "x", "y")
hill_spdf <- as(hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")

