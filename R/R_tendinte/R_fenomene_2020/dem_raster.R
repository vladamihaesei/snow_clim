library(rgdal)
library(rgeos)
library(raster)
library(elevatr)
library(rnaturalearth)
library(climatetools)
library(sf)
library(ggrepel)
library(ggnewscale)
locations <- data.frame(X1 = c(20.5,29.4),
                       X2 = c(48, 44))

dem <- get_elev_raster(locations = locations, prj = sf::st_crs(4326), z = 7, clip = "bbox")
plot(dem)

# create slope and hillshade
slope = terrain(dem, opt='slope')
aspect = terrain(dem, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)

dem_spdf <- as(dem, "SpatialPixelsDataFrame")
dem_spdf <- as.data.frame(dem_spdf)
colnames(dem_spdf) <- c("value", "x", "y")

hill_spdf <- as(hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")

# get rivers + lakes
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
lakes10 <- ne_download(scale = 10, type = 'lakes', category = 'physical')

# plot with different color scales
ggplot() +
  geom_tile(data = hill_spdf, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "black", high = "white") +
  new_scale_fill() +
  geom_tile(data = dem_spdf, aes(x = x, y = y, fill = value), alpha=0.4) +
  geom_path(data = rivers10,
            aes(long, lat, group = group), size = 1, color = '#000077') +
  geom_polygon(data = lakes10,
               aes(long, lat, group = group), size = 1, fill = '#000077', color = NA) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_continuous("", expand = c(0, 0)) +
  coord_sf(xlim = c(locations[1,1], locations[2,1]),
           ylim = c(locations[2,2], locations[1,2])) +
  theme_bw() +
  theme(legend.position="none")
