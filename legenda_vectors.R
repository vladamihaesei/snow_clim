library(dplyr)
library(sf)
library(png)
library(grid)
library(ggrepel)
library(ggspatial)
### label vecini
ctrs <-  read_sf("shp/countries.shp") %>% st_transform(4326)

ctrs <- ctrs %>% mutate(name_ro = case_when(sovereignt == "Ukraine"~"Ucraina",
                                            sovereignt == "Hungary"~"Ungaria",
                                            sovereignt == "Republic of Serbia"~"Serbia",
                                            sovereignt == "Bulgaria"~"Bulgaria",
                                            sovereignt == "Slovakia"~"Slovacia",
                                            sovereignt == "Moldova"~"Republica Moldova"))

box = c(xmin = 20, ymin = 43.4, xmax = 29.9, ymax = 48.3)
ctrs <- st_crop(ctrs,box)

### marea neagra
sea <-  read_sf("shp/sea.shp") %>% st_transform(4326)

###regiuni de dezvoltare
reg <- read_sf("shp/regiuni_ro_poligon.shp")
### orase resedinta de judet
loc <- read_sf("shp/localitati_diacritice/localitati.shp")
st_crs(loc) = 3844
loc <- loc %>% st_transform(4326)
###granite vecini
granite <- read_sf("shp/granite_masca.shp")


### scrire sursa

grob <- grobTree(textGrob("©MeteoRomania 2021", x=0.011,  y=0.062, hjust=0,
                          gp = gpar(col="black", fontsize=8.5, fontface="italic")))





