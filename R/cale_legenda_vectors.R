library(dplyr)
library(sf)
library(png)
library(grid)

# cale variabila in functie de sistemul de operare
# pentru mac creeaz link pe user cu folderul Z din terminal
# cd ~
# ln -s /Volumes/ShareCWaters/
# ln -s /Volumes/clima/

drive_z <- ifelse(Sys.info()[1] == "Darwin", "/Volumes/Z/Mac_book/Teza_doctorat/Zapada_doctorat/", "~/D/2021/Date_doctorat/Zapada_doctorat/")

#### vectori pentru pentru layout
loc <- read_sf(paste0(drive_z,"shp/localitati_diacritice/localitati.shp"))
st_crs(loc) <- 3844
loc <- loc %>% st_transform(4326)
rom <- read_sf(paste0(drive_z, "shp/ROU_adm/ROU_adm0.shp")) %>% st_transform(4326)
### label vecini
ctrs <-  read_sf(paste0(drive_z,"shp/countries.shp")) 

ctrs <- ctrs %>% mutate(name_ro = case_when(sovereignt == "Ukraine"~"Ucraina",
                                            sovereignt == "Hungary"~"Ungaria",
                                            sovereignt == "Republic of Serbia"~"Serbia",
                                            sovereignt == "Bulgaria"~"Bulgaria",
                                            sovereignt == "Slovakia"~"Slovacia",
                                            sovereignt == "Moldova"~"Republica Moldova"))

box = c(xmin = 20, ymin = 43.4, xmax = 29.9, ymax = 48.3)
ctrs <- st_crop(ctrs,box)

### marea neagra
sea <-  read_sf(paste0(drive_z,"shp/sea.shp")) %>% st_transform(4326)

###granite vecini
granite <- read_sf(paste0(drive_z,"shp/granite_masca.shp"))

### scrire sursa

grob <- grobTree(textGrob("©MeteoRomania 2021", x=0.011,  y=0.062, hjust=0,
                          gp = gpar(col="black", fontsize=8.5, fontface="italic")))





