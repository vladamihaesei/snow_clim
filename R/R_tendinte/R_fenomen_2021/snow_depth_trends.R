#load packages
library(sf) # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(tidyr)
library(terra)
library(trend)
library(raster)

source("R/cale_legenda_vectors.R")

##########listare rastere
rs <- list.files(paste0(drive_z,"grids_export/snowdepth"),recursive = T, full.names = T)
indice <- "snowdepth"
## verificare 
r <- brick(rs[1])

path <- paste0(drive_z,"grids_export/snowdepth/trends/",indice,"/")
if (!dir.exists(path)) dir.create(path, recursive = T)


for(s in 1:length(rs)){
  
  sez <- strsplit(rs[s],"/|_|.nc")[[1]][14]
  print(sez[s])
  
  r <- terra::rast(rs[s])
  crs(r) <- "+init=epsg:3844"
  
  newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  rr <- terra::project(r, newproj1)
  
  rr <- terra::mask(rr,vect(rom))

  rs.df <- as.data.frame(rr,xy=T) %>% pivot_longer(-c(x,y), names_to = "indicator") %>%na.omit()
  
  t_trend <- rs.df %>%
    group_by(x,y)%>% # we group by name and cod to perform the calculation in each station
    summarise(slope = sens.slope(value)$estimates *10,
              sign = mk.test(value)$p.value)%>%mutate(pval_0.05 = ifelse(sign <= 0.05, "sign","insig"),
                                                      pval_0.01 = ifelse(sign <= 0.01, "sign","insig"))%>%mutate(sez = sez)
  
  write.csv(t_trend,paste0(drive_z,"/tab_export/snowdepth/", indice,"_",sez, "_trend_1961-2020.csv"),row.names = F)
  
}

