library(raster)
library(terra)

path <- paste0(drive_y, "snowball/nc/observatii/lunare_nc/grosz/grosztime_198101-202004.nc")

out <- paste0(drive_z,"grids_export/snowdepth/")
system(paste0("cdo yearmean -selmon,11,12,01,02,03,04 ", path," ", out,"grosz_NDJFMA.nc"))
system(paste0("cdo yearmean -selmon,12,01,02 ", path, " ", out,"grosz_DJF.nc"))
system(paste0("cdo yearmean -selmon,03,04 ", path, " ", out,"grosz_MA.nc"))

system(paste0("cdo yearmean -selmon,11 ", path," ", out,"grosz_Noiembrie.nc"))
system(paste0("cdo yearmean -selmon,12 ", path, " ", out,"grosz_Decembrie.nc"))
system(paste0("cdo yearmean -selmon,01 ", path, " ", out,"grosz_Ianuarie.nc"))
system(paste0("cdo yearmean -selmon,02 ", path, " ", out,"grosz_Februarie.nc"))
system(paste0("cdo yearmean -selmon,03 ", path, " ", out,"grosz_Martie.nc"))
system(paste0("cdo yearmean -selmon,04 ", path, " ", out,"grosz_Aprilie.nc"))


### verificare 

r <- brick(path)
r


