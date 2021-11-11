rr <- list.files(paste0(drive_z,"grids_export/decadal"), pattern = ".tif", full.names = T)
rr <- grep("prima", rr, value = T)
rr <- grep("strat", rr, value = T)


rst <- terra::rast(rr)
names(rst) <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010","2011-2020")

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

judete <- read_sf(paste0(drive_z, "shp/ROU_adm/ROU_adm0.shp")) %>% st_transform(4326)
rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- plt%>% pivot_longer(-c(x,y), names_to = "indicator")
plt <- na.omit(plt)
names(plt)[4] <- "tt"

rand <- plt %>% filter(indicator=="X1971.1980")

plt.f <- plt %>% mutate(caz1 = ifelse(tt < 62,1,0),
                        caz2 = ifelse(tt > 61 & tt <= 81,1,0),
                        caz3 = ifelse(tt > 81 & tt <= 112,1,0),
                        caz4 = ifelse(tt > 112 & tt <= 132,1,0),
                        caz5 = ifelse(tt > 132 & tt <= 152,1,0),
                        caz6 = ifelse(tt > 152,1,0)) %>% group_by(indicator)%>% summarise(f_30_Sep = (sum(caz1)/nrow(rand)*100),
                                                                                      f_01_20_Oct = (sum(caz2)/nrow(rand)*100),
                                                                                      f_21_10_Nov = (sum(caz3)/nrow(rand)*100),
                                                                                      f_11_30_Nov = (sum(caz4)/nrow(rand)*100),
                                                                                      f_01_20_Dec = (sum(caz5)/nrow(rand)*100),
                                                                                      f_20_Dec = (sum(caz6)/nrow(rand)*100)
                                                                                      )
frec.l <- plt.f %>% pivot_longer(-c(indicator), names_to = "frecventa")
frec.l$indicator <- gsub("X","",frec.l$indicator)

frec.l <- frec.l %>% mutate(frecventa = case_when(frecventa == "f_30_Sep" ~ "< 30 Sep",
                                                  frecventa == "f_01_20_Oct" ~ "01-20 Oct",
                                                  frecventa == "f_21_10_Nov" ~ "21 Oct-10 Nov",
                                                  frecventa == "f_11_30_Nov" ~ "11-30 Nov",
                                                  frecventa == "f_01_20_Dec" ~ "01- 20 Dec",
                                                  frecventa == "f_20_Dec" ~ "> 20 Dec"))
p <- ggplot(data = frec.l,aes(x = factor(frecventa, levels = c("< 30 Sep","01-20 Oct","21 Oct-10 Nov","11-30 Nov","01- 20 Dec","> 20 Dec")), y = value, fill = factor(indicator)))+
     geom_bar(stat = "identity", position = position_dodge())+
     scale_fill_brewer( name = "", palette = "Spectral")+
     labs(x = " ", y = "Frecventa[%]")+ theme_minimal()+theme(axis.text.y = element_text(size = 18),
                                                              axis.text.x = element_text(size = 14),
                                                              axis.title =   element_text(size = 15, face = "bold"),
                                                              axis.text = element_text(size = 19))

png(paste0(drive_z,"png/frecventa_decadal_prima_zi_strat_zapada_1961_2020.png"), width = 2000, height = 1600, res = 200)
p
dev.off()




rr <- list.files(paste0(drive_z,"grids_export/decadal"), pattern = ".tif", full.names = T)
rr <- grep("ultima", rr, value = T)
rr <- grep("strat", rr, value = T)


rst <- terra::rast(rr)
names(rst) <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000","2001-2010","2011-2020")

tt_reg_prj <- terra::project(rst, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

judete <- read_sf(paste0(drive_z, "shp/ROU_adm/ROU_adm0.shp")) %>% st_transform(4326)
rst.cr <- terra::mask(tt_reg_prj,vect(judete))

## transform data frame 
plt <- as.data.frame(rst.cr, xy =T)
plt <- plt%>% pivot_longer(-c(x,y), names_to = "indicator")
plt <- na.omit(plt)
names(plt)[4] <- "tt"

rand <- plt %>% filter(indicator=="X1971.1980")

plt.f <- plt %>% mutate(caz1 = ifelse(tt < 214,1,0),
                        caz2 = ifelse(tt > 214 & tt <= 234,1,0),
                        caz3 = ifelse(tt > 234 & tt <= 254,1,0),
                        caz4 = ifelse(tt > 254 & tt <= 274,1,0),
                        caz5 = ifelse(tt > 274 & tt <= 294,1,0),
                        caz6 = ifelse(tt > 294,1,0)) %>% group_by(indicator)%>% summarise(f_30_Mar = (sum(caz1)/nrow(rand)*100),
                                                                                          f_01_20_Mar = (sum(caz2)/nrow(rand)*100),
                                                                                          f_21_10_Apr = (sum(caz3)/nrow(rand)*100),
                                                                                          f_11_30_Apr = (sum(caz4)/nrow(rand)*100),
                                                                                          f_01_20_Mai = (sum(caz5)/nrow(rand)*100),
                                                                                          f_20_Mai = (sum(caz6)/nrow(rand)*100)
  
                                                                                                                )


frec.l <- plt.f %>% pivot_longer(-c(indicator), names_to = "frecventa")
frec.l$indicator <- gsub("X","",frec.l$indicator)

frec.l <- frec.l %>% mutate(frecventa = case_when(frecventa == "f_30_Mar" ~ "< 1 Mar",
                                                  frecventa == "f_01_20_Mar" ~ "01-20 Mar",
                                                  frecventa == "f_21_10_Apr" ~ "21 Mar-10 Apr",
                                                  frecventa == "f_11_30_Apr" ~ "11-30 Apr",
                                                  frecventa == "f_01_20_Mai" ~ "01-20 Mai",
                                                  frecventa == "f_20_Mai" ~ "> 20 Mai"))
u <- ggplot(data = frec.l,aes(x = factor(frecventa, levels = c("< 1 Mar","01-20 Mar","21 Mar-10 Apr","11-30 Apr","01-20 Mai","> 20 Mai")), y = value, fill = factor(indicator)))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_brewer( name = "", palette = "Spectral")+
  labs(x = " ", y = "Frecventa[%]")+ theme_minimal()+theme(axis.text.y = element_text(size = 18),
                                                           axis.text.x = element_text(size = 14),
                                                           axis.title =   element_text(size = 15, face = "bold"),
                                                           axis.text = element_text(size = 19))


png(paste0(drive_z,"png/frecventa_decadal_ultima_zi_strat_zapada_1961_2020.png"), width = 2000, height = 1600, res = 200)
u
dev.off()






