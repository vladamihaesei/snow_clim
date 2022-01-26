library(dplyr)
library(trend)
library(climatetools)
library(data.table)

ws.prov <- read.csv(paste0(drive_z,"tab/ws_statii_koeppen.csv"))
tabs <- list.files(path = paste0(drive_z,"tab_export"),pattern = "_eaach_year_1961-2020.csv", full.names = T)
tabs <- grep("BRUMA|zile_",tabs, invert = T, value = T )


for (n in 1:length(tabs)){
  
  print(tabs[n])
  nume <- strsplit(tabs[n],"/|_|.csv")[[1]][16]
  print(nume)
  t <- read.csv(tabs[n])
  t <- as.data.frame(t)
  t <- na.omit(t)
  head(t)
  
  #### BRUMA &GROSZ & NINSOARE
  t_trend <- t %>%
    group_by(cod,nume) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.prima = sens.slope(prima_zi_jul_decalat)$estimates *10,
              sign.prima = mk.test(prima_zi_jul_decalat)$p.value,
              conf.prima = sens.slope(prima_zi_jul_decalat, conf.level = 0.95)$conf.int,
              slope.ultima = sens.slope(ultima_zi_jul_decalat)$estimates *10,
              sign.ultima = mk.test(ultima_zi_jul_decalat)$p.value,
              conf.ultima = sens.slope(ultima_zi_jul_decalat, conf.level = 0.95)$conf.int,
              slope.interval = sens.slope(interval_zile)$estimates *10,
              sign.interval = mk.test(interval_zile)$p.value,
              conf.interval = sens.slope(interval_zile, conf.level = 0.95)$conf.int)

  names(t_trend)[1] <- "CODGE"
  t_trend$mnmx <- rep(c("mn","mx"), 114)
  
  t.w <- t_trend %>% pivot_wider(c(CODGE,nume,slope.prima,sign.prima,slope.ultima,sign.ultima,slope.interval,sign.interval), values_from = c(conf.prima,conf.ultima,conf.interval), names_from = "mnmx" )
  
  t_tr <- t.w %>% left_join(ws.prov[c(5,8,9,10,13,14,15,16)])
  t_tr[3:14] <-  round(t_tr[3:14],3)
  
  write.csv(t_tr, paste0(drive_z,"tab_export/trend_",nume,"_prima_ultima_interval_1961-2020.csv"), row.names = F)
  
  #t_tr <- st_as_sf(t_tr, coords = c('Lon', 'Lat'), crs = 4326)
  
  ############### grouping for criter 1 and criter 2 koeppen 
  names(t)[2] <- "CODGE"
  
  t.join <- t%>% left_join(ws.prov[c(5,8,9,10,13,14,15,16)])
  
  t_trend_kp <- t.join %>%
    group_by(ex.category, Criter2) %>% # we group by name and cod to perform the calculation in each station
    summarise(slope.prima = sens.slope(prima_zi_jul_decalat)$estimates *10,
              sign.prima = mk.test(prima_zi_jul_decalat)$p.value,
              conf.prima = sens.slope(prima_zi_jul_decalat, conf.level = 0.95)$conf.int,
              slope.ultima = sens.slope(ultima_zi_jul_decalat)$estimates *10,
              sign.ultima = mk.test(ultima_zi_jul_decalat)$p.value,
              conf.ultima = sens.slope(ultima_zi_jul_decalat, conf.level = 0.95)$conf.int,
              slope.interval = sens.slope(interval_zile)$estimates *10,
              sign.interval = mk.test(interval_zile)$p.value,
              conf.interval = sens.slope(interval_zile, conf.level = 0.95)$conf.int)
 
  t_trend_kp$mnmx <- rep(c("mn","mx"), 11)
  
  t.kp.w <- t_trend_kp %>% pivot_wider(c(ex.category,Criter2,slope.prima,sign.prima,slope.ultima,sign.ultima,slope.interval,sign.interval), values_from = c(conf.prima,conf.ultima,conf.interval), names_from = "mnmx" )
  
  t.kp.w[3:14] <- round(t.kp.w[3:14],3)
  write.csv(t.kp.w, paste0(drive_z,"tab_export/trend_",nume,"_prima_ultima_1961-2020_grouping_koeppen.csv"), row.names = F)
  
  

}

