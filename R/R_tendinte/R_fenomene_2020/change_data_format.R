
library(xlsx)

Sys.setenv("LANG","ro_RO")
tz <- read.xlsx("tab_export/excels/prima_ultima_zi_fen_BRUMA_1961_2019_v112.xlsx", sheetIndex = 1,startRow = 3, header = T)
head(tz)

tzmnmx <- tz[,-c(grep("Medie",colnames(tz)))]
tzmed <- tz[,c(grep("Medie",colnames(tz)))]
### metoda 1 
tzmed.col <- NULL 
tzmnmx.col <- NULL

for (i in 1:ncol(tzmed)){
    
    print(names(tzmed[i])) 
    clmed <- as.Date(tzmed[,i],"%b%d")
    clmed <- format(clmed, "%j")
    clmed <- as.numeric(clmed)
    tzmed.col <- cbind(tzmed.col,clmed)
    
  }
  

colnames(tzmed.col) <- colnames(tzmed)
  for (j in 4:ncol(tzmnmx)){
    print(names(tzmnmx[j])) 
    clmnmx <- as.Date(tzmnmx[,j],"%Y%b%d")
    clmnmx <- format(clmnmx, "%j")
    clmnmx <- as.numeric(clmnmx)
    tzmnmx.col <- cbind(tzmnmx.col,clmnmx)
  }
  colnames(tzmnmx.col) <- colnames(tzmnmx[3:6])
  tz.rst <- as.data.frame(cbind(tz[1:2],tzmed.col,tzmnmx.col))
  
  
  

