library(ggplot2)
library(dplyr)
library(tidyr)
library(climatetools)
#####
library(ps)
library(pa)
library(nFactors)
library(psych)

#install.packages("FactoMineR")

library(FactoMineR)
library(factoextra)
source("R/cale_legenda_vectors.R")
## verificare numar statii 
statii <- read.csv(paste0(drive_z,"tab/statii.csv"))
statii <- statii %>% filter(COD == 428632)

statii_cr <- read.csv(paste0(drive_z,"tab/tavg_ws_inventory_selection.csv"))
statii.j <- ws[c(2,3,4,5,6,7,8,9)]%>% left_join(statii_cr)
###########################################################
###########################################################

tmin <- read.csv(paste0(drive_z,"tab/TMIN_1961_2019.csv")) #temp minima 
tmin <- tmin[1:3]
nebt <- read.csv(paste0(drive_z,"tab/NEBT_ora_6_1961_2019.csv"))
t <- read.csv(paste0(drive_z,"tab/GROSZ_1961_2019.csv"))
names(ws)[4] <- "COD"
t <- t %>%filter(COD!=509940) ### elimina sulina pentru strat de zapada
t1 <- t %>% left_join(nebt, by = c("COD", "DAT")) %>% left_join(tmin, by = c("COD", "DAT"))%>% left_join(ws[c(2,3,4,5,7,8,9)])
### validare cu nebulozitate
t1$GROSZ[is.na(t1$NEBT)] <- NA # 
t1$GROSZ[t1$NEBT >= 0 & is.na(t1$GROSZ)] <- 0 # asta e bine
t1$DAT <- as.Date(t1$DAT)
t1.n <- t1 %>% filter(format(DAT, "%m") %in% c("11","12","01","02","03")) %>% na.omit()
t1.n$DAT <- as.Date(t1.n$DAT)

####lunare multianuale 
t1.month <- t1.n %>% group_by(COD,CMR,JU,NUME,Lat,Lon,Z, format(DAT,"%m")) %>% summarise(mean_with_zero = mean(GROSZ),
                                                                                         mean_no_zero = mean(GROSZ[GROSZ>0]))

names(t1.month)[8] <- "DAT"

<<<<<<< HEAD
t1.month  <- t1.month[c(1,8,9)] %>% pivot_wider(c(COD), values_from = c(mean_with_zero), names_from = "DAT")
t1.month.sc <- t1.month[2:ncol(t1.month)]
=======
t1.month.l  <- t1.month[c(1,4,5,6,7,8,9)] %>% pivot_wider(c(COD,NUME,Z,Lat,Lon), values_from = c(mean_with_zero), names_from = "DAT" )
t1.month.sc <- t1.month.l[6:ncol(t1.month.l)]
>>>>>>> 2a0036c85cde6266ed9ceb688e3fc84758c8411d
t1.month.sc[is.na(t1.month.sc)] <- 0

pc <- prcomp(t1.month.sc, scale. = T)

plot(pc)
plot(pc, type = "l")
# First for principal components
comp <- data.frame(pc$x[,1:4])

<<<<<<< HEAD
fviz_nbclust(comp, kmeans, method = "silhouette", print.summary = T)+ labs(subtitle = "Silhouette method")
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
##### lunare 
t1.month.an <- t1.n %>% group_by(COD,CMR,JU,NUME,Lat,Lon,Z, format(DAT,"%Y%m")) %>% summarise(mean_with_zero = mean(GROSZ),
                                                                                           mean_no_zero = mean(GROSZ[GROSZ>0]))
names(t1.month.an)[8] <- "DAT"


t1.month.an  <- t1.month.an[c(1,8,9)] %>% pivot_wider(c(COD), values_from = c(mean_with_zero), names_from = "DAT")
t1.month.an.sc <- t1.month.an[2:ncol(t1.month.an)]
t1.month.an.sc[is.na(t1.month.an.sc)] <- 0
=======
k <- kmeans(comp, 6, nstart=25, iter.max=1000)
k$cluster

t1.cluster <- cbind(t1.month.l[1:5],cluster = k$cluster)

coordinates(t1.cluster) <- ~Lon+Lat

cl <- st_as_sf(t1.cluster)


ggplot()+
  geom_sf(data = cl, aes(color = as.character(cluster)))
>>>>>>> 2a0036c85cde6266ed9ceb688e3fc84758c8411d

pc <- prcomp(t1.month.an.sc, scale. = T)
plot(pc)
plot(pc, type = "l")
# First for principal components
comp <- data.frame(pc$x[,1:4])
### cluster
fviz_nbclust(comp, kmeans, method = "silhouette", print.summary = T)+ labs(subtitle = "Silhouette method")
k <- kmeans(comp, 4, nstart=25, iter.max=1000)

######

##### zilnice

t1.n <- t1.n %>% filter(GROSZ != 0)
t1.n.l <- t1.n[c(1,2,3)]%>% pivot_wider(c(COD), values_from = c(GROSZ), names_from = "DAT" )
t1.n.l[is.na(t1.n.l)] <- 0
# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(t1.n.l, scale = T)

plot(pc)
plot(pc, type = "l")
# First for principal components
comp <- data.frame(pc$x[,1:3])

fviz_nbclust(comp, kmeans, method = "silhouette", print.summary = T)+ labs(subtitle = "Silhouette method")

k <- kmeans(comp, 4, nstart=25, iter.max=1000)



# # Determine Number of Factors to Extract
ev <- eigen(cor(t1.month.an.sc)) # get eigenvalues
ap <- parallel(subject=nrow(t1.month.an.sc),var=ncol(t1.month.an.sc), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## method 1 the factor analysis
factors <- factanal(x = t1.n.l, factors = 17,lower = 0.6,rotation = "varimax")
load1<- factors$loadings
load.sub <- print(factors$loadings,cutoff = 0.6)
fs_scores <- factor.scores(t1.n.l,load.sub, method = "Thurstone")
f <- fs_scores$scores

## method 2 factor analysis
fa <- fa(tab.t,nfactors = 8,rotate = "varimax",fm = "wls",lower = 0.6, scores = "regression") # give 9 clusters
fac <- fa(df.sub, nfactors= 8,rotate="varimax", fm="pa", lower = 0.6, scores = "tenBerge") ### the method give 8 clusters

#### factor scores
f2 <- fa$scores
f3 <- fac$scores

# Silhouette method 1metoda de a gasi nr optimal de cluster
png("png/Sillouehte.png",width = 1600, height = 1400, res = 200)
fviz_nbclust(f, kmeans, method = "silhouette", print.summary = T)+ labs(subtitle = "Silhouette method")
dev.off()
#gap-static method 2
#gap_stat <- clusGap(tab.t, FUN = kmeans, nstart = 25,
#           K.max = 8, B = 50)
# Print the result
#print(gap_stat, method = "firstmax")
#fviz_gap_stat(gap_stat)

##clustering
tab.t <- scale(tab.t)
k2 <- kmeans(tab.t, centers = 8, nstart = 25)
k2

### export image for cluster
png(filename = "png/foraje_boicu_cluster.png", width = 1900, height = 1400, res = 170)
fviz_cluster(k2, data = tab.t,stand = T,geom= "point",palette = "Set2", ggtheme = theme_minimal())
dev.off()

### pentruexport tabel
df <- as.data.frame(k2$cluster) ## create data frame of the clusters
dff <- cbind(tab.t,df[1])## merge the two tabels
names(dff)[457] <- "cluster"### rename the colname


dff <- cbind(dff,tab[1]) ## join the cluster tabel at the original tabel
dff$nume <- rownames(tab.t)##  create a column with the names
dff.piv <- dff %>% pivot_longer(-c(cluster,nume), names_to = "indicator")##  form wide to long
dff.piv %>% group_by(cluster,indicator) %>%
  summarise_all("mean")  #### grouping and summarise by cluster and year
#export csv
write.csv(dff.piv, file = "tab/boicu_cluster_foraje_noname.csv", row.names = F)





d = mtcars
d2 = prcomp(d, scale=T)
x =d2$x[,1:2]
y = kmeans(x,2)
y$cluster
plot(x,col=y$cluster,cex=0.1)
text(x,row.names(mtcars),col=y$cluster)



