library(raster)
library(ggplot2)
library(ggridges)
library(progress)
library(sf)
library(dplyr)

setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

albers_proj <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +datum=WGS84")

Series <- 1:length(unique(dataIn$cat2))
Series <- Series[-c(1,2)] # remove first two series, few points
Series # Centroid analysis will start since 1920

Years <- sort(unique(dataIn$Year))
Series <- 1:length(Years)
Series <- Series[-c(1:29)]
Years[Series]

#################################
#          FINAL CODE           #
#################################

#First, create the polygons for each period for each region
# Then, calculate the centroids and area of each polygon


## Lists for polygons

ESpol <- list()
MIpol <- list()
WEpol <- list()

## List for centroids

ESc <- list()
MIc <- list()
WEc <- list()

## List for areas

ESa <- list()
MIa <- list()
WEa <- list()

## Split the data by region

ES <- dataIn[which(dataIn$Region=="ES"),]
MI <- dataIn[which(dataIn$Region=="MI"),]
WE <- dataIn[which(dataIn$Region=="WE"),]

######################

cumulative <- c()

for(i in 1:length(Series)){
  if(i==1){
    #Extract only the specific period, this is an exception 1900-1920
    periodES <- ES[which(ES$cat2%in%c(1,2,Series[i])), c('Lon','Lat')]
    periodMI <- MI[which(MI$cat2%in%c(1,2,Series[i])), c('Lon','Lat')]
    periodWE <- WE[which(WE$cat2%in%c(1,2,Series[i])), c('Lon','Lat')]
    
    #periodES <- ES[which(ES$Year%in%Years[c(1:29,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    #periodMI <- MI[which(MI$Year%in%Years[c(1:29,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    #periodWE <- WE[which(WE$Year%in%Years[c(1:29,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    
    
    periodES <- st_as_sf(periodES, coords = c("Lon", "Lat"), crs = 4326)
    periodES <- st_transform(periodES, crs=albers_proj)
    
    ESpol[[i]] <- st_convex_hull(st_union(periodES)) # Polygon
    ESc[[i]] <- st_centroid(st_convex_hull(st_union(periodES))) # Centroid
    ESa[[i]] <- (st_area(st_convex_hull(st_union(periodES))) / 1e6) #area in Km
    
    periodMI <- st_as_sf(periodMI, coords = c("Lon", "Lat"), crs = 4326)
    periodMI <- st_transform(periodMI, crs=albers_proj)
    
    MIpol[[i]] <- st_convex_hull(st_union(periodMI)) # Polygon
    MIc[[i]] <- st_centroid(st_convex_hull(st_union(periodMI))) # Centroid
    MIa[[i]] <- (st_area(st_convex_hull(st_union(periodMI))) / 1e6) #area in Km
    
    periodWE <- st_as_sf(periodWE, coords = c("Lon", "Lat"), crs = 4326)
    periodWE <- st_transform(periodWE, crs=albers_proj)
    
    WEpol[[i]] <- st_convex_hull(st_union(periodWE)) # Polygon
    WEc[[i]] <- st_centroid(st_convex_hull(st_union(periodWE))) # Centroid
    WEa[[i]] <- (st_area(st_convex_hull(st_union(periodWE))) / 1e6) #area in Km
    
    cumulative <- c(cumulative,1,2,Series[i])
    #cumulative <- c(cumulative,1:29,Series[i]) ## FOR EACH YEAR
    
  }else{
    
    #Extract only the specific period, this is an exception 1900-1920
    periodES <- ES[which(ES$cat2%in%c(cumulative,Series[i])), c('Lon','Lat')]
    periodMI <- MI[which(MI$cat2%in%c(cumulative,Series[i])), c('Lon','Lat')]
    periodWE <- WE[which(WE$cat2%in%c(cumulative,Series[i])), c('Lon','Lat')]
    
    #periodES <- ES[which(ES$Year%in%Years[c(cumulative,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    #periodMI <- MI[which(MI$Year%in%Years[c(cumulative,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    #periodWE <- WE[which(WE$Year%in%Years[c(cumulative,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    
    periodES <- st_as_sf(periodES, coords = c("Lon", "Lat"), crs = 4326)
    periodES <- st_transform(periodES, crs=albers_proj)
    
    ESpol[[i]] <- st_convex_hull(st_union(periodES)) # Polygon
    ESc[[i]] <- st_centroid(st_convex_hull(st_union(periodES))) # Centroid
    ESa[[i]] <- (st_area(st_convex_hull(st_union(periodES))) / 1e6) #area in Km
    
    periodMI <- st_as_sf(periodMI, coords = c("Lon", "Lat"), crs = 4326)
    periodMI <- st_transform(periodMI, crs=albers_proj)
    
    MIpol[[i]] <- st_convex_hull(st_union(periodMI)) # Polygon
    MIc[[i]] <- st_centroid(st_convex_hull(st_union(periodMI))) # Centroid
    MIa[[i]] <- (st_area(st_convex_hull(st_union(periodMI))) / 1e6) #area in Km
    
    periodWE <- st_as_sf(periodWE, coords = c("Lon", "Lat"), crs = 4326)
    periodWE <- st_transform(periodWE, crs=albers_proj)
    
    WEpol[[i]] <- st_convex_hull(st_union(periodWE)) # Polygon
    WEc[[i]] <- st_centroid(st_convex_hull(st_union(periodWE))) # Centroid
    WEa[[i]] <- (st_area(st_convex_hull(st_union(periodWE))) / 1e6) #area in Km
    
    cumulative <- c(cumulative, Series[i])
    #cumulative <- c(cumulative, Series[i])
    
  }
}


#############################################

centroid_distES <- c()
centroid_distMI <- c()
centroid_distWE <- c()

for(i in 1:length(Series)){
  if(i==1){
    centroid_distES <- c(centroid_distES, 0)
    centroid_distMI <- c(centroid_distMI, 0)
    centroid_distWE <- c(centroid_distWE, 0)
  }else{
    distES <- st_distance(ESc[[i]],ESc[[i-1]])
    distES <- as.numeric(distES)/1000
    
    distMI <- st_distance(MIc[[i]],MIc[[i-1]])
    distMI <- as.numeric(distMI)/1000
    
    distWE <- st_distance(WEc[[i]],WEc[[i-1]])
    distWE <- as.numeric(distWE)/1000
    
    avgES <- (ESa[[i]] + ESa[[i-1]]) / 2
    avgMI <- (MIa[[i]] + MIa[[i-1]]) / 2
    avgWE <- (WEa[[i]] + WEa[[i-1]]) / 2

    centroid_distES <- c(centroid_distES,(distES / sqrt(avgES)))
    centroid_distMI <- c(centroid_distMI,(distMI / sqrt(avgMI)))
    centroid_distWE <- c(centroid_distWE,(distWE / sqrt(avgWE)))
    
  }
}

centroid_distES
centroid_distMI
centroid_distWE


ggplot()+
  geom_point(aes(x=1:length(Series), centroid_distWE))

centroid_frame <- data.frame(ES  = centroid_distES,
                             MI = centroid_distMI,
                             WE = centroid_distWE)

centroid_frame

write.csv(centroid_frame, file = "Data/Centroids_Data.csv", quote = F, row.names = F)

#################

c_coords <- matrix(NA, nrow = 1, ncol = 2)
colnames(c_coords) <- c("X","Y")

for(i in 1:length(ESc)){
  
  EScc <- st_transform(ESc[[i]], crs = 4326)
  MIcc <- st_transform(MIc[[i]], crs = 4326)
  WEcc <- st_transform(WEc[[i]], crs = 4326)
  
  c_coords <- rbind(c_coords, 
                    st_coordinates(EScc),
                    st_coordinates(MIcc),
                    st_coordinates(WEcc))
  
}

c_coords <- c_coords[-1,]
c_coords <- as.data.frame(cbind(Year=rep(1:length(Series), each=3), c_coords, Region=rep(c("ES","MI","WE"), length(Series))))
c_coords

write.csv(c_coords, file = "Data/Centroids_Data_Coords.csv", quote = F, row.names = F)
#write.csv(c_coords, file = "Data/Centroids_Data_CoordsYEARS.csv", quote = F, row.names = F)


###############################################################################

          ##########################################################
          #                Again for U.S. territory                #
          ##########################################################

###############################################################################


dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

albers_proj <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +datum=WGS84")

Series <- 1:length(unique(dataIn$cat2))
Series <- Series[-c(1,2)] # remove first two series, few points
Series # Centroid analysis will start since 1920

Years <- sort(unique(dataIn$Year))
Series <- 1:length(Years)
Series <- Series[-c(1:29)]
Years[Series]

#################################
#          FINAL CODE           #
#################################

#First, create the polygons for each period for each region
# Then, calculate the centroids and area of each polygon


## Lists for polygons

USpol <- list()

## List for centroids

USc <- list()

## List for areas

USa <- list()

######################

US <- dataIn

cumulative <- c()

for(i in 1:length(Series)){
  if(i==1){
    #Extract only the specific period, this is an exception 1900-1920
    period <- US[which(US$cat2%in%c(1,2,Series[i])), c('Lon','Lat')] ## FOR DECADES
    #period <- US[which(US$Year%in%Years[c(1:29,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR

    
    period <- st_as_sf(period, coords = c("Lon", "Lat"), crs = 4326)
    period <- st_transform(period, crs=albers_proj)
    
    USpol[[i]] <- st_convex_hull(st_union(period)) # Polygon
    USc[[i]] <- st_centroid(st_convex_hull(st_union(period))) # Centroid
    USa[[i]] <- (st_area(st_convex_hull(st_union(period))) / 1e6) #area in Km
    
    cumulative <- c(cumulative,1:2,Series[i]) ## FOR DECADES
    #cumulative <- c(cumulative,1:29,Series[i]) ## FOR EACH YEAR
    
    
  }else{
    
    #Extract only the specific period, this is an exception 1900-1920
    period <- US[which(US$cat2%in%c(cumulative,Series[i])), c('Lon','Lat')] ## FOR DECADES
    #period <- US[which(US$Year%in%Years[c(cumulative,Series[i])]),c('Lon','Lat')] ## FOR EACH YEAR
    
    period <- st_as_sf(period, coords = c("Lon", "Lat"), crs = 4326)
    period <- st_transform(period, crs=albers_proj)
    
    USpol[[i]] <- st_convex_hull(st_union(period)) # Polygon
    USc[[i]] <- st_centroid(st_convex_hull(st_union(period))) # Centroid
    USa[[i]] <- (st_area(st_convex_hull(st_union(period))) / 1e6) #area in Km
    
    cumulative <- c(cumulative, Series[i])
    
  }
}

#############################################

centroid_distUS <- c()

for(i in 1:length(Series)){
  if(i==1){
    
    centroid_distUS <- c(centroid_distUS, 0)

  }else{
    
    distUS <- st_distance(USc[[i]],USc[[i-1]])
    distUS <- as.numeric(distUS)/1000
    
    avgUS <- (USa[[i]] + USa[[i-1]]) / 2
    
    centroid_distUS <- c(centroid_distUS,(distUS / sqrt(avgUS)))
    
  }
}

centroid_distUS

ggplot()+
  geom_point(aes(x=1:length(Series), centroid_distUS))


centroid_frame <- data.frame(US  = centroid_distUS)

centroid_frame

write.csv(centroid_frame, file = "Data/Centroids_Data_US.csv", quote = F, row.names = F)
#write.csv(centroid_frame, file = "Data/Centroids_Data_USYEARS.csv", quote = F, row.names = F)

################

c_coords <- matrix(NA, nrow = 1, ncol = 2)
colnames(c_coords) <- c("X","Y")

for(i in 1:length(USc)){
  
  UScc <- st_transform(USc[[i]], crs = 4326)
  
  c_coords <- rbind(c_coords, 
                    st_coordinates(UScc))
  
}

c_coords <- c_coords[-1,]
c_coords <- as.data.frame(cbind(Year=1:length(Series), c_coords))
c_coords

write.csv(c_coords, file = "Data/Centroids_Data_CoordsUS.csv", quote = F, row.names = F)
#write.csv(c_coords, file = "Data/Centroids_Data_CoordsUSYEARS.csv", quote = F, row.names = F)
