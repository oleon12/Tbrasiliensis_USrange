library(raster)
library(ggplot2)
library(ggridges)
library(progress)
library(rgdal)
library()

setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_Periods.csv")
dataIn <- dataIn[which(dataIn$Country%in%"US"),]
head(dataIn)

dataIn2 <- dataIn[,c("Lon","Lat")]

coordinates(dataIn2) <- ~Lon+Lat

Division2 <- readOGR("GBIF/USA_Divisions_2.shp")

# US Avg. Temp 1985-01-01 / 2024-08-01

USclimNames <- names(raster::brick("Data/USAnclimgrid_tavg.nc"))
USclim <- terra::rast("Data/USAnclimgrid_tavg.nc")

# Some points without month data
# Thus I'll use the mean of the year
# Then calculate the mean of each year and save it into a list

### Preparing vectors and variables

USyear <- list()

Year <- seq(1895,2024,by=1)
Mont <- c(paste0("0",1:9),10:12)

Montw <- c(paste0("0",4:8))

### Loop for the mean

for(i in 1:length(Year)){
  
  # Due to 2024 has only 8 months, an special conditional
  if(Year[i]=="2024"){
    Ys <- paste0("X",Year[i],".",Mont[-c(9,10,11,12)],".","01")
    MeanYear <- mean(USclim[[which(USclimNames%in%Ys)]])
    #plot(MeanYear)
    USyear[[i]] <- MeanYear
  }
  # Normal conditional
  Ys <- paste0("X",Year[i],".",Mont,".","01")
  MeanYear <- mean(USclim[[which(USclimNames%in%Ys)]])
  #plot(MeanYear)
  USyear[[i]] <- MeanYear
}

names(USyear) <- Year

############

USwarm <- list()

for (i in 1:length(Year)) {
  
  Ys <- paste0("X",Year[i],".",Montw,".","01")
  MeanYearW <- mean(USclim[[which(USclimNames%in%Ys)]])
  USwarm[[i]] <- MeanYear

}

names(USwarm) <- Year


### No

Temp.Avg <- rep(NA, length(dataIn$Year))
Temp.AvgY <- rep(NA, length(dataIn$Year))
Temp.AvgW <- rep(NA, length(dataIn$Year))

pb <- progress_bar$new(total=length(dataIn$Year))

i <- 23

for(i in 1:length(dataIn$Year)){
  
  if(is.na(dataIn$Month[i])==TRUE){
    
    if(dataIn$Year[i]<1895){
      rastTmp <- USyear[[1]]
    }else{
      rastTmp <- USyear[[which(names(USyear)%in%dataIn$Year[i])]]
    }
  
  }else{
    
    if(dataIn$Month[i]<10){
      Data <- paste0("X", dataIn$Year[i],".","0",dataIn$Month[i],".","01")
    }else{
      Data <- paste0("X", dataIn$Year[i],".",dataIn$Month[i],".","01")
    }
    
    if(Data%in%c("X2024.09.01","X2024.10.01","X2024.11.01","X2024.12.01")){
      Data <- "X2024.08.01"
    }
    
    if(Data%in%USclimNames){
      
      rastTmp <- USclim[[which(USclimNames%in%Data)]]

    }
  }
  
  Temp.Avg[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  #print(i);print(Temp.Avg[i])

  pb$tick()
  
}

##########

pb <- progress_bar$new(total=length(dataIn$Year))

for(i in 1:length(dataIn$Year)){
  
  if(dataIn$Year[i]<1895){
    rastTmp <- USyear[[1]]
  }else{
    rastTmp <- USyear[[which(names(USyear)%in%as.character(dataIn$Year[i]))]]
  }
  
  Temp.AvgY[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  #print(i);print(Temp.Avg[i])
  
  pb$tick()
}


##########

pb <- progress_bar$new(total=length(dataIn$Year))

for(i in 1:length(dataIn$Year)){
  
  if(dataIn$Year[i]<1895){
    rastTmp <- USwarm[[1]]
  }else{
    rastTmp <- USwarm[[which(names(USwarm)%in%as.character(dataIn$Year[i]))]]
  }
  
  Temp.AvgW[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  #print(i);print(Temp.Avg[i])
  
  pb$tick()
}

Temp.Avg
Temp.AvgY
Temp.AvgW

dataIn <- cbind(dataIn, Temp.Avg, Temp.AvgY, Temp.AvgW)
head(dataIn)

########

USdivisions <- readOGR("GBIF/USA_Divisions_2.shp")
plot(USdivisions)

proj4string(dataIn2) <- CRS(proj4string(USdivisions))

Division2 <- over(dataIn2, USdivisions)["Name2"]

dataIn <- cbind(dataIn, as.vector(Division2))
head(dataIn)

write.csv(dataIn, "Data/Occurences_Tb_TempPeriods.csv", quote = F, row.names = F)
