library(raster)
library(ggplot2)
library(ggridges)
library(progress)

setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

USclimNames <- names(raster::brick("Data/USAnclimgrid_tavg.nc"))
USclim <- terra::rast("Data/USAnclimgrid_tavg.nc")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods.csv")
#dataIn <- dataIn[which(dataIn$Country%in%"US"),]
head(dataIn)


# Some points without month data
# Thus I'll use the mean of the year
# Then calculate the mean of each year and save it into a list

### Preparing vectors and variables

USyear <- list()

Year <- seq(1895,2024,by=1)
Mont <- c(paste0("0",1:9),10:12)

Montw <- c(paste0("0",4:9))
Montc <- c(paste0("0",c(1,2,3)),12)

### Loop for the mean
#The data is a layer for each moth of the year
#Then I extract the months of each year
# and calculate the mean annual temperature per pixel of each year

for(i in 1:length(Year)){
  
  # Due to 2024 has only 8 months, an special conditional
  if(Year[i]=="2024"){
    Ys <- paste0("X",Year[i],".",Mont[-c(9,10,11,12)],".","01")
    MeanYear <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm =T)
    #plot(MeanYear)
    USyear[[i]] <- MeanYear
  }
  # Normal conditional
  Ys <- paste0("X",Year[i],".",Mont,".","01")
  MeanYear <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm=T)
  #plot(MeanYear)
  USyear[[i]] <- MeanYear
}

names(USyear) <- Year
USyear

############
# Calculate only, per pixel,
# the mean annual temperature only for the warm months

USwarm <- list()

for (i in 1:length(Year)) {
  
  # Due to 2024 has only 8 months, an special conditional
  if(Year[i]=="2024"){
    Ys <- paste0("X",Year[i],".",Montw[-c(6)],".","01")
    MeanYearW <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm =T)
    #plot(MeanYear)
    USwarm[[i]] <- MeanYearW
  }
  
  Ys <- paste0("X",Year[i],".",Montw,".","01")
  MeanYearW <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm=T)
  USwarm[[i]] <- MeanYearW
  
}

names(USwarm) <- Year
USwarm

############
# Calculate only, per pixel,
# the mean annual temperature only for the warm months

UScold <- list()

for(i in 1:length(Year)){
    
    # Due to 2024 has only 8 months, an special conditional
    if((Year[i]=="2024")==T){
      Ys <- paste0("X",Year[i],".",Montc[-c(4)],".","01")
      MeanYearC <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm=T)
      #plot(MeanYear)
      UScold[[i]] <- MeanYearC
    }
    # Normal conditional
    Ys <- paste0("X",Year[i],".",Montc,".","01")
    MeanYearC <- mean(USclim[[which(USclimNames%in%Ys)]], na.rm=T)
    #plot(MeanYear)
    UScold[[i]] <- MeanYearC
    #print(mean(values(MeanYearC), na.rm=T))
    
  }
names(UScold) <- Year
UScold

#############
# Now, lets calculate the mean temperature of the whole year
# So, mean annual temperature of each year
# then mean annual temperature of the warm months each year
# finally, the mean annual temperature of the cold months each year

##########
Temp.AvgY <- list()
Temp.AvgW <- list()
Temp.AvgC <- list()

Temp.AvgY2 <- list()
Temp.AvgW2 <- list()
Temp.AvgC2 <- list()

Years <- c()


pb <- progress_bar$new(total=length(dataIn$Year))

for(i in 1:length(dataIn$Year)){
  
  if((dataIn$Year[i]<1895)==T){
    rastTmp <- USyear[[1]]
  }else{
    rastTmp <- USyear[[which(names(USyear)%in%as.character(dataIn$Year[i]))]]
  }
  
  Temp.AvgY[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  Temp.AvgY2[i] <- mean(as.vector(values(rastTmp)),na.rm=T)
  #print(i);print(Temp.AvgY[i])
  
  pb$tick()
}


##########

pb <- progress_bar$new(total=length(dataIn$Year))

for(i in 1:length(dataIn$Year)){
  
  if((dataIn$Year[i]<1895)==T){
    rastTmp <- USwarm[[1]]
  }else{
    rastTmp <- USwarm[[which(names(USwarm)%in%as.character(dataIn$Year[i]))]]
  }
  
  Temp.AvgW[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  Temp.AvgW2[i] <- mean(as.vector(values(rastTmp)),na.rm=T)
  #print(i);print(Temp.Avg[i])
  
  pb$tick()
}


##########

pb <- progress_bar$new(total=length(dataIn$Year))

for(i in 1:length(dataIn$Year)){
  
  if((dataIn$Year[i]<1895)==T){
    rastTmp <- UScold[[1]]
  }else{
    rastTmp <- UScold[[which(names(UScold)%in%as.character(dataIn$Year[i]))]]
  }
  
  Temp.AvgC[i] <- unlist(terra::extract(rastTmp, cbind(dataIn$Lon[i], dataIn$Lat[i])))
  Temp.AvgC2[i] <- mean(as.vector(values(rastTmp)),na.rm=T)
  #print(i);print(Temp.Avg[i])
  
  pb$tick()
}


#########

dataIn2 <- read.csv("Data/Occurences_Tb_TempPeriods.csv")
head(dataIn2)

TempAll <- data.frame(Temp.AvgY= unlist(Temp.AvgY), Temp.AvgW= unlist(Temp.AvgW), Temp.AvgC = unlist(Temp.AvgC),
                      Temp.AvgY2= unlist(Temp.AvgY2), Temp.AvgW2= unlist(Temp.AvgW2), Temp.AvgC2 = unlist(Temp.AvgC2))
TempAll

summary(TempAll)

#dataIn2 <- cbind(dataIn2[,1:11], Region= dataIn$State, TempAll)
dataIn2 <- cbind(dataIn2, TempAll)
head(dataIn2)

write.csv(dataIn2, file = "Data/Occurences_Tb_TempPeriods_FINAL.csv", row.names = F, quote = F)
