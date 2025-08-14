library(raster)
library(ggplot2)
library(ggridges)
library(plotly)
library(grid)
library(dplyr)

setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

####################

ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat, y=dataIn$cat, group=dataIn$cat),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1.5, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Lat),max(dataIn$Lat)))+
  
  geom_vline(xintercept = max(dataIn$Lat), linetype="dotted", alpha=0.5, color="darkred")+
  geom_text(aes(x=max(dataIn$Lat), y=14.8), label="WY-US",angle=90, size=3)+
  
  geom_vline(xintercept = 44.05319, linetype="dotted", alpha=0.5, color="darkred")+
  geom_text(aes(x=44.05319, y=14.8), label="OR-US",  angle=90, size=3)+
  
  geom_vline(xintercept = 43.25784, linetype="dotted", alpha=0.5, color="darkred")+
  geom_text(aes(x=43.25784, y=14.8), label="OR-US",  angle=90, size=3)+
  
  geom_vline(xintercept = summary(dataIn$Lat)[3], color="black",linetype="dashed", linewidth=0.5, alpha=0.5)+
  
  xlab("Latitude")+ylab("Time periods")+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=12))

ggsave("ImgFinal/LatitudeVsPeriods_V3_USA.pdf", dpi = 300, device = "pdf")
ggsave("ImgFinal/LatitudeVsPeriods_V3_USA.png", dpi = 300, device = "png")

#############################

summary(dataIn$Lat)
quantile(dataIn$Lat)

ggplot()+
  geom_density(aes(x=dataIn$Lat), fill="grey")+
  #geom_point(aes(x=dataIn$Lat, y=0, color=dataIn$Temp.AvgY), shape="|", alpha=0.7)+
  #scale_color_continuous(type = "viridis")+
  #geom_point(aes(x=dataIn$Lat, y=0.02, color=dataIn$Temp.AvgW), shape="|", alpha=0.7)+
  #scale_color_continuous(type = "viridis")+
  xlab("Latitude")+ylab("Density")+
  geom_vline(xintercept = summary(dataIn$Lat)[c(2,5)], linetype="dotted", linewidth=0.5, alpha=0.5)+
  geom_vline(xintercept = summary(dataIn$Lat)[4], color="black",linetype="dashed", linewidth=0.5, alpha=0.5)+
  geom_text(aes(x=summary(dataIn$Lat)[4], y=.01), label="Mean",angle=90, size=5)+
  geom_text(aes(x=summary(dataIn$Lat)[c(2,5)], y=.01), label=c("25%","75%"),angle=90, size=5)+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=12))

ggsave("ImgFinal/Latitude_Distribution.pdf", dpi = 300, device = "pdf")
ggsave("ImgFinal/Latitude_Distribution.png", dpi = 300, device = "png")

#############################  

ggplot()+
  geom_density_ridges(aes(x=dataIn$Temp.AvgY, y=dataIn$cat, group=dataIn$cat),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Temp.AvgY),max(dataIn$Temp.AvgY)))+
  xlab("Mean Temperature (Annual)")+ylab("Time periods")

ggplot()+
  geom_density_ridges(aes(x=dataIn$Temp.AvgW, y=dataIn$cat, group=dataIn$cat),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Temp.AvgW),max(dataIn$Temp.AvgW)))+
  xlab("Mean Temperature (Warm)")+ylab("Time periods")

#############################  

ggplot()+
  geom_point(aes(x=dataIn$Lat,y=dataIn$Year, color=dataIn$Temp.AvgW))

ggplot()+
  geom_point(aes(y=dataIn$Lat, x=dataIn$Temp.AvgW, color=dataIn$cat))

ggplot()+
  geom_point(aes(x=dataIn$Year, y=dataIn$Temp.AvgW))

#############################

ggplot()+
  geom_density_ridges(aes(x=dataIn$Temp.AvgY, y=dataIn$Name2, group=dataIn$Name2))+
  ylim(limits=c("South","North"))+
  xlab("Mean Temperature (Annual)")+ylab(NULL)

SouthT <- ggplot()+
  geom_density_ridges(aes(x=dataIn$Temp.AvgW[which(dataIn$Name2=="South")],
                          y=dataIn$cat[which(dataIn$Name2=="South")],
                          group=dataIn$cat[which(dataIn$Name2=="South")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  xlab("Mean Temperature (Warm)")+ylab("Time Periods")+
  scale_x_continuous(limits = c(min(dataIn$Temp.AvgW[which(dataIn$Name2=="South")], na.rm = T),
                                max(dataIn$Temp.AvgW[which(dataIn$Name2=="South")], na.rm = T)))+
  ggtitle("B) Southern Distribution")
SouthT

SouthL <- ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat[which(dataIn$Name2=="South")],
                          y=dataIn$cat[which(dataIn$Name2=="South")],
                          group=dataIn$cat[which(dataIn$Name2=="South")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  xlab("Latitude")+ylab("Time Periods")+
  scale_x_continuous(limits = c(min(dataIn$Lat[which(dataIn$Name2=="South")], na.rm = T),
                                max(dataIn$Lat[which(dataIn$Name2=="South")], na.rm = T)))
SouthL

gridExtra::grid.arrange(SouthT,SouthL,ncol=2,nrow=1)


#############################

p <- plot_ly(x = dataIn$cat[which(dataIn$Name2=="South")], 
             y = dataIn$Temp.AvgW[which(dataIn$Name2=="South")], 
             z = dataIn$Lat[which(dataIn$Name2=="South")], type = "scatter3d", mode = "markers",
             marker = list(size = 5, color = dataIn$Temp.AvgW[which(dataIn$Name2=="South")], 
                           colorscale = "Viridis", symbol = "circle")) %>%
  layout(title = "Southern Distribution",
         scene = list(xaxis = list(title = "Year Period"),
                      yaxis = list(title = "Mean Temperature (Warm)"),
                      zaxis = list(title = "Latitude",
                                   backgroundcolor = "rgb(230, 230,230)",
                                   gridcolor = "rgb(255, 255, 255)",
                                   showbackground = TRUE)))
p

#############################

NorthT <- ggplot()+
  geom_density_ridges(aes(x=dataIn$Temp.AvgW[which(dataIn$Name2=="North")],
                          y=dataIn$cat[which(dataIn$Name2=="North")],
                          group=dataIn$cat[which(dataIn$Name2=="North")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  xlab("Mean Temperature (Warm)")+ylab("Time Periods")+
  scale_x_continuous(limits = c(min(dataIn$Temp.AvgW[which(dataIn$Name2=="North")], na.rm = T),
                                max(dataIn$Temp.AvgW[which(dataIn$Name2=="North")], na.rm = T)))+
  ggtitle("A) Northern Distribution")

NorthT

NorthL <- ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat[which(dataIn$Name2=="North")],
                          y=dataIn$cat[which(dataIn$Name2=="North")],
                          group=dataIn$cat[which(dataIn$Name2=="North")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  xlab("Latitude")+ylab("Time Periods")+
  scale_x_continuous(limits = c(min(dataIn$Lat[which(dataIn$Name2=="North")], na.rm = T),
                                max(dataIn$Lat[which(dataIn$Name2=="North")], na.rm = T)))
  
NorthL

gridExtra::grid.arrange(NorthT,NorthL,ncol=2,nrow=1)

#############################

p <- plot_ly(x = dataIn$cat[which(dataIn$Name2=="North")], 
             y = dataIn$Temp.AvgW[which(dataIn$Name2=="North")], 
             z = dataIn$Lat[which(dataIn$Name2=="North")], type = "scatter3d", mode = "markers",
             marker = list(size = 5, color = dataIn$Temp.AvgW[which(dataIn$Name2=="North")], 
                           colorscale = "Viridis", symbol = "circle")) %>%
  layout(title = "Northern Distribution",
         scene = list(xaxis = list(title = "Year Period"),
                      yaxis = list(title = "Mean Temperature (Warm)"),
                      zaxis = list(title = "Latitude",
                                   backgroundcolor = "rgb(230, 230,230)",
                                   gridcolor = "rgb(255, 255, 255)",
                                   showbackground = TRUE)))
p


###################

####################

ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat[which(dataIn$State=="WE")], 
                          y=dataIn$cat[which(dataIn$State=="WE")], 
                          group=dataIn$cat[which(dataIn$State=="WE")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Lat[which(dataIn$State=="WE")]),
                                max(dataIn$Lat[which(dataIn$State=="WE")])))+
  geom_vline(xintercept = max(dataIn$Lat[which(dataIn$State=="WE")]), linetype="dotted", alpha=0.5)+
  
  xlab("Latitude")+ylab("Time periods")

ggsave("ImgFinal/LatitudeVsPeriods_WE_USA.pdf", dpi = 300, device = "pdf")

#############################

ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat[which(dataIn$State=="MI")], 
                          y=dataIn$cat[which(dataIn$State=="MI")], 
                          group=dataIn$cat[which(dataIn$State=="MI")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Lat[which(dataIn$State=="MI")]),
                                max(dataIn$Lat[which(dataIn$State=="MI")])))+
  geom_vline(xintercept = max(dataIn$Lat[which(dataIn$State=="MI")]), linetype="dotted", alpha=0.5)+
  
  xlab("Latitude")+ylab("Time periods")

ggsave("ImgFinal/LatitudeVsPeriods_MI_USA.pdf", dpi = 300, device = "pdf")

#############################

ggplot()+
  geom_density_ridges(aes(x=dataIn$Lat[which(dataIn$State=="ES")], 
                          y=dataIn$cat[which(dataIn$State=="ES")], 
                          group=dataIn$cat[which(dataIn$State=="ES")]),
                      jittered_points = TRUE, point_shape="|",
                      position = position_points_jitter(width = 0.01, height = 0),
                      point_size = 1, point_alpha = 1, alpha = 0.7)+
  scale_x_continuous(limits = c(min(dataIn$Lat[which(dataIn$State=="ES")]),
                                max(dataIn$Lat[which(dataIn$State=="ES")])))+
  geom_vline(xintercept = max(dataIn$Lat[which(dataIn$State=="ES")]), linetype="dotted", alpha=0.5)+
  
  xlab("Latitude")+ylab("Time periods")

ggsave("ImgFinal/LatitudeVsPeriods_ES_USA.pdf", dpi = 300, device = "pdf")
