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
