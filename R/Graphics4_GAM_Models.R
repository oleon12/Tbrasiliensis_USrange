library(ggplot2)
library(mgcv)
library(ggnewscale)


setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

Years <- unique(dataIn$Year)
Years <- sort(Years)

Yannual <- c()
Ycold <- c()
Ywarm <- c()


for(i in 1:length(Years)){
  
  dataTMP <- dataIn[which(dataIn$Year%in%Years[i]),]
  
  Yannual <- c(Yannual, mean(dataTMP$Temp.AvgY2, na.rm=T))
  Ycold <- c(Ycold, mean(dataTMP$Temp.AvgC2, na.rm=T))
  Ywarm <- c(Ywarm, mean(dataTMP$Temp.AvgW2, na.rm=T))
  
}

ggplot()+
  geom_smooth(aes(x=Years, y=Ycold), method = "gam",alpha = 0.1)+
  geom_point(aes(x=Years, y=Ycold, color=Ycold), shape=16)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Cold")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=Yannual), method = "gam",alpha = 0.1, color="black")+
  geom_point(aes(x=Years, y=Yannual, color=Yannual), shape = 15)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=Ywarm), method = "gam",alpha = 0.1, color="darkred")+
  geom_point(aes(x=Years, y=Ywarm, color=Ywarm), shape = 17)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm")+
  scale_x_continuous(limits = c(min(Years), max(Years)), breaks = seq(1860,2030,10))+
 
   xlab("Years")+ylab("Temperature °C")+ ggtitle("A") +
  
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=7),
        legend.position = "bottom",
        plot.title = element_text(size = 20))

ggsave("ImgFinal/Temperature_Years.png", dpi = 300, device = "png")


############

AIC(gam(Yannual~s(Years)),
    gam(Yannual~te(Years)) )

AIC(gam(Ycold~s(Years)),
    gam(Ycold~te(Years)) )

AIC(gam(Ywarm~s(Years)),
    gam(Ywarm~te(Years)) )

summary(gam(Yannual~s(Years)))
summary(gam(Ycold~s(Years)))
summary(gam(Ywarm~s(Years)))

#############

YannualWE <- c()
YcoldWE <- c()
YwarmWE <- c()

YannualES <- c()
YcoldES <- c()
YwarmES <- c()

YannualMI <- c()
YcoldMI <- c()
YwarmMI <- c()


for(i in 1:length(Years)){
  
  dataTMP <- dataIn[which(dataIn$Year%in%Years[i]),]
  
  dataTMP2 <- dataTMP[which(dataTMP$Region%in%"WE"),]
  
  YannualWE <- c(YannualWE, mean(dataTMP2$Temp.AvgY2, na.rm=T))
  YcoldWE <- c(YcoldWE, mean(dataTMP2$Temp.AvgC2, na.rm=T))
  YwarmWE <- c(YwarmWE, mean(dataTMP2$Temp.AvgW2, na.rm=T))
  
  dataTMP2 <- dataTMP[which(dataTMP$Region%in%"ES"),]
  
  YannualES <- c(YannualES, mean(dataTMP2$Temp.AvgY2, na.rm=T))
  YcoldES <- c(YcoldES, mean(dataTMP2$Temp.AvgC2, na.rm=T))
  YwarmES <- c(YwarmES, mean(dataTMP2$Temp.AvgW2, na.rm=T))
  
  dataTMP2 <- dataTMP[which(dataTMP$Region%in%"MI"),]
  
  YannualMI <- c(YannualMI, mean(dataTMP2$Temp.AvgY2, na.rm=T))
  YcoldMI <- c(YcoldMI, mean(dataTMP2$Temp.AvgC2, na.rm=T))
  YwarmMI <- c(YwarmMI, mean(dataTMP2$Temp.AvgW2, na.rm=T))
}

############
#    WE    #
############

AIC(gam(YannualWE~s(Years)),
    gam(YannualWE~te(Years)) )

AIC(gam(YcoldWE~s(Years)),
    gam(YcoldWE~te(Years)) )

AIC(gam(YwarmWE~s(Years)),
    gam(YwarmWE~te(Years)) )

summary(gam(YannualWE~s(Years)))
summary(gam(YcoldWE~s(Years)))
summary(gam(YwarmWE~s(Years)))

#############

ggplot()+
  geom_smooth(aes(x=Years, y=YcoldWE), method = "gam",alpha = 0.1)+
  geom_point(aes(x=Years, y=YcoldWE, color=Ycold), shape=16)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Cold")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YannualWE), method = "gam",alpha = 0.1, color="black")+
  geom_point(aes(x=Years, y=YannualWE, color=Yannual), shape = 15)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YwarmWE), method = "gam",alpha = 0.1, color="darkred")+
  geom_point(aes(x=Years, y=YwarmWE, color=Ywarm), shape = 17)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm")+
  scale_x_continuous(limits = c(min(Years), max(Years)), breaks = seq(1860,2030,10))+
  
  xlab("Years")+ylab("Temperature °C")+ ggtitle("B") +
  
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=7),
        legend.position = "bottom",
        plot.title = element_text(size=20))

ggsave("ImgFinal/Temperature_Years_WE.png", dpi = 300, device = "png")


############
#    MI    #
############

AIC(gam(YannualMI~s(Years)),
    gam(YannualMI~te(Years)) )

AIC(gam(YcoldMI~s(Years)),
    gam(YcoldMI~te(Years)) )

AIC(gam(YwarmMI~s(Years)),
    gam(YwarmMI~te(Years)) )

summary(gam(YannualMI~s(Years)))
summary(gam(YcoldMI~s(Years)))
summary(gam(YwarmMI~s(Years)))

#############

ggplot()+
  geom_smooth(aes(x=Years, y=YcoldMI), method = "gam",alpha = 0.1)+
  geom_point(aes(x=Years, y=YcoldMI, color=Ycold), shape=16)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Cold")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YannualMI), method = "gam",alpha = 0.1, color="black")+
  geom_point(aes(x=Years, y=YannualMI, color=Yannual), shape = 15)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YwarmMI), method = "gam",alpha = 0.1, color="darkred")+
  geom_point(aes(x=Years, y=YwarmMI, color=Ywarm), shape = 17)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm")+
  scale_x_continuous(limits = c(min(Years), max(Years)), breaks = seq(1860,2030,10))+
  
  xlab("Years")+ylab("Temperature °C")+ ggtitle("C") +
  
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=7),
        legend.position = "bottom",
        plot.title = element_text(size=20))

ggsave("ImgFinal/Temperature_Years_MI.png", dpi = 300, device = "png")

############
#    ES    #
############

AIC(gam(YannualES~s(Years)),
    gam(YannualES~te(Years)) )

AIC(gam(YcoldES~s(Years)),
    gam(YcoldES~te(Years)) )

AIC(gam(YwarmES~s(Years)),
    gam(YwarmES~te(Years)) )

summary(gam(YannualES~s(Years)))
summary(gam(YcoldES~s(Years)))
summary(gam(YwarmES~s(Years)))

#############

ggplot()+
  geom_smooth(aes(x=Years, y=YcoldES), method = "gam",alpha = 0.1)+
  geom_point(aes(x=Years, y=YcoldES, color=Ycold), shape=16)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Cold")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YannualES), method = "gam",alpha = 0.1, color="black")+
  geom_point(aes(x=Years, y=YannualES, color=Yannual), shape = 15)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year")+
  
  new_scale_color()+
  geom_smooth(aes(x=Years, y=YwarmES), method = "gam",alpha = 0.1, color="darkred")+
  geom_point(aes(x=Years, y=YwarmES, color=Ywarm), shape = 17)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm")+
  scale_x_continuous(limits = c(min(Years), max(Years)), breaks = seq(1860,2030,10))+
  
  xlab("Years")+ylab("Temperature °C")+ ggtitle("D") +
  
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 12),
        axis.text = element_text(siz=7.5),
        legend.text = element_text(size=7),
        legend.position = "bottom",
        plot.title = element_text(size=20))

ggsave("ImgFinal/Temperature_Years_ES.png", dpi = 300, device = "png")

########################

#########################################
#    Latitude  + Year + Temperature     #
#########################################


setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

WE <- dataIn[which(dataIn$Region=="WE"),]
ES <- dataIn[which(dataIn$Region=="ES"),]
MI <- dataIn[which(dataIn$Region=="MI"),]


##################
#       ES       #
##################


ggplot()+
  geom_point(data = ES, aes(x=Temp.AvgY, y=Lat, colour =Year), shape=15, size=3)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year average")+
  new_scale_colour()+
  geom_point(data = ES, aes(x=Temp.AvgW, y=Lat, colour=Year), shape=17, size=3)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm months")+
  new_scale_colour()+
  geom_point(data = ES, aes(x=Temp.AvgC, y=Lat, colour=Year), shape=16, size=3)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Warm months")+
  scale_y_continuous(limits = c(25,40))+
  #geom_smooth(data = ES, aes(x=Temp.AvgW, y=Lat), method = "loess", se = T, alpha=0.5)+
  xlab("Temperature °C")+ ylab("Latitude")+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_ES.png", dpi = 300, device = "png")


AIC(gam(Lat ~ s(Temp.AvgY)+s(Year)+ti(Temp.AvgY, Year), data = ES),
    gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY, Year), data = ES),
    gam(Lat ~ (Temp.AvgY*Year), data = ES))

summary(gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = ES))
summary(gam(Lat ~ s(Temp.AvgY)+s(Year)+ti(Temp.AvgY, Year), data = ES))

gam_model <- gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = ES)

##################
#       MI       #
##################


ggplot()+
  geom_point(data = MI, aes(x=Temp.AvgY, y=Lat, colour =Year), shape=15, size=3)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year average")+
  new_scale_colour()+
  geom_point(data = MI, aes(x=Temp.AvgW, y=Lat, colour=Year), shape=17, size=3)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm months")+
  new_scale_colour()+
  geom_point(data = MI, aes(x=Temp.AvgC, y=Lat, colour=Year), shape=16, size=3)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Warm months")+
  scale_y_continuous(limits = c(25,40))+
  #geom_smooth(data = ES, aes(x=Temp.AvgW, y=Lat), method = "loess", se = T, alpha=0.5)+
  xlab("Temperature °C")+ ylab("Latitude")+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_MI.png", dpi = 300, device = "png")

AIC(gam(Lat ~ s(Temp.AvgY)+s(Year)+ti(Temp.AvgY, Year), data = MI),
    gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY, Year), data = MI),
    gam(Lat ~ (Temp.AvgY*Year), data = MI))

summary(gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY, Year), data = MI))

##################
#       WE       #
##################


ggplot()+
  geom_point(data = WE, aes(x=Temp.AvgC, y=Lat, colour=Year), shape=16, size=3)+
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Warm months")+
  scale_y_continuous(limits = c(25,40))+
  new_scale_colour()+
  geom_point(data = WE, aes(x=Temp.AvgY, y=Lat, colour =Year), shape=15, size=3)+
  scale_color_gradient(low = "lightgray", high = "black", name = "Year average")+
  new_scale_colour()+
  geom_point(data = WE, aes(x=Temp.AvgW, y=Lat, colour=Year), shape=17, size=3)+
  scale_color_gradient(low = "lightpink", high = "darkred", name = "Warm months")+
  scale_y_continuous(limits = c(29,40))+

  #geom_smooth(data = ES, aes(x=Temp.AvgW, y=Lat), method = "loess", se = T, alpha=0.5)+
  xlab("Temperature °C")+ ylab("Latitude")+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_line(colour = "#adb5bd", linetype = "dotted"),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_WE.png", dpi = 300, device = "png")

AIC(gam(Lat ~ s(Temp.AvgY)+s(Year)+ti(Temp.AvgY, Year), data = WE),
    gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY, Year), data = WE),
    gam(Lat ~ (Temp.AvgY*Year), data = WE))

summary(gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = WE))
