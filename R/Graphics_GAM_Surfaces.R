library(mgcv)
library(plotly)
library(ggnewscale)
library(ggplot2)
library(viridis)
library(metR) 


#########################################
#    Latitude  + Year + Temperature     #
#########################################


setwd("/media/omar/Extreme Pro/Projects/Tbrasiliensis/")

dataIn <- read.csv("Data/Occurences_Tb_TempPeriods_FINAL.csv")
head(dataIn)

summary(dataIn$Lat)

length(which(dataIn$Lat<29.89))
length(which(dataIn$Lat>36.10))


WE <- dataIn[which(dataIn$Region=="WE"),]
ES <- dataIn[which(dataIn$Region=="ES"),]
MI <- dataIn[which(dataIn$Region=="MI"),]


ModelES <- gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = ES)
ModelMI <- gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = MI)
ModelWE <- gam(Lat ~ s(Temp.AvgY)+s(Year)+te(Temp.AvgY,Year), data = WE)

summary(ModelES)
summary(ModelMI)
summary(ModelWE)
#########################################
#         Plot ES GAM Surface           #
#########################################

# Create a grid of Temp.AvgY and Year values for prediction
grid <- expand.grid(
  Temp.AvgY = seq(min(ES$Temp.AvgY, na.rm = T), max(ES$Temp.AvgY, na.rm = T), length.out = 100),
  Year = seq(1930, 2050, length.out = 100) )

# Predict Lat values for the grid
grid$Lat_pred <- predict(ModelES, newdata = grid, type = "response", se.fit = F)

# Define equal-width breaks (7 intervals)
lat_breaks <- seq(0, 50, length.out = 9)  # 7 intervals (8 break points)
lat_labels <- paste0(lat_breaks[-length(lat_breaks)], "-", lat_breaks[-1])  # Labels like "24-2


# Plot the response surface
ggplot(grid, aes(x = Temp.AvgY, y = Year, z = Lat_pred)) +
  geom_contour_filled(aes(fill = after_stat(level)), breaks = lat_breaks, color = "black", linewidth = 0.6) +
  #geom_contour_filled() +
  labs(title = "GAM Response Surface for Latitude (ES Region)",
       x = "Mean Annual Temperature °C",
       y = "Year",
       fill = "Predicted Latitude") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_PRED_ES.png", dpi = 300, device = "png")

###############

# Create a grid of Temp.AvgY and Year values for prediction
grid <- expand.grid(
  Temp.AvgY = seq(min(MI$Temp.AvgY, na.rm = T), max(MI$Temp.AvgY, na.rm = T), length.out = 100),
  Year = seq(1930, 2050, length.out = 100) )

# Predict Lat values for the grid
grid$Lat_pred <- predict(ModelMI, newdata = grid, type = "response")

# Define equal-width breaks (7 intervals)
lat_breaks <- seq(22, 44, length.out = 9)  # 7 intervals (8 break points)
lat_labels <- paste0(lat_breaks[-length(lat_breaks)], "-", lat_breaks[-1])  # Labels like "24-2

# Plot the response surface
ggplot(grid, aes(x = Temp.AvgY, y = Year, z = Lat_pred)) +
  #geom_contour_filled() +
  geom_contour_filled(aes(fill = after_stat(level)), breaks = lat_breaks) +
  geom_contour(breaks = lat_breaks, color = "black", linewidth = 0.6) +  # Contour lines
  labs(title = "GAM Response Surface for Latitude (MI Region)",
       x = "Mean Annual Temperature °C",
       y = "Year",
       fill = "Predicted Latitude") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_PRED_MI.png", dpi = 300, device = "png")


###############

# Create a grid of Temp.AvgY and Year values for prediction
grid <- expand.grid(
  Temp.AvgY = seq(min(WE$Temp.AvgY, na.rm = T), max(WE$Temp.AvgY, na.rm = T), length.out = 100),
  Year = seq(1930, 2050, length.out = 100) )

# Predict Lat values for the grid
grid$Lat_pred <- predict(ModelWE, newdata = grid, type = "response", se.fit = F)


# Define equal-width breaks (7 intervals)
lat_breaks <- seq(15, 50, length.out = 9)  # 7 intervals (8 break points)
lat_labels <- paste0(lat_breaks[-length(lat_breaks)], "-", lat_breaks[-1])  # Labels like "24-2

# Plot the response surface
ggplot(grid, aes(x = Temp.AvgY, y = Year, z = Lat_pred)) +
  geom_contour_filled(aes(fill = after_stat(level)), breaks = lat_breaks) +
  geom_contour(breaks = lat_breaks, color = "black", linewidth = 0.6) +  # Contour lines
  #geom_contour_filled() +
  labs(title = "GAM Response Surface for Latitude (WE Region)",
       x = "Mean Annual Temperature °C",
       y = "Year",
       fill = "Predicted Latitude") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(siz=10),
        legend.text = element_text(size=12),
        legend.position = "bottom")

ggsave("ImgFinal/Lat_TempYear_PRED_WE.png", dpi = 300, device = "png")

#########################
