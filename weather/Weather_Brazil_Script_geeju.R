#install.packages("nasapower") # For the installation of the pacakage
library(nasapower)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)

Data.nasa.power <- get_power(community = 'ag', 
          lonlat = c(11.7,48.4),
          temporal_api = 'daily',
          pars = c("T2M","PRECTOTCORR","T2M_MAX"),
          dates = c("1985-01-01","2022-01-01"))


#Data.nasa.power[Times,columns]

#Data_abov_oct <- Data.nasa.power[Data.nasa.power$MM >= 10,]

#Data.nasa.power[Data.nasa.power $MM <= 7,]

Data <- Data.nasa.power[Data.nasa.power $MM <= 7 | Data.nasa.power $MM >= 10,]

Data$Crop_Season <- 0

for (y in unique(Data$YEAR)[-1]) {
  
  Data[Data$YEAR == y-1 & Data$MM >= 10,]$Crop_Season <- y
  Data[Data$YEAR == y & Data$MM <= 7,]$Crop_Season <- y 
  
  print(y)
}

Data_avg <- aggregate(data = Data, . ~ MM:Crop_Season, mean)

Data_avg$MM <- factor(Data_avg$MM, levels = c(10,11,12,1,2,3,4,5,6,7))

#Data_avg$order <- NA
#Data_avg[Data_avg$MM == 10,]$order <- 1
#Data_avg[Data_avg$MM == 11,]$order <- 2
#Data_avg[Data_avg$MM == 12,]$order <- 3
#Data_avg[Data_avg$MM == 1,]$order <- 4
#Data_avg[Data_avg$MM == 2,]$order <- 5
#Data_avg[Data_avg$MM == 3,]$order <- 6
#Data_avg[Data_avg$MM == 4,]$order <- 7
#Data_avg[Data_avg$MM == 5,]$order <- 8
#Data_avg[Data_avg$MM == 6,]$order <- 9
#Data_avg[Data_avg$MM == 7,]$order <- 10


Data_one_crop_season <- Data_avg[Data_avg$Crop_Season == 2018,]

a <- ggplot() + 
  geom_boxplot(data = Data_avg, 
               aes(MM,T2M), outlier.shape = NA) +
  geom_point(data = Data_one_crop_season,
             aes(MM, T2M), color = 'darkorchid', shape = 8) +
  scale_x_discrete("Month") + scale_y_continuous("Temperature (C)")

  

b <- ggplot() + 
  geom_boxplot(data = Data_avg, 
               aes(MM, PRECTOTCORR), outlier.shape = NA) + 
  geom_point(data = Data_one_crop_season,
             aes(MM, PRECTOTCORR), color = 'darkorchid', shape = 8) +
  scale_x_discrete("Month") + scale_y_continuous("Rainfall (mm)")


gridExtra::grid.arrange(a,b,ncol=2)

###### Part 2 ######
Data$heat_days <- 0

Data[Data$T2M_MAX >= 30,]$heat_days <- 1 

Data$drought <- 0 

Data[Data$PRECTOTCORR <= 0.1,]$drought <- 1
Extremes_data <- aggregate(data = Data, . ~ MM:Crop_Season, sum)

Extremes_data$MM <- factor(Extremes_data$MM, levels = c(10,11,12,1,2,3,4,5,6,7))

# Extremes_data$order <-0
# Extremes_data[Extremes_data$MM == 10,]$order<-1 
# Extremes_data[Extremes_data$MM == 11,]$order<-2 
# Extremes_data[Extremes_data$MM == 12,]$order<-3 
# Extremes_data[Extremes_data$MM == 1,]$order<-4 
# Extremes_data[Extremes_data$MM == 2,]$order<-5 
# Extremes_data[Extremes_data$MM == 3,]$order<-6 
# Extremes_data[Extremes_data$MM == 4,]$order<-7 
# Extremes_data[Extremes_data$MM == 5,]$order<-8 
# Extremes_data[Extremes_data$MM == 6,]$order<-9 
# Extremes_data[Extremes_data$MM == 7,]$order<-10 

Extremes_one_crop_season <-
  Extremes_data[Extremes_data$Crop_Season == 2018,]


c <- ggplot() + 
  geom_boxplot(data = Extremes_data, 
               aes(MM, heat_days), outlier.shape = NA) +
  geom_point(data = Extremes_one_crop_season,
             aes(MM, heat_days), color = 'darkorchid', shape = 8) +
  scale_x_discrete("Month") + scale_y_continuous("Number of Days > 30C")



d <- ggplot() + 
  geom_boxplot(data = Extremes_data, 
               aes(MM, drought), outlier.shape = NA) + 
  geom_point(data = Extremes_one_crop_season,
             aes(MM, drought), color = 'darkorchid', shape = 8) +
  scale_x_discrete("Month") + scale_y_continuous("Number of Drought Days")
  

gridExtra::grid.arrange(a,b,c,d,ncol=2)

