## Sim/Obs Plot of the results of the calibration of the SIMPLE model to experimental data
library(ggplot2)
library(dplyr)
library(here)

here::here()

BRS264 <- read.csv("./results/2023-02-15_experiments_all.csv")

str(BRS264)

ggplot(BRS264,aes(x=Sim_Yield,y=Obs_Yield))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1000,5500)) +
  scale_y_continuous(limits = c(1000,5500))
