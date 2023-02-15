#### Plotting the results of wheat growth simulation
#### using a future climate model for Brasilia, Brazil.
library(here)
library(ggplot2)
library(dplyr)
library(magrittr)

here::here()

###Read csv file
cc_model <- read.csv("./results/cc-model/CC-model-Simulation.csv")

str(cc_model)

ggplot(cc_model,aes(x=Label,y=Yield))+
  geom_point()
