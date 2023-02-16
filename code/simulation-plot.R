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


lm_cc <- lm(data = cc_model,Yield ~ Label)
summary(lm_cc)

slope <- summary(lm_cc)$coefficients[2,1]
intercept <- summary(lm_cc)$coefficients[1,1]

ggplot(cc_model,aes(x=Label,y=Yield))+
  geom_point()+
  geom_abline(slope = slope, intercept = intercept) +
  theme_light()
