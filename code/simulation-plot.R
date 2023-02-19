#### Plotting the results of wheat growth simulation
#### using a future climate model for Brasilia, Brazil.
library(here)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(magrittr)

setwd(here::here())

simName <- "yield_prediction_cc_model_CO2"
filename <- paste0("./results/cc-model/",format(Sys.time(),"%Y-%m-%d_"),simName)

###Read csv file
cc_model <- read.csv("./results/cc-model/CC-model-Simulation.csv")

str(cc_model)


glm_cc <- glm(data = cc_model,Yield ~ Label + Exp)
summary(glm_cc)

slope <- summary(glm_cc)$coefficients[2,1]
intercept <- summary(glm_cc)$coefficients[1,1]

plot1 <- ggplot(cc_model[cc_model$Exp == "CC-model",],mapping = aes(x=Label,y=Yield))+
  geom_line(color = "orange",lwd=1) +
  geom_point(color = "black", size = 3)+
  #geom_abline(slope = slope, intercept = intercept) +
  geom_smooth(data = cc_model[cc_model$Exp == "CC-model",],aes(x=Label,y=Yield), method = "glm", color = "blue")+
  scale_y_continuous(limits = c(2000,5000), breaks = seq(2000,5000,by = 500)) +
  scale_x_continuous(breaks = seq(2030,2100, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size = 16)) +
  labs(x = "Year", y = "Simulated yield (t ha-1)")
plot1


plot2 <- ggplot(cc_model[cc_model$Exp == "CC-model_CO2",],mapping = aes(x=Label,y=Yield))+
  geom_line(color = "orange",lwd=1) +
  geom_point(color = "black", size = 3)+
  #geom_abline(slope = slope, intercept = intercept) +
  geom_smooth(data = cc_model[cc_model$Exp == "CC-model_CO2",],aes(x=Label,y=Yield), method = "glm", color = "red")+
  scale_y_continuous(limits = c(2000,5000), breaks = seq(2000,5000,by = 500)) +
  scale_x_continuous(breaks = seq(2030,2100, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size = 16)) +
  labs(x = "Year", y = "Simulated yield (t ha-1)")
plot2

comb <- ggpubr::ggarrange(plot1,plot2 + ylab(NULL),nrow = 1, labels = "AUTO")

ggsave(paste0(filename,".png"),comb,device = "png", width = 10, height = 6, bg = "white")
