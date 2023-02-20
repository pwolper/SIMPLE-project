#### Plotting the results of wheat growth simulation
#### using a future climate model for Brasilia, Brazil.
library(here)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(magrittr)

setwd(here::here())

simName <- "yield_prediction_cc_model_CO2_with_conc"
filename <- paste0("./results/cc-model/",format(Sys.time(),"%Y-%m-%d_"),simName)

###Read csv file
cc_model <- read.csv("./results/cc-model/CC-model-Simulation.csv")
cc_model <- cc_model[order(cc_model$Label),]

cc_model_const <- cc_model[cc_model$Exp == "CC-model",]
cc_model_const$CO2 <- rep(450,times = nrow(cc_model_const))

cc_model_CO2 <- cc_model[cc_model$Exp == "CC-model_CO2",]
cc_model_CO2$CO2  <- c()

for(i in 1:nrow(cc_model_CO2)){
  cc_model_CO2$CO2[i] <- (i-1)*5+450
}

str(cc_model_const);str(cc_model_CO2)

str(cc_model)


glm_cc <- glm(data = cc_model,Yield ~ Label)
summary(glm_cc)

slope <- summary(glm_cc)$coefficients[2,1]
intercept <- summary(glm_cc)$coefficients[1,1]

plot1 <- ggplot(cc_model[cc_model$Exp == "CC-model",],mapping = aes(x=Label,y=Yield))+
  geom_line(color = "orange",lwd=1) +
  geom_point(color = "black", size = 3)+
  #geom_abline(slope = slope, intercept = intercept) +
  geom_smooth(data = cc_model[cc_model$Exp == "CC-model",],aes(x=Label,y=Yield), method = "loess", color = "blue")+
  scale_y_continuous(limits = c(2000,5000), breaks = seq(2000,5000,by = 500)) +
  scale_x_continuous(breaks = seq(2030,2100, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size = 16)) +
  labs(x = "Year", y = "Simulated yield (t ha-1)")

plot_const <- ggplot() +
  geom_line(cc_model_const, mapping = aes(x = Label, y = CO2), color = "darkgreen", lwd = 1) +
  scale_y_continuous(limits = c(400,800)) +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 1),
        plot.background = element_rect(colour = "black", fill="white", size=0.5)) +
  labs(x = "Year", y = "ppm", title = "Atmospheric CO2 concentration")

plot1 <- plot1 + annotation_custom(ggplotGrob(plot_const), xmin = 2030, xmax = 2060, ymin = 1990, ymax = 2750)

plot2 <- ggplot(cc_model[cc_model$Exp == "CC-model_CO2",],mapping = aes(x=Label,y=Yield))+
  geom_line(color = "orange",lwd=1) +
  geom_point(color = "black", size = 3)+
  #geom_abline(slope = slope, intercept = intercept) +
  geom_smooth(data = cc_model[cc_model$Exp == "CC-model_CO2",],aes(x=Label,y=Yield), method = "loess", color = "red")+
  scale_y_continuous(limits = c(2000,5000), breaks = seq(2000,5000,by = 500)) +
  scale_x_continuous(breaks = seq(2030,2100, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size = 16)) +
  labs(x = "Year", y = "Simulated yield (t ha-1)")

plot_incr <- ggplot() +
  geom_line(cc_model_CO2, mapping = aes(x = Label, y = CO2), color = "darkgreen", lwd = 1) +
  scale_y_continuous(limits = c(400,800)) +
  #scale_x_continuous(position = "top") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 1),
        plot.background = element_rect(colour = "black", fill="white", size=0.5)) +
  labs(x = "Year", y = "ppm", title = "Atmospheric CO2 concentration")

plot2 <- plot2 + annotation_custom(ggplotGrob(plot_incr),xmin = 2030, xmax = 2060, ymin = 2000, ymax = 2750)

comb <- ggpubr::ggarrange(plot1,plot2 + ylab(NULL),nrow = 1, labels = "AUTO")
comb

ggsave(paste0(filename,".png"),comb,device = "png", width = 17, height = 10, bg = "white")
