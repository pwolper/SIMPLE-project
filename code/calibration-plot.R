## Sim/Obs Plot of the results of the calibration of the SIMPLE model to experimental data
library(ggplot2)
library(dplyr)
library(here)

here::here()

BRS264 <- read.csv("./results/2023-02-15_experiments_all.csv")


BRS264$location <- c()
BRS264$irri <- c()
BRS264$loc_year <- c()
BRS264$fungal <- ifelse(BRS264$Label=="SAOG_22_nonirrigated"|
                        BRS264$Label=="SAOG_21_nonirrigated"|
                        BRS264$Label=="SAOG_20_nonirrigated", "fungal", "none")

for(i in 1:nrow(BRS264)){
  BRS264$location[i] <- unlist(unlist(strsplit(BRS264$Label[i], split = "_"))[1])
  BRS264$irri[i] <- unlist(unlist(strsplit(BRS264$Label[i], split = "_"))[3])
  BRS264$loc_year[i] <- gsub("^([^_]+_[^_]+).*","\\1",BRS264$Label[i])
}

BRS264$irri <- as.factor(BRS264$irri)
BRS264$location <- as.factor(BRS264$location)

str(BRS264)
BRS264[BRS264$fungal == "none",]

# Plotting of results
ggplot(BRS264,aes(x=Sim_Yield,y=Obs_Yield))+
  geom_point(aes(color=location,shape=irri), size = 5)+
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1000,5500)) +
  scale_y_continuous(limits = c(1000,5500)) +
  labs(title = "Simulation of wheat growth in Mato Grosso, Brazil", subtitle = "13 experiments were simulated and compared to the observed yield")

# Summary statistics

lm <- lm(data = BRS264,Sim_Yield ~ Obs_Yield)
summary(lm)

lm_nof <- lm(data = BRS264[BRS264$fungal == "none",], Sim_Yield ~ Obs_Yield)
summary(lm_nof)
