## Sim/Obs Plot of the results of the calibration of the SIMPLE model to experimental data
library(ggplot2)
library(dplyr)
library(here)
library(Metrics)
library(hydroGOF)

setwd(here::here())

BRS264 <- read.csv("./results/2023-02-18_experiments_all_415ppm.csv")

simName <- "Obs_Sim_all_415"

BRS264$location <- c()
BRS264$irrigation <- c()
BRS264$loc_year <- c()
BRS264$fungal <- ifelse(BRS264$Label=="SAOG_22_nonirrigated"|
                        BRS264$Label=="SAOG_21_nonirrigated"|
                        BRS264$Label=="SAOG_20_nonirrigated", "fungal", "none")

for(i in 1:nrow(BRS264)){
  BRS264$location[i] <- unlist(unlist(strsplit(BRS264$Label[i], split = "_"))[1])
  BRS264$irrigation[i] <- unlist(unlist(strsplit(BRS264$Label[i], split = "_"))[3])
  BRS264$loc_year[i] <- gsub("^([^_]+_[^_]+).*","\\1",BRS264$Label[i])
}

BRS264$irri <- as.factor(BRS264$irri)
BRS264$location <- as.factor(BRS264$location)

str(BRS264)

BRS264_nof <- BRS264[BRS264$fungal == "none",]

BRS264_vico <- BRS264[BRS264$location == "VICO",]
BRS264_vico

# Plotting of results
ggplot(BRS264,aes(x=Sim_Yield,y=Obs_Yield))+
  geom_point(aes(color=location,shape=irrigation), size = 5)+
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(1000,5500)) +
  scale_y_continuous(limits = c(1000,5500)) +
  labs(caption = "Fig. 2", x = "Simulated Yield (t ha-1)", y  = "Observed yield (t ha-1)") +
  theme_light()

filename <- paste0("./results/experimental-data/",format(Sys.time(),"%Y-%m-%d_"),simName)
ggsave(paste0(filename,".png"),device = "png", bg = "white", width = 10, height = 8)
# Summary statistics

## r.squared
lm <- lm(data = BRS264,Sim_Yield ~ Obs_Yield)
r_all <- summary(lm)$r.squared

lm_nof <- lm(data = BRS264_nof, Sim_Yield ~ Obs_Yield)
r_nof <- summary(lm_nof)$r.squared

lm_vico <- lm(data = BRS264_vico, Sim_Yield ~ Obs_Yield)
r_vico <- summary(lm_vico)$r.squared

r_squared <- c(r_all, r_nof, r_vico)
r_squared

##Mean absolute error
mae <- c(mae(BRS264$Obs_Yield,BRS264$Sim_Yield),
         mae(BRS264_nof$Obs_Yield,BRS264_nof$Sim_Yield),
         mae(BRS264_vico$Obs_Yield,BRS264_vico$Sim_Yield))

mae

## Root mean squared error
rmse <- c(rmse(BRS264$Obs_Yield,BRS264$Sim_Yield),
          rmse(BRS264_nof$Obs_Yield,BRS264_nof$Sim_Yield),
          rmse(BRS264_vico$Obs_Yield,BRS264_vico$Sim_Yield))

rmse

## Index of Agreement (d)
md <- c(md(BRS264$Sim_Yield,BRS264$Obs_Yield),
        md(BRS264_nof$Sim_Yield,BRS264_nof$Obs_Yield),
        md(BRS264_vico$Sim_Yield,BRS264_vico$Obs_Yield))

md
#### Why is md smaller for the nof data??

# Write to csv
                                        #
stats <- data.frame(r_squared,mae, rmse, md)
rownames(stats) = c("All","healthy","Vicosa")
stats

filename_csv <- paste0("./results/experimental-data/",format(Sys.time(),"%Y-%m-%d_"),simName,"_stats")
write.csv(stats, paste0(filename_csv,".csv"),quote = FALSE)
