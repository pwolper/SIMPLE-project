#if (!require("remotes")) {
#install.packages("remotes")
#}

#remotes::install_github("ropensci/nasapower")

#install.packages("TDPanalysis")
library(readr)
library(nasapower)
library("TDPanalysis")
library("tidyr")


###Obtaining data from Nasa Power
INSI <- "FREI" ##Code for the location
LAT <- 48.40 #Latitude
LONG <- 11.44 #Longitude
ELEV <- 448 #Elevation

###Dates
From_Day <- "1985-01-01" #Fisrt day of the period, right order is "year-month-day"
To_Day <-  "2022-12-01"  #Last day of the period

#
nasa <- get_power(
  community = "ag",
  lonlat = c(LONG, LAT),
  pars = c("ALLSKY_SFC_SW_DWN", "T2M_MAX","T2M_MIN", "PRECTOTCORR"),
  dates =c(From_Day,To_Day),
  temporal_api = "daily"
)


#
DATE <- c("DATE",paste(substr(nasa$YEAR, 3, 4), sprintf("%03d", nasa$DOY), sep = ""))
SRAD <- c("SRAD",as.numeric(formatC(nasa$ALLSKY_SFC_SW_DWN, width = 1, flag = "")))  
TMAX <- c("TMAX",as.numeric(formatC(nasa$T2M_MAX, width = 1, flag = "")))
TMIN <- c("TMIN",as.numeric(formatC(nasa$T2M_MIN, width = 1, flag = "")))
RAIN <- c("RAIN",as.numeric(formatC(nasa$PRECTOTCORR, width = 1, flag = "")))
B1 <- rep("", times = length(RAIN))
B2 <- rep("", times = length(RAIN))
B3 <- rep("", times = length(RAIN))
B4 <- rep("", times = length(RAIN))


SRAD[is.na(SRAD)] <- -99
TMAX[is.na(TMAX)] <- -99
TMIN[is.na(TMIN)] <- -99
RAIN[is.na(RAIN)] <- -99



#
Weather <- data.frame(INSI = DATE, LAT = SRAD, LONG = TMAX, ELEV = TMIN, TAV = RAIN, AMP = B1,
                      REFHT = B2, WNDHT = B3)

Max <- mean(nasa[!is.na(nasa$T2M_MAX),]$T2M_MAX)
Min <- mean(nasa[!is.na(nasa$T2M_MIN),]$T2M_MIN)
#
TAV <- c("TAV",round(mean(Max, Min, digits = 2)))
AMP <- c("AMP",round(mean(Max - Min , digits = 2)))
REFHT <- c("REFHT",-99.0)
WNDHT <- c("WNDHT",-99.0)




Complement <- data.frame(INSI = c("@INSI", INSI), LAT = c("LAT", LAT), LONG = c("LONG", LONG), ELEV = c("ELEV", ELEV), TAV, AMP, REFHT, WNDHT)

Complement <- format(Complement, digits=1)


Final <- rbind(Complement,Weather)


for (i in 1:ncol(Final)) {
  Final[,i] <- formatC(Final[,i], width = 1, flag = "")
}


names(Final) <- format(names(Final),  width = 1, sep = "", justify = "left")

for (i in 1:ncol(Final)) {
  Final[1,i] <- formatC(Final[1,i], width = 1, flag = "")
}

Final1 <- Final


for (i in 2:nrow(Final)) {
  Final[i,] <- Final1[i-1,]  
}

Final[1,] <- c(rep("", times = ncol(Final)))
Final[nrow(Final)+1,] <- Final1[nrow(Final1),]

##Exporting
setwd("") ##Directory to be exported in


name_file <- "FREIDE.WTH" ##Name of the file to be export


write.table(Final, file = name_file, sep = " ", col.names = c(paste("*WEATHER : ", INSI), rep("", times = ncol(Final)-1)),
            row.names = F, dec = ".", quote = F)



