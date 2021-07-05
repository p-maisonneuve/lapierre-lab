# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script converts the .csv file created by Absorbance.R into transmittance data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#Load the Absorbance.csv file
setwd("~/Documents/Lapierre Lab/Maîtrise/Photodegradation")
data <- read.csv("Absorbance_photo_MSL2019.csv", sep = ";")

#Convert to transmitance through a 1.7cm glass vial (Balathandayuthabani et al., 2017)
vial_diameter <- 0.017

for(i in 2:(ncol(data)-1))
{  
  data[,i] <- (data[,i])*vial_diameter
  data[,i] <- 10^(2-data[,i])
}

write.csv(x = data, file = "Transmittance_photo_MSL2019.csv", fileEncoding = "UTF-8")

#Extract specific wavelength to be inputed to the Bala spreadsheet
WL <- c(300,320,340,360,380,400,420,440,450)
dataWL <- subset(data, subset = X %in% WL)
write.csv(x = dataWL, file = "Transmittance_300-450_photo_MSL2019.csv", fileEncoding = "UTF-8")


