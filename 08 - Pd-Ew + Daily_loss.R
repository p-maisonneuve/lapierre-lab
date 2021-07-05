# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script calculates PD-Ew (DOC photodegradation rates per joule of energy)
# And multiplies those rate by the average daily areal energy (J/m^2) absorbed 
# in the water column during the mission (see script 06 and 04 for more info)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(readr)
library(readxl)


Average_dose_vial <- read_csv("~/Documents/Labo Lapierre/Maîtrise/Data/2019/Average_areal_light_dose_Vial.csv")
Average_areal_light_dose_WaterColumn <- read_csv("~/Documents/Labo Lapierre/Maîtrise/Data/2019/Average_areal_light_dose_WaterColumn_T0.csv")
MSL2019_Data <- read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx")

sites <- MSL2019_Data$Stations
E_abs_vial <- Average_dose_vial$avg_absorbed_Energy_24h
Pd <- MSL2019_Data$Average_Loss_DOC_daily
E_WC <- Average_areal_light_dose_WaterColumn$avg_Daily_Energy

# rate of DOC loss (mg/L)  per energy dose (J/m^2) 
Pd_E <- Pd/E_vial

# Estimation of daily DOC loss (mgL⋅L-1⋅m−2⋅d−1) at a given site
Daily_Loss_insitu <- Pd_E*E_WC

#Ratio of daily energy in the WC : energy in the vial
Ratio_WC_lamp <- data.frame(E_WC/E_vial)

output <- data.frame(cbind(sites, E_abs_vial, Pd, E_WC, Pd_E, Daily_Loss_insitu, Ratio_WC_lamp))
output

setwd("~/Documents/Labo Lapierre/Maîtrise/Data/2019")
write.csv(x = output, file = "Photo_summary_T0.csv", fileEncoding = "UTF-8")

mean(Daily_Loss_insitu, na.rm = T)

Ratio_WC_lamp <- data.frame(E_WC/E_vial)
rownames(Ratio_WC_lamp) <- sites


plot(Ratio_WC_lamp$E_WC.E_vial~MSL2019_Data$km, pch= 19, col = colors, ylim=c(0,1.1))
colors <- c("brown", "green")
colors <- colors[as.factor(MSL2019_Data$`Water mass`)]
text(MSL2019_Data$km, Ratio_WC_lamp$E_WC.E_vial, labels=MSL2019_Data$Stations, cex= 0.7, pos = 3)


Pd_E*10^3

