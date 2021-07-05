# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script compares CDOM values measured values to light attenuation coefficient (Kd) 
# measured in the whole water column with a PUV-800 radiospectrometer
# R^2=1 indicates that all of the absorbed light is absorbed by dissolved matter
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Importing CDOM data
CDOM <- data.frame(Absorbance_MSL2019 <- read.csv("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019 - CDOM/Absorbance_MSL2019.csv", sep = ";"))

# Importing Kd values
Kd <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))

sample <- Kd$Stations

output = matrix(0,ncol = 2, nrow = nrow(Kd))
colnames(output) = c("CDOM", "Kd")


# At 313 nm
for(i in 1:40)
{

  output[i,1] <-  CDOM[CDOM$WL==313,paste0(sample[i],'.P0')]
  output[i,2] <- Kd[i,which(colnames(Kd)=="Kd_313")]
}
output <- data.frame(output)
lm1 <- lm(output$Kd~output$CDOM)
plot(output, pch=19)
abline(lm1, col = "red")
summary(lm1)
text(output$Kd~output$CDOM, labels=sample, cex= 0.7, pos = 3)

# At 320 nm
for(i in 1:40)
{
  
  output[i,1] <-  CDOM[CDOM$WL==320,paste0(sample[i],'.P0')]
  output[i,2] <- Kd[i,which(colnames(Kd)=="Kd_320")]
}
plot(output)

# At 340
for(i in 1:40)
{
  
  output[i,1] <-  CDOM[CDOM$WL==340,paste0(sample[i],'.P0')]
  output[i,2] <- Kd[i,which(colnames(Kd)=="Kd_340")]
}
plot(output)

# At 443
for(i in 1:40)
{
  
  output[i,1] <-  CDOM[CDOM$WL==443,paste0(sample[i],'.P0')]
  output[i,2] <- Kd[i,which(colnames(Kd)=="Kd_443")]
}
plot(output)

# At 550
for(i in 1:40)
{
  
  output[i,1] <-  CDOM[CDOM$WL==550,paste0(sample[i],'.P0')]
  output[i,2] <- Kd[i,which(colnames(Kd)=="Kd_550")]
}
plot(output)

