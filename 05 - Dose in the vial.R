# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script calculates the average areal light dose (in W*m^-2)
# in a 40mL clear borosilicate glass vial (Fischer Scientific)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Loading required packages
#install.packages("cubature")
library(cubature)
library(readr)
library(readxl)

#Creating a matrix to store the outputed data
output = matrix(0,ncol = 4, nrow = 40)
colnames(output) = c("Station", "avg_LD","avg_absorbed_Energy_24h", "avg_fraction_absorbed")

# Creating a list of all the sampled sites 
setwd("~/Documents/Labo Lapierre/Maîtrise/Data/2019")
MSL2019_Data <- read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx")
sites.names <- MSL2019_Data$Stations

for(s in 1:40)
{
# 1 - Importing the energetic spectrum of the lamp measured in duplicates with a spectroradiometer
#     at the distance of the exposition platter (~37cm)

  #Averaging the two measured spectra replicates 
  setwd("~/Documents/Labo Lapierre/Maîtrise/Photodegradation")    
  Lamp_platter_Spectrum_1 <- read.table("~/Documents/Labo Lapierre/Maîtrise/Photodegradation/0037_JAZ.txt", dec = ",", skip = 17)
  Lamp_platter_Spectrum_2 <- read.table("~/Documents/Labo Lapierre/Maîtrise/Photodegradation/0038_JAZ.txt", dec = ",", skip = 17)
  Lamp_platter_Spectrum_1 <- approxfun(Lamp_platter_Spectrum_1, method = "linear")
  plot(Lamp_platter_Spectrum_1, xlim=c(313,550), ylim=c(0,3), main="Spectra 1")
  Lamp_platter_Spectrum_2 <- approxfun(Lamp_platter_Spectrum_2, method = "linear")
  plot(Lamp_platter_Spectrum_2, xlim=c(313,550), ylim=c(0,3), main="Spectra 2")
  
  #Creating a function averaging the 2 replicates of the incident light spectra
  Lamp_platter_Spectrum <- function(x)
  {
    (Lamp_platter_Spectrum_1(x)+Lamp_platter_Spectrum_2(x))/2 
  }

  #Calculate the power (in Watts) emitted by the lamp and measured at 37 cm from the light source
  cubintegrate(Lamp_platter_Spectrum, lower = 300, upper = 600, method = "pcubature")
  # 466.97 W 
  # About half of the total lamp power (700W) is converted to heat, 466W is the power received by the glass vials
  
  
  #Plotting the resulting spectrum
  plot(Lamp_platter_Spectrum, xlim=c(313,550), ylim=c(0,3), main="Spectre moyen")



# 2 - Importing the spectrum of the fraction of energy passing through the glass
  
  setwd("~/Documents/Labo Lapierre/Maîtrise/Photodegradation")
  Fraction_vial_Spectrum <- read.csv("~/Documents/Labo Lapierre/Maîtrise/Photodegradation/Fraction_vial_Spectrum.csv", sep = ";")
    
  #Creating a function performing a linear interpolation between each wavelength 
  Fraction_vial_Spectrum <- approxfun(Fraction_vial_Spectrum, method = "linear")
  plot(Fraction_vial_Spectrum, xlim=c(190,550), ylim=c(0,1))

  
    x=0
  for (i in 313:450)
  {
    x<-x+Fraction_vial_Spectrum(i)
  }
x/138
  
# 3 - Multiplying the lamp spectrum with the fraction spectrum 
#     to obtain the spectrum of incoming energy into the vial
  
      Energy_vial <- function(x)
        {
          Lamp_platter_Spectrum(x)*Fraction_vial_Spectrum(x) 
        }

plot(Energy_vial, ylim = c(0,5), xlim = c(313,550), main = "Energy incoming into the vial (W/m^2)")

#     3.1 Integrating between 313-550 to obtain the total light dose incoming to the vial
      integrate(Energy_vial, 313, 450)
      ## 236.91

# 4 - Importing the aCDOM spectrum for sample i      
      
      CDOM <- Absorbance_MSL2019 <- data.frame(read_csv("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019 - CDOM/Absorbance_MSL2019.csv"))
      CDOM_i <- CDOM[,which(colnames(CDOM)==paste0(sites.names[s],".P0"))]
      Spectrum_i <- data.frame(cbind(CDOM$WL, CDOM_i))
      
      #Linear interpolation to 1-nm increments between 313-550nm
      CDOM_Spectrum <- data.frame(approx(Spectrum_i, xout = seq(190, 900, by = 1), method = "linear")) 
      plot(CDOM_Spectrum)
      
      #Creating a function performing the linear interpolation of Kd (between 313-550nm)
      CDOM_Spectrum <- approxfun(Spectrum_i, method = "linear")
      
            
# 5 - Creating the spectrum of the average fraction of penetrating radiation in a tube 
#     
      
      
      
      n_WL <- 138 #Number of wavelength we're integrating over
      L <- (c(0.744, 1.736, 2.128, 2.351, 2.455, 2.455, 2.351, 2.128, 1.736, 0.744))/100
      
      avg_fraction <- data.frame(matrix(0,ncol = 2,nrow = n_WL))
      colnames(avg_fraction) = c("WL", "Fraction") 
      avg_fraction[,1] <- c(313:450)
      

    for(w in 313:450)
    {
    x = 0
      for(i in L)
      {
        x <- x + (1-(exp(-CDOM_Spectrum(w)*i)))*1/10
      }
      row <- w-312
      avg_fraction[row,2] <- x
    }

      avg_fraction_Spectrum <- approxfun(avg_fraction, method = "linear")
      plot(avg_fraction_Spectrum, 313,550, main=paste0("avg_fraction_absorbed for ",sites.names[s]))
      
      x=0
      for (i in 313:450)
      {
        x<-x+avg_fraction_Spectrum(i)
      }
      output[s,4] <- x/138
      
      
# 6 - Calculating the total absorbed energy in the vial (313-450nm)
      
  Irradiation_Time <- 86400 # = 24h*3600s
  
  # Fraction absorbed * Energy incoming
  
  avg_abs_E <- function(i)
  {
    (Energy_vial(i)*avg_fraction_Spectrum(i))
  }
 
  plot(avg_abs_E, xlim=c(313,450), main=paste0("avg_energy_absorbed for ",sites.names[s]))
  avg_LD <- cubintegrate(avg_abs_E, lower = 313, upper = 450, method = "pcubature")
  avg_LD <- as.numeric(avg_LD[1])


  
  
  #Final calculation to obtain total energy absorbed (J/m^2 ; between 313-450nm)
  avg_absorbed_energy_24h <- Irradiation_Time * avg_LD[1]
  
  #Storing the data
  output[s,1] <- sites.names[s]
  
  output[s,2] <- avg_LD
  output[s,3] <- avg_absorbed_energy_24h
  
}

setwd("~/Documents/Labo Lapierre/Maîtrise/Data/2019")
write.csv(x = output, file = "Average_areal_light_dose_Vial.csv", fileEncoding = "UTF-8")



####### Junk 

data <- incoming_vial
colnames(data) <- c("WL", "E")

vial_Spectrum <- data.frame(approx(data, xout = seq(300, 450, by = 1), method = "linear"))
plot(vial_Spectrum)

vial_Spectrum <- approxfun(vial_Spectrum, method = "linear")

integrate(vial_Spectrum, 300, 450)$value

