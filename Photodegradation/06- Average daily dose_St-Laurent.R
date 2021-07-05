# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script roughly estimates the daily light dose (in J*m^-2) during the sampling mission
# (29-07-2019 to 02-08-2019)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# 1 - Importing and plotting the data
  
  library(readr)
  Ed0_Time <- data.frame(read_delim("~/Documents/Labo Lapierre/Maîtrise/Data/2019/PUV/Ed0_Time.csv", ";", escape_double = FALSE, trim_ws = TRUE))
  plot(Ed0_Time$Ed0~Ed0_Time$Heure..s., na.rm = F, xlim=c(20000, 75000))


# 2 - Fitting a polynomial curve on the data (because it looks a lot like a parabola)
  y <- Ed0_Time$Ed0
  x <- Ed0_Time$Heure..s.
  
  fit1 <- lm(y~poly(x,2, raw = TRUE))
  
  xx <- seq(20000,80000, length.out=500)
  lines(xx, predict(fit1, data.frame(x=xx)), col='blue')

# 3 - Creating a function representing the polynomial curve
  
  fit1$coefficients
  #(Intercept)         poly(x, 2, raw = TRUE)1  poly(x, 2, raw = TRUE)2 
  #-4.735624e+02            2.846672e-02           -3.027564e-07
  
  total_daily_Spectrum <- function(x)
  {
    -3.027564e-07*(x^2)+2.846672e-02*x-4.735624e+02
    
  }
plot(total_daily_Spectrum, 20000,75000, ylim=c(0,300), xlim=c(20000,75000), xlab="Hour of the day (s)", ylab="Surface irradiance (W/m^2)")
points(x,y, pch=19)


#Finding the roots of the polynomial function
#install.packages("rootSolve")
library(rootSolve)
uniroot.all(total_daily_Spectrum, interval = c(10000, 80000))

#Integrating between the roots to estimate a daily light dose (J/m^2)
integrate(total_daily_Spectrum, 21595.79, 72429.37)
# 6628176 with absolute error < 7.4e-08
# 6.62 x 10^6 J/m^2

#Calculating the average surface irradiance by averaging the total_daily_Spectrum function

Spectrum_values <- data.frame(matrix(0,ncol = 2,nrow = 86400))
colnames(Spectrum_values) = c("H", "Irradiance")

for(i in 1:86400)
{
  if(total_daily_Spectrum(i)>0)
  {
  Spectrum_values[i,2] <- total_daily_Spectrum(i)
  Spectrum_values[i,1] <- i
  }
  else
  {
    Spectrum_values[i,2] <- 0
    Spectrum_values[i,1] <- i
  }
  
}




mean(Spectrum_values$Irradiance)
#  W/m^2 
# 76.715
Ed0_Time
# Closest value to 76.715 W/m^2 is at station BOU4 at 17:25 (81.22 W/m^2), therefore we will use as our average surface spectra 
# in script 04- Average light dose_WaterColumn




