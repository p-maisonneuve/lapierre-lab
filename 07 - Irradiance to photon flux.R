# LapierreLab
# Author: Philippe Maisonneuve
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script converts irradiance spectra (W/m^2) into photon flux spectra ()
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

photon_flux <- function(x)
  
{
  Ed0_Spectrum(x)*x*0.836E-2
  
}
plot(photon_flux, 313, 550)
