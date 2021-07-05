# Figure curves

# Libraries

if (!require("plyr")) install.packages("plyr")
library(plyr)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("minpack.lm")) install.packages("minpack.lm")
library(minpack.lm)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)



# *How to organize your data for this script* #
# A first column named "Time" containing all of your degradation times
# All following columns with the name of your sample followed by all of your 
# DOC concentration for the associated degradation times
# All that saved in a .csv file

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/R/Projects/Decay rates/bio_msl2019.xlsx"))
#View(data)



# Create a data frame to store the variables calculated by the model
output = data.frame(x = 1, a = 1, v = 1, k = 1, R2 = 1, rate_B1 = 1, rate_B2 = 1, rate_B7 = 1, rate_B1sec = 1, rate_B112 = 1, Ambient_DOC = 1, SPC = 1)
colnames(output) <- c("Sample", "a", "v", "k", "R2", "rate_B1", "rate_B2", "rate_B7", "rate_B1sec", "rate_B112", "Ambient_DOC", "SPC")


#Plot the graph area and axis
plot(DOC_proportion*100 ~ Time, data = data2, 
     xlim = c(0, 112), ylim = c(0.6, 1), 
     ylab = "Proportion of ambient concentration",
     xlab = "Time (days)",
     main = "Biological degradation (n=40)", 
     cex.axis=1.2,
     cex.lab=1.2, 
     axes=F)

box(bty="l")
axis(2, las=1)
axis(1)

#Plot the curves

palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(output$SPC,breaks = 50))]

for (i in 1:40){
  
curve(((output$a[i] / (output$a[i] + x))^output$k[i]), add =T, col = col[i])
  
}



##########################
for(i in 2:length(data))
  
 {

  # Organize the data
  sample <- select(data, i)
  DOC_zero <- data[1,i]
  DOC_proportion <- data[,i]/DOC_zero
  data2 <- cbind(DOC_proportion, data)
  
  # Run the model
  RCmodel <- nlsLM(DOC_proportion ~ (a / (a + Time))^v,
                   data = data2,
                   start = list(a = 20, v = 0.1),
                   lower = c(-2,-2),
                   upper = c(60, 1))
  
  RCmodel.param = summary(RCmodel)$parameters
  aaa <- RCmodel.param[1] # Calculate a
  vvv <- RCmodel.param[2] # Calculate v
  kkk <- vvv/aaa          # Calculate k
  R2 <-  1 - sum((DOC_proportion - predict(RCmodel))^2) / sum((DOC_proportion-mean(DOC_proportion))^2)
  
  #Store the data
  output[i-1,1] <- colnames(sample)
  output[i-1,2] <- aaa
  output[i-1,3] <- vvv
  output[i-1,4] <- kkk
  output[i-1,5] <- R2
  
  #Plot the curve
  g1+stat_function(fun=(function(x) (((RCmodel.param[1] / (RCmodel.param[1] + x))^RCmodel.param[2]))*DOC_zero))
  
  plot(DOC_proportion ~ Time, data = data2, 
       xlim = c(0, 112), ylim = c(0.6, 1), 
       main = colnames(data2)[i+1])
  
  curve((RCmodel.param[1] / (RCmodel.param[1] + x))^RCmodel.param[2], add =T)
  
  
  #Interpolate the relative degradation rate after 1 day (in % of DOC lost)
  output[i-1,6] <- (RCmodel.param[1] / (RCmodel.param[1] + 1))^RCmodel.param[2]
  
  #...after 2 days
  output[i-1,7] <- (RCmodel.param[1] / (RCmodel.param[1] + 2))^RCmodel.param[2]
  
  #...after 7 days
  output[i-1,8] <- (RCmodel.param[1] / (RCmodel.param[1] + 7))^RCmodel.param[2]
  
  #...after 1 second
  output[i-1,9] <- (RCmodel.param[1] / (RCmodel.param[1] + 0.000001))^RCmodel.param[2]
  
  #...after 112 days
  output[i-1,10] <- (RCmodel.param[1] / (RCmodel.param[1] + 112))^RCmodel.param[2]
  
  #Ambient DOC
  output[i-1,11] <- DOC_zero
  
}


data_main <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data_main)

#SPC
output[12] <- SPC
data
write.csv2(output, file = "decay_avk_vf3.csv")

kval <- data.frame(output$k)

rate_1s<-(1-output$rate_B1sec)*DOC_zero*86400
rate_1d<-(1-output$rate_B1)*DOC_zero
plot(rate_1d~rate_1s)
abline(0,1, col="red", lty="dashed")


#####

