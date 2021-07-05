# Figure curves

# Libraries
if (!require("readxl")) install.packages("readxl")
library(readxl)
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

#Import the data
data <- data.frame(read_excel("Documents/Labo Lapierre/Maîtrise/Rédaction/Figures/Figure - degradation curves/data_photo_deg.xlsx"))
attach(data)
#View(data)

#Create a data frame to store the parameters of the linear model + SPC
output  = data.frame(Site = 0, slope = 0, intercept = 0, SPC = 0)

#Plot the graph area and axis
time <- rbind(0,24,48,72)
y_na <- rbind(5,5,5,5)
plot(y_na ~ time, 
     xlim = c(0, 72), ylim = c(0.7, 1), 
     ylab = "Proportion of ambient concentration",
     xlab = "Time (hours)",
     main = "Photochemical degradation (n=40)", 
     cex.axis=1.2,
     cex.lab=1.2, 
     axes=F)

box(bty="l")
axis(2, las=1)
axis(1)



#Calculate the slopes
for(i in 1:40)
{

  DOC_proportion <- data[,i+1]
  time <- rbind(0,24,48,72)
  
  lm1<-lm(DOC_proportion~time)
  output[i,2]<- lm1$coefficients[2]
  output[i,3]<- lm1$coefficients[1]
  
  summary(lm1)
}

output[4]<-SPC
output<-data.frame(output)
output$slope<-as.numeric(as.character(output$slope))
output$intercept<-as.numeric(as.character(output$intercept))
output$SPC<-as.numeric(as.character(output$SPC))

#Plot the curves


palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(output$SPC,breaks = 50))]


for (i in 1:40){
  
  curve((output$slope[i] * x)+output$intercept[i], add =T, col = col[i])
  
}


