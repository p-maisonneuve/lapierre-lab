# Libraries

if (!require("plyr")) install.packages("plyr")
library(plyr)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("minpack.lm")) install.packages("minpack.lm")
library(minpack.lm)



# *How to organize your data for this script* #
# A first column named "Time" containing all of your degradation times
# All following columns with the name of your sample followed by all of your 
# DOC concentration for the associated degradation times
# All that saved in a .csv file

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/R/Projects/Decay rates/bio_msl2019.xlsx"))
#View(data)



# Create a data frame to store the variables calculated by the model
output = data.frame(x = 1, a = 1, v = 1, k = 1, R2 = 1, rate_B1 = 1, rate_B2 = 1, rate_B7 = 1, rate_B1sec = 1, rate_B112 = 1)
colnames(output) <- c("Sample", "a", "v", "k", "R2", "rate_B1", "rate_B2", "rate_B7", "rate_B1sec", "rate_B112")


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
  
  #Plot the data
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
  
}

  write.csv2(output, file = "decay_avk_vf3.csv")

kval <- data.frame(output$k)

rate_1s<-(1-output$rate_B1sec)*DOC_zero*86400
rate_1d<-(1-output$rate_B1)*DOC_zero
plot(rate_1d~rate_1s)
abline(0,1, col="red", lty="dashed")


  #####


ggplot(data, aes(x = Time, y = DOC_proportion)) + geom_point() +
  labs(x = "Temps (jours)", y = "DOC (mg/L) \n") + ggtitle(colnames(sample)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(0, 155), ylim = c(0.7,1.0)) + geom_smooth(method = auto)
  




ggplot(data.predict, aes(Time, DOC_proportion)) + geom_point() + geom_line(aes(Time, data.predict$`predict(RCmodel, interval = "confidence")`))
loggammas=deriv(~(exp(aa)*(exp(aa)+xx)^-1)^exp(vv),c("aa","vv"),function(xx,aa,vv){})
plot(DOC_proportion~Time, data = data.predict)
aa <- summary(RCmodel)$parameters[1,1]
vv <- summary(RCmodel)$parameters[2,1]
curve(loggammas(DOC_proportion, aa, vv), from = 0, to = 30)
summary(RCmodel)$parameters


data <- MSL2019_Data
data<- data.frame(data)
plot(a254~km, data = data)

colors <- c("brown", "blue", "green")
colors <- colors[as.numeric(data$Water.mass)]
data$Water.mass <- as.factor(data$Water.mass)
plot(data$a254 ~ data$km, col = colors, data = data, pch = 16,
     ylab = "perte d'absorptivité (a254)",
     xlab = "Distance du lac Ontario (km)")
text(data$km, data$a254, labels=data$Stations, cex= 0.7, pos = 3)


colors <- c("brown", "blue", "green")
colors <- colors[as.numeric(data$Water.mass)]
data$Water.mass <- as.factor(data$Water.mass)
plot(data$DOC_P0 ~ data$km, col = colors, data = data, pch = 16,
     ylab = "PDOC après 72h",
     xlab = "Distance du lac Ontario (km)")
text(data$km, data$DOC_P0, labels=data$Stations, cex= 0.7, pos = 3)



boxplot(data$Loss_DOC_B7,
        data$Loss_DOC_B14,
        data$Loss_DOC_B28,
        data$Loss_DOC_B112)



rsd[,7] <- (data$Loss_DOC_B7)
