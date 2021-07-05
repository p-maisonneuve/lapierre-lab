library(readr)

data <- Absorbance_MSL2019

data <- data.frame(data)

data$Sample <- as.factor(data$Sample)
s.list <- levels(data$Sample)


output = data.frame(x = 1, y = 1, z = 1)
colnames(output) <- c("Sample", "a254", "a440")

for(i in 1:nlevels(data$Sample))
{


  output[i,2] <- (data[which(data$Sample==paste(s.list[i]) & data$Time=='0'), 3] - data[which(data$Sample==paste(s.list[i]) & data$Time=='72'), 3])
  output[i,3] <- (data[which(data$Sample==paste(s.list[i]) & data$Time=='0'), 4] - data[which(data$Sample==paste(s.list[i]) & data$Time=='72'), 4])
  output[i,1] <- s.list[i]

  
}


write.csv2(x = output, file = "P0-P72_MSL2019.csv", fileEncoding = "UTF-8")


output$a254 <- scale(output$a254)
hist(output$a254)
