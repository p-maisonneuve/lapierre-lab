output = matrix(0,ncol = 3, nrow = 40)
colnames(output) = c("Site","slope", "intercept")

#Slope photo
for(i in 1:40)
{
  sites.names <- data$Stations
  DOC_P <- (data.frame(rbind(C4_P0, C4_P24, C4_P48, C4_P72)))
  time <- rbind(0,24,48,72)
  
  lm1<-lm(DOC_P[,i]~time)
  plot(DOC_P[,i]~time, main = paste0("SUVA ", sites.names[i]), ylim = c(2,4))
  abline(lm1)
  legend("topright", legend = paste("R^2 =",
                                    round(summary(lm1)$adj.r.squared, digits = 2),
                                    "; p = ",
                                    round(summary(lm1)$coefficients[,"Pr(>|t|)"][2], digits = 4)))
  output[i,1]<- sites.names[i]
  output[i,2]<- lm1$coefficients[2]
  output[i,3]<- lm1$coefficients[1]
  
  summary(lm1)
}

output<-data.frame(output)
output$slope<-as.numeric(as.character(output$slope))
output$slope <- output$slope*-24
mean(output$slope/data$C4_P0, na.rm =T)
sd(output$slope/data$C4_P0, na.rm =T)

#Slope bio
for(i in 1:40)
{
  sites.names <- data$Stations
  DOC_P <- (data.frame(rbind(C5_B0, C5_B7, C5_B14, C5_B28, C5_B112)))
  time <- rbind(0,7,14,28,112)
  
  lm1<-lm(DOC_P[,i]~time)
  plot(DOC_P[,i]~time, main = paste0("SUVA ", sites.names[i]), ylim = c(0,4))
  abline(lm1)
  legend("topright", legend = paste("R^2 =",
                                    round(summary(lm1)$adj.r.squared, digits = 2),
                                    "; p = ",
                                    round(summary(lm1)$coefficients[,"Pr(>|t|)"][2], digits = 4)))
  output[i,1]<- sites.names[i]
  output[i,2]<- lm1$coefficients[2]
  output[i,3]<- lm1$coefficients[1]
  
  summary(lm1)
}
output<-data.frame(output)
output$slope<-as.numeric(as.character(output$slope))
output$slope <- output$slope*-1
mean(output$slope/data$C5_B0, na.rm =T)
sd(output$slope/data$C5_B0, na.rm =T)
