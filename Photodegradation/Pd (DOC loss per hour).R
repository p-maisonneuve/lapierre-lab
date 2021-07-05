library(readxl)
data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))

output = matrix(0,ncol = 3, nrow = 40)
colnames(output) = c("Site","slope", "intercept")
attach(data)
# Calculate Pd from the slope
for(i in 1:40)
{

  sites.names <- data$Stations
  DOC_P <- (data.frame(rbind(DOC_P0, DOC_P24, DOC_P48, DOC_P72)))
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
mean(output$slope/data$Total_RU_B0, na.rm =T)
sd(output$slope/data$Total_RU_B0, na.rm =T)