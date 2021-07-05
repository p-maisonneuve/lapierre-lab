# Figure S2 - DOC concentration ~ Distance from rivermouth
# 600x344 px

library(readxl)

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)

palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(data$SPC,breaks = 50))]

# Plot
attach(mtcars)
par(mar=c(5,5,4,1))

plot(DOC_B0~km, 
     xlab = "Distance from headwaters (km)", 
     ylab = expression('DOC (mg L'^-1*')'),
     pch = 21,
     bg = col,
     axes = F)

box(bty="l")
axis(2, las=1)
axis(1)

