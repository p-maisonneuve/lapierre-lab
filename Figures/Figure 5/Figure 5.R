# Figure 5 - In situ photo vs bio ~ Distance from rivermouth — whole water column
# 601x317 px

library(readxl)

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)

palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(data$SPC,breaks = 50))]

# Plot
png(file="Fig8.png",width=600,height=335)
attach(mtcars)
par(mfrow=c(1,2))
par(mar=c(5,5,4,1))

plot(InSitu_losses_negatives_removed~km, 
     xlab = "Distance from rivermouth \n (km)", 
     ylab = expression('DOC processing (mg C m'^-2*' d'^-1*')'), 
     main = "Photochemical",
     pch = 16,
     col = col,
     axes = F)

legend('topleft', legend = "a", bty = 'n')
box(bty="l")
axis(2, las=1)
axis(1)
text(km, InSitu_losses_negatives_removed, labels = Stations)

plot(model_InSitu_Loss_DOC_B1~km, 
     xlab = "Distance from rivermouth \n (km)", 
     ylab = expression('DOC processing (mg C m'^-2*' d'^-1*')'),
     main = "Biological",
     pch = 16,
     col = col,
     axes = F)

legend('topleft', legend = "b", bty = 'n')
box(bty="l")
axis(2, las=1)
axis(1)

dev.off()
