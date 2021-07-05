# Figure 6 - Total processing ~ Distance from rivermouth — Whole water column vs first meter
#601x317
library(readxl)

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)

palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(data$SPC,breaks = 50))]
total_processing_1m <- (InSitu_losses_negatives_removed_1m + model_InSitu_Loss_DOC_B1_1m)
total_processing <- (InSitu_losses_negatives_removed + model_InSitu_Loss_DOC_B1)

# Plot
png(file="Fig6.png",width=600,height=335)
attach(mtcars)
par(mfrow=c(1,2))
par(mar=c(5,5,4,1))

plot(total_processing~km, 
     xlab = "\n", 
     ylab = expression('Total degradation (mg C m'^-2*' d'^-1*')'), 
     #main = "Whole water column",
     pch = 21,
     bg = col,
     axes = F)

legend('topleft', legend = "a", bty = 'n')
box(bty="l")
axis(2, las=1)
axis(1)
abline(v = 310,                # Add vertical line
       col = "dark grey",            # Modify color
       lty = "dashed",         # Modify line type
       lwd = 2)    


plot(total_processing_1m~km, 
     xlab = "Distance from rivermouth \n (km)", 
     ylab = expression('Total DOC processing (mg C m'^-2*' d'^-1*')'), 
     main = "First meter of \n the water column",
     pch = 16,
     col = col,
     axes = F)

legend('topleft', legend = "b", bty = 'n')
box(bty="l")
axis(2, las=1)
axis(1)

dev.off()

