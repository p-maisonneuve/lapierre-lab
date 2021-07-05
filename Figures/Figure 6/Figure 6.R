# Figure 6 - Photochemical DOC removal ~ Bacterial DOC removal — Whole water column vs first meter
# 601x317 px


library(readxl)

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)

SPC_STM1rem <- SPC[-31]
palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(data$SPC,breaks = 50))]

# Plot
#png(file="Fig5.png",width=601,height=317)
attach(mtcars)
par(mfrow=c(1,2))
par(mar=c(5,5,4,1))

plot(InSitu_losses_negatives_removed~model_InSitu_Loss_DOC_B1, 
     xlab = expression('BDOC (mg C m'^-2*' d'^-1*')'), 
     ylab = expression('PDOC (mg C m'^-2*' d'^-1*')'), 
     font.main = 1,
     cex.main = 1,
     main = "Whole water column",
     pch = 21,
     bg = col,
     axes = F)

box(bty="l")
axis(2, las=2)
axis(1)
abline(0,0.25)
#text(200,27.5, labels="1:4", cex= 1, pos = 3)
abline(0,0.1)
#text(400,27, labels="1:10", cex= 1, pos = 3)
abline(0,0.01)
#text(825,8, labels="1:100", cex= 1, pos = 3)

plot(InSitu_losses_negatives_removed_1m~model_InSitu_Loss_DOC_B1_1m, 
     xlab = expression('BDOC (mg C m'^-2*' d'^-1*')'), 
     ylab = expression('PDOC (mg C m'^-2*' d'^-1*')'), 
     font.main = 1,
     main = "First meter",
     cex.main = 1,
     pch = 21,
     bg = col,
     axes = F)

box(bty="l")
axis(2, las=2)
axis(1)
abline(0,1)
#text(17.8,29.8, labels="1:1", cex= 1, pos = 3)
abline(0,0.25)
#text(100,27.5, labels="1:4", cex= 1, pos = 3)
abline(0,0.1)
#text(220,23, labels="1:10", cex= 1, pos = 3)
abline(0,0.01)
#text(240,2, labels="1:100", cex= 1, pos = 3)

#dev.off()

