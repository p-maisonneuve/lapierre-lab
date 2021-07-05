# Figure 1 789x422
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(beeswarm)
library(readxl)
library(grDevices)

data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)
percent_humic <- 100*(C1_B0+C2_B0+C3_B0)/Total_RU_B0
percent_proteinic <- 100*(C4_B0+C5_B0)/Total_RU_B0


#L&O paper colors
palette <- colorRampPalette(c('firebrick1','brown1','blue1'))
col <- palette(50)[as.numeric(cut(dat$SPC,breaks = 50))]

#Seminar colors
#palette <- colorRampPalette(c('#A36B2A','#909853','#7DC47C'))
#col <- palette(50)[as.numeric(cut(dat$SPC,breaks = 50))]


# Plot
attach(mtcars)
par(mfrow=c(2,4))
par(mar=c(5,3,1,1))
space <- 0.6
lab_size <- 1.75 # size of horizontal axis title
axis_size <- 1.2 # size of vertical axis labels
line_size <- 1.2 # space between axis and axis title
cex_size <- 1.3 #point size

boxplot(DOC_B0, las = 2, axes = F)
title(xlab = expression('DOC (mg L'^-1*')'), line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(DOC_B0, add=T, pch = 21, col = "white", pwbg = col, xlab="DOC", spacing = space, cex= cex_size)

boxplot(pH, las = 2, axes = F)
title(xlab="pH", line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(pH, add=T, pch = 21, col = "white", pwbg = col, xlab="pH", spacing = space, cex= cex_size)

boxplot(TN, las = 2, axes = F)
title(xlab = expression('TN (μg L'^-1*')'), line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(TN, add=T, pch = 21, col = "white", pwbg = col, xlab="TN", spacing = space, cex= cex_size)

boxplot(TP, las = 2, axes = F)
title(xlab = expression('TP (μg L'^-1*')'), line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(TP, add=T, pch = 21, col = "white", pwbg = col, xlab="TP", spacing = space, cex= cex_size)

boxplot(Kd_443, las = 2, axes = F)
title(xlab= expression('K'[d]*' (m'^-1*'; 443 nm)'), line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(Kd_443, add=T, pch = 21, col = "white", pwbg = col, spacing = space, cex= cex_size)

boxplot(percent_humic, las = 2, axes = F)
title(xlab="% humic-like", line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(percent_humic, add=T, pch = 21, col = "white", pwbg = col, xlab="% humic-like", spacing = space, cex= cex_size)

boxplot(percent_proteinic, las = 2, axes = F)
title(xlab="% protein-like", line = line_size, cex.lab = lab_size)
box(bty="l")
axis(2, las=2, cex.axis = axis_size)
beeswarm(percent_proteinic, add=T, pch = 21, col = "white", pwbg = col, xlab="% protein-like", spacing = space, cex= cex_size)


#Legend on last panel
plot(NULL, xlab = "",xaxt='n',yaxt='n',bty='n',ylab='', xlim=0:1, ylim=0:1)
title(xlab= expression('Conductivity (μS cm'^-1*')'), line = line_size, cex.lab = 1.6)
lgd_ = rep(NA, 11)
lgd_[c(1,6,11)] = c(28,166,303)
legend(x = 0.25, y = 1.02,
       legend = lgd_,
       fill = colorRampPalette(colors = c('firebrick1','brown1','blue1'))(11),
       border = NA,
       x.intersp = 0.5,
       y.intersp = 0.5,
       cex = 1.5, text.font = 1.2)

