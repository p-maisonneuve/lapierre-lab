# Figure 3 - Average losses of total fluorescence (FDOM) and PARAFAC components (C1-C5) 
#            during light exposure compared to average losses during incubations
# 736x420 px

library(readxl)
data <- read_excel("Documents/Labo Lapierre/Maîtrise/Rédaction/Figures/Figure 3/Figure 3.xlsx")
attach(data)


# Plot
png(file="Fig3.png",width=600,height=335)
attach(mtcars)
par(mfrow=c(1,2))
par(mar=c(5,5,4,1))

axis_size <- 1
cex_lab <- 1.3

y <- data$`avg bio loss (RU/d)`
x <- data$`avg photo loss (RU/d)`
y_SEM <- data$`SEM bio (RU/d)`
x_SEM <- data$`SEM photo (RU/d)`

plot(y~x, 
     xlab = expression('Photochemical loss (RU d'^-1*')'), 
     ylab = expression('Biological loss (RU d'^-1*')'), 
     pch = 19,
     axes = F,
     cex.lab=cex_lab,
     ylim=c(-0.0002,0.0016),
     xlim=c(0,0.30))

#legend('topleft', legend = "a", bty = 'n')
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
arrows(x0=x, y0=y-y_SEM, x1=x, y1=y+y_SEM, code=3, angle=90, length=0.03)
arrows(x0=x-x_SEM, y0=y, x1=x+x_SEM, y1=y, code=3, angle=90, length=0.03)
#text(y~x, labels=data$Labels, cex= 0.7, pos = 3)

y <- data$`avg bio loss (%/d)`
x <- data$`avg photo loss (%/d)`
y_SEM <- data$`SEM bio (%/d)`
x_SEM <- data$`SEM photo (%/d)`

plot(data$`avg bio loss (%/d)`~data$`avg photo loss (%/d)`, 
     xlab = expression('Photochemical loss (% d'^-1*')'), 
     ylab = expression('Biological loss (% d'^-1*')'), 
     pch = 19,
     axes = F,
     cex.lab=cex_lab,
     ylim=c(-0.12,0.40),
     xlim=c(8,22))
#text(y~x, labels=data$Labels, cex= 0.7, pos = 3)

#legend('topleft', legend = "b", bty = 'n')
box(bty="l")
axis(2, las=0)
axis(1)
arrows(x0=x, y0=y-y_SEM, x1=x, y1=y+y_SEM, code=3, angle=90, length=0.03)
arrows(x0=x-x_SEM, y0=y, x1=x+x_SEM, y1=y, code=3, angle=90, length=0.03)


dev.off()
