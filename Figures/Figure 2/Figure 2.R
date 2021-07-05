# Figure 2 

library(readxl)
data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)


# Figure 2a - [DOC] removal rate during light exposure
#png(file="Fig2.png",width=902,height=320)
attach(mtcars)
par(mfrow=c(1,3))
par(mar=c(5,5,4,1))

axis_size <- 1.2
cex_size <- 1.5

plot(Pd..mg.L.1.h.1.~SPC, ylab = expression('Pd (mg L'^-1*' h'^-1*')'), xlab = expression('Conductivity (μS cm'^-1*')'),
     pch = 20, axes = F, cex.lab=1.7, cex = cex_size)
#text(Pd~SPC, labels=data$Station, cex= 0.7, pos = 3)
box(bty="l")
axis(2, las=2, cex.axis = axis_size, las=0)
axis(1, cex.axis = axis_size)
lm1<-lm(Pd..mg.L.1.h.1.~SPC)
lm1_sum <- summary(lm1)
r2 <- lm1_sum$adj.r.squared
p <- lm1_sum$coefficients[2,4]
clip(28,303, -0.06026873, 0.43205540)

#abline(lm1)
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=2)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
#legend(10, 0.004, legend = rp, bty = 'n', cex = 1.5)
#legend('topleft', legend = "a", bty = 'n', cex = 2)



# Figure 2b - [DOC] removed in bio - short term 
par(mar=c(5,5,4,1))
plot(model_loss_B1~SPC, ylab = expression('ST-BDOC (mg L'^-1*')'), xlab = expression('Conductivity (μS cm'^-1*')'), 
     pch = 20, axes = F, cex.lab=1.7, cex = cex_size)
#text(Loss_DOC_B7~SPC, labels=data$Station, cex= 0.7, pos = 3)
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
#legend('topleft', legend = "b", bty = 'n', cex = 2)



# Figure 2c - [DOC] removed in bio - long term 
par(mar=c(5,5,4,1))
plot(model_loss_B112~SPC, ylab = expression('LT-BDOC (mg L'^-1*')'), xlab = expression('Conductivity (μS cm'^-1*')'), 
     pch = 20, axes = F, cex.lab=1.7, cex = cex_size)
#text(Loss_DOC_B112~SPC, labels=data$Station, cex= 0.7, pos = 3)
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
lm1<-lm(Loss_DOC_B112~SPC)
lm1_sum <- summary(lm1)
r2 <- lm1_sum$adj.r.squared
p <- lm1_sum$coefficients[2,4]
clip(28, 303, 0.285387, 1.588257)
#abline(lm1)
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=2)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
#legend(10,0.7, legend = rp, bty = 'n', cex=1.5)

#legend('topleft', legend = "c", bty = 'n', cex=2)

#dev.off()

