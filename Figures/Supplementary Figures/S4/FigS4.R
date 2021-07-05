# Figure 7
# 902x318px

library(readxl)
data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)


# Figure Xa - Areal in situ daily loss ~ Pd/J 
png(file="Fig2.png",width=902,height=318)
attach(mtcars)
par(mfrow=c(1,3))
par(mar=c(5,5,4,1))

axis_size <- 1.6

attach(data)
plot(InSitu_losses_negatives_removed~Pd.par.joule, ylab = expression('In situ PDOC (mg C m'^-2*' d'^-1*')'), xlab = expression('Pd-E'[w]*' (mg C J'^-1*'m'^-2*')'), 
     pch = 20, axes = F, cex.lab=1.7)
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
lm1<-lm(InSitu_losses_negatives_removed~Pd.par.joule)
lm1_sum <- summary(lm1)
r2 <- lm1_sum$adj.r.squared
p <- lm1_sum$coefficients[2,4]
range(Pd.par.joule)
range(InSitu_losses_negatives_removed)
clip(-1.237709e-05,2.169004e-05, 0, 31.49955)
abline(lm1)
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
legend(0.3e-05, 8, legend = rp, bty = 'n', cex = 1.5)
legend('topleft', legend = "a", bty = 'n', cex = 2)



# Figure Xb - Areal in situ daily loss ~ daily BDOC
par(mar=c(5,5,4,1))
plot(model_InSitu_Loss_DOC_B1~model_loss_B1, ylab = expression('In situ BDOC (mg C m'^-2*' d'^-1*')'), xlab = expression('BDOC after 1 day (mg L'^-1*')'), 
     pch = 20, axes = F, cex.lab=1.7)
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
lm1<-lm(model_InSitu_Loss_DOC_B1~model_loss_B1)
lm1_sum <- summary(lm1)
r2 <- lm1_sum$adj.r.squared
p <- lm1_sum$coefficients[2,4]
range(model_loss_B1)
range(model_InSitu_Loss_DOC_B1)
clip(0.00952009,0.25806901, 36.7154, 892.1684)
abline(lm1)
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
legend(0.12, 260, legend = rp, bty = 'n', cex = 1.5)
legend('topleft', legend = "b", bty = 'n', cex = 2)


# Figure 2c - [DOC] removed in bio - long term 
par(mar=c(5,5,4,1))
plot(model_InSitu_Loss_DOC_B112~MaxDepth_PUV_corrected, xlab = expression('Depth (m)'), ylab = expression('In situ BDOC (mg C m'^-2*')'),
     pch = 20, axes = F, cex.lab=1.7)
box(bty="l")
axis(2, las=0, cex.axis = axis_size)
axis(1, cex.axis = axis_size)
lm1<-lm(model_InSitu_Loss_DOC_B112~MaxDepth_PUV_corrected)
lm1_sum <- summary(lm1)
r2 <- lm1_sum$adj.r.squared
p <- lm1_sum$coefficients[2,4]
range(MaxDepth_PUV_corrected)
range(model_InSitu_Loss_DOC_B112)
clip(1.302, 11.562, 821.4084, 11291.4336)
abline(lm1)
rp = vector('expression',2)

rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
legend(6, 3500, legend = rp, bty = 'n', cex = 1.5)

legend('topleft', legend = "c", bty = 'n', cex=2)

dev.off()

