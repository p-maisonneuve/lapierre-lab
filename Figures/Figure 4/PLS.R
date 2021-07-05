#523x540
#Import the data
library(readxl)
data <- data.frame(read_excel("~/Documents/Labo Lapierre/Maîtrise/Data/2019/MSL2019_Data.xlsx"))
attach(data)
palette <- colorRampPalette(c('firebrick1','brown1','blue1')) # Create color palette 
col <- palette(50)[as.numeric(cut(dat$SPC,breaks = 50))]      # from conductivity values

loadings_X <- data.frame(read_excel("Documents/Labo Lapierre/Maîtrise/Rédaction/Figures/PLS/stdX_loadings.xlsx"))
colnames(loadings_X) <- c("Variables", "X1", "X2", "X3")
loadings_Y <- data.frame(read_excel("Documents/Labo Lapierre/Maîtrise/Rédaction/Figures/PLS/stdY_loadings.xlsx"))
colnames(loadings_Y) <- c("Variables", "Y1", "Y2", "Y3")
std_scores <- data.frame(read_excel("Documents/Labo Lapierre/Maîtrise/Rédaction/Figures/PLS/std_scores.xlsx"))

scores_X1 <- std_scores$X1_std_scores #Standardized scores for the first factor
scores_X2 <- std_scores$X2_std_scores #Standardized scores for the second factor
scores_X3 <- std_scores$X3_std_scores #Standardized scores for the third factor

attach(loadings_Y)
par(mar=c(5,5,4,1))
plot(scores_X2~scores_X1, ylim = c(-1,1), xlim = c(-1,1), pch=20, cex=1.4,
     # col=col, bg="grey", #Color sites according to conductivity values
     xlab = expression('Factor 1 (X-R'^2*' = 55.9%, Y-R'^2*' = 29.9%)'),
     ylab = expression('Factor 2 (X-R'^2*' = 14.4%, Y-R'^2*' = 9.5%)'),
     cex.lab= 1.1, yaxt = "n")
# text(scores_X1, scores_X2, labels = Stations, cex = 0.7) #Display sites names
axis(2, las=2)
abline(0,0, lty=4, col="darkgrey")
abline(v=0, lty=4, col="darkgrey")
points(Y1, Y2, pch=24, col="black", bg="white", cex=1.6)
points(Y1, Y2, pch=2, col="black", bg="white", cex=1.7)
text(Y1, Y2, Variables, pos=2, font=2) # Display names of Y variables
detach(loadings_Y)

attach(loadings_X)
for(i in 1:length(X1))
{
arrows(0,0,X1[i],X2[i], length = 0.1)
}
text(X1, X2, Variables, pos= 3) # Display names of X variables



