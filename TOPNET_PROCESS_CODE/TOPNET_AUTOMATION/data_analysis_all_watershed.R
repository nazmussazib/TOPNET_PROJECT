##rainfall comparison###########
setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\Results_Analysis\\Rainfall_comparison\\A1_watershed')
daymet=t(matrix(scan('rain_daymet.dat',skip=3),nrow=64,ncol=12054))
daymet_rainfall=rowMeans(daymet[,c(-64,-63)])
time_all_dm= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_cali= seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all_dm,) # get overlap time interval
dm_rainfall=matrix(daymet_rainfall[date_overlap])




time_all_nd= seq(as.Date("2008/1/1"), as.Date("2012/12/31"), "day") #create time series
date_overlap=match(time_cali,time_all_nd,) # get overlap time interval
nldas=t(matrix(scan('rain_nldas.dat',skip=3),nrow=9,ncol=1827))
nldas_rainfall=rowMeans(nldas[,c(-9,-8)])
nd_rainfall=matrix(nldas_rainfall[date_overlap])



time_all_prism= seq(as.Date("2007/1/1"), as.Date("2012/12/31"), "day") #create time series
date_overlap=match(time_cali,time_all_prism,) # get overlap time interval
prism=t(matrix(scan('rain_prism.dat',skip=3),nrow=64,ncol=2192))
prism_rainfall=rowMeans(prism[,c(-64,-63)])
pr_rainfall=matrix(prism_rainfall[date_overlap])


par(mfrow=c(3,1))
plot(time_cali,dm_rainfall, type="o", col="blue",,xlab=" Time days",ylab="precipitation (mm/day)",main="DAYMET")
plot(time_cali,nd_rainfall, type="o", col="red",,xlab=" Time days",ylab="precipitation (mm/day)",main="NLDAS")
plot(time_cali,pr_rainfall, type="o", col="black",,xlab=" Time days",ylab="precipitation (mm/day)",main="PRISM")

par(mfrow=c(2,2))


plot(dm_rainfall,nd_rainfall,col='blue',xlab="DAYMET",ylab="NLDAS")
abline(fit <- lm(nd_rainfall ~ dm_rainfall-1), col='blue')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=4)))

plot(dm_rainfall,pr_rainfall,col='red',xlab="DAYMET",ylab="PRISM")
abline(fit <- lm(pr_rainfall ~ dm_rainfall-1), col='red')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=4)))

plot(nd_rainfall,pr_rainfall,col='black',xlab="NLDAS",ylab="PRISM")
abline(fit <- lm(pr_rainfall ~ nd_rainfall-1), col='black')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=4)))

#########scatter plot for stream flwo regime variables##############
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\Results_Analysis")
srg=t(matrix(scan('SG_PRESENT_ALL_WAS.txt'),nrow=16,ncol=16))
cali=seq(1,16,2)
obs=seq(2,16,2)



par(mfrow=c(2,2))

plot(srg[obs,1],srg[cali,1],col='blue',xlab="observed",ylab="calibrated",main="ZFE")
abline(fit <- lm(srg[cali,1] ~ srg[obs,1]-1), col='black')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,2],srg[cali,2],col='blue',xlab="observed",ylab="calibrated",main="SI")
abline(fit <- lm(srg[cali,2] ~ srg[obs,1]-2), col='black')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,3],srg[cali,3],col='blue',xlab="observed",ylab="calibrated",main="LFE")
abline(fit <- lm(srg[cali,3] ~ srg[obs,1]-3), col='black')
legend("topright", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,4],srg[cali,4],col='blue',xlab="observed",ylab="calibrated",main="Q7min")
abline(fit <- lm(srg[cali,4] ~ srg[obs,4]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))


par(mfrow=c(2,2))

plot(srg[obs,5],srg[cali,5],col='blue',xlab="observed",ylab="calibrated",main="Q7max")
abline(fit <- lm(srg[cali,5] ~ srg[obs,5]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))


plot(srg[obs,6],srg[cali,6],col='blue',xlab="observed",ylab="calibrated",main="Q167")
abline(fit <- lm(srg[cali,6] ~ srg[obs,6]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))


plot(srg[obs,7],srg[cali,7],col='blue',xlab="observed",ylab="calibrated",main="FLDDUR")
abline(fit <- lm(srg[cali,7] ~ srg[obs,7]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))


plot(srg[obs,8],srg[cali,8],col='blue',xlab="observed",ylab="calibrated",main="Peak time")
abline(fit <- lm(srg[cali,8] ~ srg[obs,8]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))



par(mfrow=c(2,2))

plot(srg[obs,9],srg[cali,9],col='blue',xlab="observed",ylab="calibrated",main="HFE")
abline(fit <- lm(srg[cali,9] ~ srg[obs,9]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,10],srg[cali,10],col='blue',xlab="observed",ylab="calibrated",main="Qmean")
abline(fit <- lm(srg[cali,10] ~ srg[obs,10]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,11],srg[cali,11],col='blue',xlab="observed",ylab="calibrated",main="COV")
abline(fit <- lm(srg[cali,11] ~ srg[obs,11]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,12],srg[cali,12],col='blue',xlab="observed",ylab="calibrated",main="P")
abline(fit <- lm(srg[cali,12] ~ srg[obs,12]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))



par(mfrow=c(2,2))

plot(srg[obs,13],srg[cali,13],col='blue',xlab="observed",ylab="calibrated",main="C")
abline(fit <- lm(srg[cali,13] ~ srg[obs,13]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,14],srg[cali,14],col='blue',xlab="observed",ylab="calibrated",main="M")
abline(fit <- lm(srg[cali,14] ~ srg[obs,14]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,15],srg[cali,15],col='blue',xlab="observed",ylab="calibrated",main="FR")
abline(fit <- lm(srg[cali,15] ~ srg[obs,15]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))

plot(srg[obs,16],srg[cali,16],col='blue',xlab="observed",ylab="calibrated",main="T50")
abline(fit <- lm(srg[cali,16] ~ srg[obs,16]-1), col='black')
legend("topleft", bty="n", legend=paste("R2 is",format(summary(fit)$adj.r.squared, digits=2)))



