######climate change result analysis######

dir="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\Results_Analysis"
setwd(dir)
regime_files =list.files(path=dir,pattern="*.txt")
sg=read.table("A1_climate_change_regime.txt",skip=1)
dss=data.frame(var=nrow(sg))

for ( m in 1:length(regime_files)){
  hjk=read.table(regime_files[m],skip=1)
  dss=cbind(dss,hjk)
}

dss=data.matrix(dss[,-1])


obs=(dss[1,]);
obs=matrix(obs,17,8);
obs=obs[-1,];
daymet=(dss[2,]);
daymet=matrix(daymet,17,8);daymet=daymet[-1,]

model=matrix(colMeans(dss[3:7,]),nrow=17,ncol=8)
model=model[-1,]

CE=matrix(NA,nrow=3,ncol=16)
for ( i in 1:16){
  
  CE[1,i]=NSE(daymet[i,],obs[i,])
  CE[2,i]=NSE(model[i,],daymet[i,])
  CE[3,i]=NSE(model[i,],obs[i,])
}


#########scatter plot to see how it looks like plot#########


col_name=c("BFI","Slp_BFI","Pval_BFI",
           "CoV_whole",
           "CoV_yr","Slp_CoV","Pval_CoV",
           "Qmean_whole",
           "Qmean_yr","Slp_Qmean","Pval_Qmean",
           "Zero","Slp_Zero","Pval_Zero",
           "BFF_Ln",
           "FD_Ln","Slp_FD_Ln","Pval_FD_Ln",
           "BFF_Lp3",
           "FD_Lp3","Slp_FD_Lp3","Pval_FD_Lp3",
           "P","C","M",
           "T25","Slp_T25","Pval_T25",
           "T50","Slp_T50","Pval_T50",
           "T75","Slp_T75","Pval_T75",
           "Pk_time","H1",
           "Q7min","Slp_Q7min","Pval_Q7min",
           "Q7max","Slp_Q7max","Pval_Q7max",
           "FR","Slp_FR","Pval_FR",
           "SI","Slp_SI","Pval_SI",
           "HFE","Slp_HFE","Pval_HFE",
           "LFE","Slp_LFE","Pval_LFE",
           "ZFE","Slp_ZFE","Pval_ZFE",
           "Stn_No")
col_names=matrix(col_name)
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
names=col_names[var_order]

rg_names=data.frame(name=matrix(rep(names,8)))
obs_data=data.frame(obs=matrix(obs,128,1))
daymet_data=data.frame(daymet=matrix(daymet,128,1))
model_data=data.frame(model=matrix(model,128,1))
DATA=cbind(obs_data,daymet_data,model_data,rg_names)

d=ggplot(DATA, aes(x=obs, y=model)) +
  geom_point(shape=1)
d + facet_wrap(~ name,scales = "free")










#####plot daymet vs obs
op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(daymet[1,],obs[1,])

###see how changes occur#########

change2646=((mod2646/model)-1)*100
change2676=((mod2676/model)-1)*100
change8546=((mod8546/model)-1)*100
change8576=((mod8576/model)-1)*100


###for A1 and A2 watershed

m=1
op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(change2646[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2646[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+1],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))



###for B1,B21 and B22 watershed

m=6
op <- par(mfrow = c(3,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(change2646[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2646[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+1],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))




plot(change2646[,m+2],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+2],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+2],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+2],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


m=2
plot(change2646[m,],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-50,100))
lines(change8546[m,],col='grey',lty=1,lwd=3,type='o')
lines(change2676[m,],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-50,100))
lines(change8576[m,],col='grey',lty=2,lwd=3,type='o')
lines(seq(-5,16,1),rep(0,22),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=1,lwd=3,type='o')
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)





title(xlab="Month", col.lab=rgb(0,0,0))
title(ylab="Monthly Rainfall (mm)", col.lab=rgb(0,0,0))



plot(df$Qmean_whole[rv_56_4.5],main="Qmean",type="o",pch=21,ylim=c(-70,70),col='red',lty=1,xlab=" ",ylab="%change",axes=FALSE)
lines(df$Qmean_whole[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$Qmean_whole[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3, ylim=c(-70,70),col="red")
lines(df$Qmean_whole[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1,at=seq(1,4,1),x[1:4],labels=FALSE) #), at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)
