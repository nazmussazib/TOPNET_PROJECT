

##Run model

library(hydroGOF)
library(zoo)
library(plyr)
library(nsga2R)
require(hydroTSM)


##set working directory######

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_TOPNETRUN_CC") #for A1

##RUN MODEL FOR EACH RCP AND SAVED IT DIFFERENT DIRECTORY
##RCP26_65, RCP26_99 etc


system(paste("topnet_modified"))


####Do analysis only for the streamlow and runoff ########
##Baseline period analysis
bn=29
ff=scan("FlowAtStreamNodes_cms.txt", what="")
#l=basin_number+2
l=bn+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later


sf=scan("A1_calibratedflow.txt", what="")
sf1=sf[seq(l,length(sf),1)] ## need to change theis things later
obs=matrix(as.numeric(sf1[seq(2,length(sf1),l-1)])) ##need to change this later

date_base=seq(as.Date("2003/1/1"), as.Date("2012/12/31"), "day")

date_proj=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")
date_proj=seq(as.Date("2090/1/1"), as.Date("2099/12/31"), "day")
sf_base<- data.frame(monthlyfunction(obs, FUN=mean, na.rm=TRUE,dates=date_base))
sf_base1<- data.frame(monthlyfunction(simu_flow, FUN=mean, na.rm=TRUE,dates=date_proj))

par(mfrow=c(2,2))
plot(date_base,obs)
plot(date_proj,simu_flow,col='red',ylim=c(0,50))

plot(seq(1,12,1),sf_base,ylim=c(0,12))
lines(seq(1,12,1),sf_base1)

plot(seq(1,12,1),(mf_base-mf_base1)*100/mf_base)






###projected runoff change

dir=paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_results\\Climate change analysis\\",folder[i],sep="")
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_CC_data\\CanESM2")

stream_file=list.files(path ="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_CC_data\\MRI-CGCM3",pattern ="*.5.txt")

mf_proj=data.frame(matrix(NA,nrow=1,ncol=12))

for ( i in 1:length(stream_file)){
ff=scan(stream_file[i], what="")
#l=basin_number+2
l=bn+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
d1=strtoi(unlist(strsplit(stream_file[i],NULL))[23])
if(d1==5)date_proj=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")else 
date_proj=seq(as.Date("2090/1/1"), as.Date("2099/12/31"), "day")

mf_proj[i,]<- data.frame(monthlyfunction(simu_flow, FUN=mean, na.rm=TRUE,dates=date_proj))
}



###########change in stream flow regime variables############

par(mfrow=c(2,1))
a=seq(1,12,1)
plot(a,sf_base,ylim=c(0,8),main="year 56-65")
lines(a,mf_proj[1,],col='red')
lines(a,mf_proj[2,],col='blue')
legend("topright", c('base','rcp4.5','rcp8.5'), cex=0.8, col=c("black","red","blue"), pch=21:22, lty=1:2)
plot(a,sf_base,ylim=c(0,12),main='year 90-99')
lines(a,mf_proj[3,],col='grey')
lines(a,mf_proj[4,],col='green')
legend("topright", c('base','rcp4.5','rcp8.5'), cex=0.8, col=c("black","grey","green"), pch=21:22, lty=1:2)

plot(a,mf_base)
lines(a,mf_proj[2,],col='red')
lines(a,mf_proj[4,],col='blue')
lines(a,mf_proj[6,],col='grey')
lines(a,mf_proj[8,],col='green')
legend("topleft", c('base','rcp2.6','rcp4.5','rcp6.0','rcp8.5'), cex=0.8, col=c("black","red",'blue','grey','green'), pch=21:22, lty=1:2)


pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=131+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later
plot(cumsum(simu_rain))


ev=scan("Evaporation_mm.txt", what="")
#l=basin_number+2
l=113+2#for A2
ev1=ev[seq(l,length(ev),1)] ## need to change theis things later
simu_eva=matrix(as.numeric(ev1[seq(2,length(ev1),l-1)])) ##need to change this later
plot(cumsum(simu_eva))
lines(cumsum(simu_rain))





time_fore= seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day") # create time series based on start and end date
plot(time_fore,simu_flow, type="o", col="blue",,xlab=" Time(days)",ylab="stream flow (m3/s)",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)




lines(time_c,cv_flow$simu, type="o", pch=22, lty=2, col="red")
legend('topright', max(calibrated_flow$observ), c("observed flow","simulated flow"), bty="n",cex=1.2, col=c("blue","red"), pch=21:22, lty=1:2,pt.cex=1.2);
CE_c=NSE(cv_flow$simu[1:1096],cv_flow$observ[1:1096])
Bias_c=sum(cv_flow$simu[1:1096])/sum(cv_flow$observ[1:1096]) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error_c=rmse(cv_flow$simu[1:1096],cv_flow$observ[1:1096], na.rm=TRUE)
text(14250, 5, paste("Bias=",format(Bias_c,digit=2)),cex = 1.25)
text(14250, 4.6, paste("NSE=",format(CE_c,digit=2)),cex = 1.25)
text(14250, 4.2, paste("MSE=",format(Mean_error_c,digit=2)),cex =1.25)




##compare runoff with base line period 2003-2012############






###cumulative rainfall and run off ####

pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=29+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later



plot(time_c,cumsum(0.2*cv_flow$observ[1:1096]),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$simu[1:1096]),col='red')

plot(time_c,cumsum(simu_rain[732:1827]),ylab="cumulative rainfall/flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$observ[1:1096]),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$simu[1:1096]),col='red')
legend(14650, 800, c("rainfall","observed flow","simulated flow"), cex=0.8, col=c("black","blue","red"), pch=21, lty=1);






##validation plot###



plot(time_cv,cumsum(0.2*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_cv,cumsum(0.2*cv_flow$simu),col='red')

pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=29+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later



plot(time_cv,cumsum(simu_rain),ylab="cumulative rainfall/flow (mm)")
lines(time_cv,cumsum(0.2*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_cv,cumsum(0.2*cv_flow$simu),col='red')
legend(14650, 800, c("rainfall","observed flow","simulated flow"), cex=0.8, col=c("black","blue","red"), pch=21, lty=1);






#lines(simu_rain, type="o", col="red",ylim=rev(range(simu_rain)))
#plot(calibrated_flow$simu, type="o", pch=22, lty=2, col="red",ylim=(range(calibrated_flow$simu)))









par(mar=c(2, 4, 0, 6)+0.25)
plot(time, calibrated_flow$simu, axes=T, ylim=c(0,50), xlab="", ylab="",type="l",lty=1,col="red", main="")
points(time, calibrated_flow$simu,pch=20,col="red")
axis(2, ylim=c(0,30),col="red",lwd=2)
mtext(2,text="Stream flow(m3/s)",line=2)

par(new=T)
plot(time, calibrated_flow$observ, axes=T, ylim=c(0,50), xlab="", ylab="",type="l",lty=2,col="blue", main="")
points(time, calibrated_flow$observ,pch=20,col="blue")
axis(2, ylim=c(0,600),col="blue",lwd=2)
#mtext(2,text="NSE",line=2)

par(new=T)
plot(time, simu_rain[732:1827,], axes=F, ylim=rev(c(0,100)), col="darkgrey",xlab="", ylab="", type="l",lty=3, main="",lwd=2)
axis(4, ylim=rev(range(simu_rain[732:1827,])),col="darkgrey",lwd=1,line=1.5)
#points(time, simu_rain[732:1827,],pch=20)
mtext(4,text="Rainfall (mm/day)",line=-.1)


#axis(1,pretty(range(time),4))
mtext(" tr factor ",side=1,col="black",line=2) ## change title 
legend(15270,40,legend=c("Simulate flow","Observed flow","Precipitation "),lty=c(1,2,3),col=c("red","blue","darkgrey")) ## change legend location





###overall water balance######


sim_cum=cumsum(calibrated_flow$simu)
obs_cum=cumsum(calibrated_flow$observ)
sim_precp=cumsum(simu_rain[732:1827,])
plot(time,sim_precp, type="o", col="red",ylim=range(sim_precp),lty=1,xlab="time(days)",ylab="cumulative precipitation(mm/day)")

plot(time,obs_cum, type="o", col="blue",ylim=range(obs_cum),lty=1,xlab="time(days)",ylab="cumulative Streamflow(m3/s)")

# Graph trucks with red dashed line and square points
lines(time,sim_cum, type="o", pch=22, lty=2, col="red")
legend(15270,400,legend=c("Observed flow","Simulatedflow"),lty=c(1,2),col=c("blue","red"))


########cheking whether water is going###########
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final_DAYMET\\matlab_cali")
system(paste("topnet_modified"))
ff=scan("FlowAtStreamNodes_cms.txt", what="")
#l=basin_number+2
l=29+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later

sf=scan("streamflow_calibration.dat", what="")
sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later

time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_cali= seq(as.Date("2008/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
calibrated_flow=data.frame(simu=simu_flow[732:1827,],observ=observd_flow[732:1827,]) ##take only from 2010/01/01---2012/12/31
time= seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
plot(time,calibrated_flow$observ, type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")
lines(time,calibrated_flow$simu, type="o", pch=22, lty=2, col="red")
legend(14650, max(calibrated_flow$observ), c("observed flow","simulated flow"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);
CE=NSE(calibrated_flow$simu,calibrated_flow$observ)
Bias=sum(calibrated_flow$simu)/sum(calibrated_flow$observ) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error=rmse(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE)

pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=29+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later
#lines(simu_rain, type="o", col="red",ylim=rev(range(simu_rain)))
#plot(calibrated_flow$simu, type="o", pch=22, lty=2, col="red",ylim=(range(calibrated_flow$simu)))


cs=scan("Canopy_storage_mm.txt", what="") ##canopy storgae
#l=basin_number+2
l=29+2#for A2
cs1=cs[seq(l,length(cs),1)] ## need to change theis things later
simu_canopy=matrix(as.numeric(cs1[seq(2,length(cs1),l-1)])) ##need to change this later


dw=scan("Depth_to_Water_mm.txt", what="") ##depth to water
#l=basin_number+2
l=29+2#for A2
dw1=dw[seq(l,length(dw),1)] ## need to change theis things later
simu_dw=matrix(as.numeric(dw1[seq(2,length(dw1),l-1)])) ##need to change this later

ss=scan("Soil_storage_mm.txt", what="") ##depth to water
#l=basin_number+2
l=29+2#for A2
ss1=ss[seq(l,length(ss),1)] ## need to change theis things later
simu_ss=matrix(as.numeric(ss1[seq(2,length(ss1),l-1)])) ##need to change this later

ep=scan("Evaporation_mm.txt", what="") ##depth to water
#l=basin_number+2
l=29+2#for A2
ep1=ep[seq(l,length(ep),1)] ## need to change theis things later
simu_ep=matrix(as.numeric(ep1[seq(2,length(ep1),l-1)])) ##need to change this later

pe=scan("Potential_evapotranspiration_mm.txt", what="") ##depth to water
#l=basin_number+2
l=29+2#for A2
pe1=pe[seq(l,length(pe),1)] ## need to change theis things later
simu_pe=matrix(as.numeric(pe1[seq(2,length(pe1),l-1)])) ##need to change this later

tav=scan("TemperatureAve_C.txt", what="") ##depth to water
#l=basin_number+2
l=29+2#for A2
tav1=tav[seq(l,length(tav),1)] ## need to change theis things later
simu_tav=matrix(as.numeric(tav1[seq(2,length(tav1),l-1)])) ##need to change this later

df=matrix(simu_tav[732:1091],nrow=30,ncol=12)
plot(df)


par(mfrow=c(2,2))
plot(time[366:731],cumsum(simu_rain[1097:1462]))
lines(time[366:731],cumsum(simu_ep[1097:1462]),col='red')
wff=cumsum(simu_ep[732:1097])

plot(time[1:366],cumsum(simu_rain[732:1097]))
lines(time[1:366],cumsum(0.2*simu_flow[732:1097]),col='red')
lines(time[1:366],cumsum(0.2*observd_flow[732:1097]),col='blue')

plot(time[366:731],cumsum(simu_rain[1097:1462]))
lines(time[366:731],cumsum(0.2*simu_flow[1097:1462]),col='red')
lines(time[366:731],cumsum(0.2*observd_flow[1097:1462]),col='blue')



plot(simu_flow[732:1097]-observd_flow[732:1097])
plot(time[1:366],cumsum(simu_pe[732:1097]))
lines(time[1:366],cumsum(simu_rain[732:1097]),col='green')


plot(time[1:366],simu_tav[732:1097])
lines(time[1:366],cumsum(simu_rain[732:1097]),col='green')


df=simu_flow[732:1097]-observd_flow[732:1097]
df[df<0]=0

plot(time[1:366],cumsum(0.2*observd_flow[732:1097]),col='blue')

lines(time[1:366],cumsum(0.2*simu_flow[732:1097]),col='black')
lines(simu_flow[732:1097])
plot(observd_flow[732:1097])
plot(time[1:366],df,col='red')



lines(cumsum(his_rain_sim))
lines(cumsum(qmr1),col='red')
lines(cumsum(his_corr),col='green')







par(mar=c(2, 4, 0, 6)+0.25)
plot(time, calibrated_flow$simu, axes=T, ylim=c(0,50), xlab="", ylab="",type="l",lty=1,col="red", main="")
points(time, calibrated_flow$simu,pch=20,col="red")
axis(2, ylim=c(0,30),col="red",lwd=2)
mtext(2,text="Stream flow(m3/s)",line=2)

par(new=T)
plot(time, calibrated_flow$observ, axes=T, ylim=c(0,50), xlab="", ylab="",type="l",lty=2,col="blue", main="")
points(time, calibrated_flow$observ,pch=20,col="blue")
axis(2, ylim=c(0,600),col="blue",lwd=2)
#mtext(2,text="NSE",line=2)

par(new=T)
plot(time, simu_rain[732:1827,], axes=F, ylim=rev(c(0,100)), col="darkgrey",xlab="", ylab="", type="l",lty=3, main="",lwd=2)
axis(4, ylim=rev(range(simu_rain[732:1827,])),col="darkgrey",lwd=1,line=1.5)
#points(time, simu_rain[732:1827,],pch=20)
mtext(4,text="Rainfall (mm/day)",line=-.1)


#axis(1,pretty(range(time),4))
mtext(" tr factor ",side=1,col="black",line=2) ## change title 
legend(15270,40,legend=c("Simulate flow","Observed flow","Precipitation "),lty=c(1,2,3),col=c("red","blue","darkgrey")) ## change legend location





                      ###overall water balance######


sim_cum=cumsum(calibrated_flow$simu)
obs_cum=cumsum(calibrated_flow$observ)
sim_precp=cumsum(simu_rain[732:1827,])
plot(time,sim_precp, type="o", col="red",ylim=range(sim_precp),lty=1,xlab="time(days)",ylab="cumulative precipitation(mm/day)")

plot(time,obs_cum, type="o", col="blue",ylim=range(obs_cum),lty=1,xlab="time(days)",ylab="cumulative Streamflow(m3/s)")

# Graph trucks with red dashed line and square points
lines(time,sim_cum, type="o", pch=22, lty=2, col="red")
legend(15270,400,legend=c("Observed flow","Simulatedflow"),lty=c(1,2),col=c("blue","red"))
















