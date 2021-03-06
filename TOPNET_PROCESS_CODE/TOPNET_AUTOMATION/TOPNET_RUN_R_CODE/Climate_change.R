

##Run model

library(hydroGOF)
library(zoo)
library(plyr)
library(nsga2R)
#add one line in  stream flow calibration.dat file 

##trying to do some calibration:


setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final_DAYMET\\matlab_cali")
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_TOPNETRUN")
#trying to do some calibration:


#library(rgenoud)
#xe1=(c(0.01,0.1,0.5,0.6,1,1,0.01))
#xe2=(c(500,1000,1,1,500,100,50))
#xd=cbind(xe1,xe2)
#claw1 <- genoud(calibration, nvars=7,pop.size=100,Domains=xd,max=TRUE)

# f_parm=x[1]
# k_parm=x[2]
# dth1_parm=x[3]
# dth2_parm=x[4]
# chv_parm=x[5]
# cc_parm=x[6]
# cr_parm=x[7]
# n_parm=x[8]
# T_parm=x[9]
library(hydroPSO)
xe1=(c(0.2,1,0.5,0.5,0.75,0.1,0.75,1))
xe2=(c(25,500,1,2,3,10,10,15))
hydroPSO(fn=calibration,lower=xe1,upper=xe2,control=list(MinMax='max'))

####multi objective algorithm#########
pbject=nsga2R(calibration, 8, 2, lowerBounds =xe1, upperBounds = xe2,
popSize = 60, tourSize = 2, generations = 20, cprob = 0.7, XoverDistIdx = 5,
mprob = 0.2, MuDistIdx = 10)

########statistices########
##After calibration , need to do some statitical analysis of the results

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
time_cv= seq(as.Date("2008/1/1"), as.Date("2010/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cv,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
cv_flow=data.frame(simu=simu_flow[731:1826,],observ=observd_flow) ##take only from 2010/01/01---2012/12/31

##calibration plot###

time_c= seq(as.Date("2008/1/1"), as.Date("2010/12/31"), "day") # create time series based on start and end date
plot(time_c,cv_flow$observ, type="o", col="blue",,xlab=" Time(days)",ylab="stream flow (m3/s)",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)
lines(time_c,cv_flow$simu, type="o", pch=22, lty=2, col="red")
legend('topright', max(calibrated_flow$observ), c("observed flow","simulated flow"), bty="n",cex=1.2, col=c("blue","red"), pch=21:22, lty=1:2,pt.cex=1.2);
CE_c=NSE(cv_flow$simu[1:1096],cv_flow$observ[1:1096])
Bias_c=sum(cv_flow$simu[1:1096])/sum(cv_flow$observ[1:1096]) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error_c=rmse(cv_flow$simu[1:1096],cv_flow$observ[1:1096], na.rm=TRUE)
text(14250, 5, paste("Bias=",format(Bias_c,digit=2)),cex = 1.25)
text(14250, 4.6, paste("NSE=",format(CE_c,digit=2)),cex = 1.25)
text(14250, 4.2, paste("MSE=",format(Mean_error_c,digit=2)),cex =1.25)

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

##validation plot###

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
time_cv= seq(as.Date("2011/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cv,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
cv_flow=data.frame(simu=simu_flow,observ=observd_flow) ##take only from 2010/01/01---2012/12/31


plot(time_cv,cv_flow$observ, type="o", col="blue",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)#,,xlab=" Time days",ylab="stream flow (m3/s)")
lines(time_cv,cv_flow$simu, type="o", pch=22, lty=2, col="red")
#legend(14650, max(cv_flow$observ), c("observed flow","simulated flow"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);
CE_v=NSE(cv_flow$simu,cv_flow$observ)
Bias_v=sum(cv_flow$simu)/sum(cv_flow$observ) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error_v=rmse(cv_flow$simu,cv_flow$observ, na.rm=TRUE)
text(15400,23, paste("Bias=",format(Bias_v,digit=2)),cex = 1.25)
text(15400,21, paste("NSE=",format(CE_v,digit=2)),cex = 1.25)
text(15400,19, paste("MSE=",format(Mean_error_v,digit=2)),cex =1.25)




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











############sensitivity analysis

setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A1_watershed_final/A1_TOPNETRUN")
parm=seq(0.1,10,0.4)   #seq(1,128.5,10), seq(1,477.2,38)  

#
CE=NA
Bias=NA
Mean_error=NA
for ( i in 1:length(parm)) {

f_parm=128.5 ##128.5
k_parm=477.02   ##477.02
dth1_parm=0.862 #0.862
dth2_parm=0.808 #0.808
soilc_parm=1
c_parm=1
psif_parm=1
chv_parm=1
cc_parm=136.41 #136.41
cr_parm=0.478 #0.478
sro_parm=1
cvo_parm=1
n_parm=1
T_parm=parm[i]

sink('topinp.dat')
cat(sprintf("%d  %s", 20080101, "start_date"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d  %s", 240000, "start_hour"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d  %s", 86400, "timestep(86400 s = 1 day)"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d  %d  %s",1827,1827, "nsteps  (11325 is 31 years, 19591001 to 19901002.  16894 is 46 years 19591001 to 20051231)"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d  %s", 1, "detailstart"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d  %s", 0, "detailend"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%d %d %d %s", 0,0,0, "Detail/Debug Output Control (3 flags.  OutputYes, Basin, IrrigDrainCase.  The first is 0 or 1 to indicate if topsbd_v7.txt is to be written or not.  The second flag controls whether detail output should be written for one basin or all basins. A value of 0 (with OutputYes=1) results in all basins being output. The third flac controls whether detail output should be written for one or all irrigation drainage cases.  A value of 0 implies all cases, otherwise just the case indicated is output (1: no irrig, no drainage, 2: tile no irrig, 3: ditch no irrig, 4: irrig no drainage, 5: irrig tile, 6: irrig ditch) "),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", f_parm, "f"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", k_parm, "Ko"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f %s", dth1_parm, "DTH1"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f %s", dth2_parm, "DTH2"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s",soilc_parm, "SOILC"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f %s", c_parm, "C"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s",psif_parm, "PSIF"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s",chv_parm, "CHV"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)

cat(sprintf("%f  %s",cc_parm, "CC"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", cr_parm, "CR"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s",1, "Albedo"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", sro_parm, "SRO"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f %s", 1, "ZBARO"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", cvo_parm, "CVO"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", n_parm, "n"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f %s", T_parm, "Transmissivity "),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
cat(sprintf("%f  %s", 0.5, "ImperviousFraction"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
sink()
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
CE[i]=NSE(calibrated_flow$simu,calibrated_flow$observ)
Bias[i]=pbias(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error[i]=rmse(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE)

}

sensitivity=data.frame(parm=parm,NSE=CE,Bias=Bias,RMSE=Mean_error)



par(mar=c(5, 12, 4, 4) + 0.1)
plot(sensitivity$parm, sensitivity$NSE, axes=F, ylim=c(min(sensitivity$NSE),max(sensitivity$NSE)), xlab="", ylab="",type="l",col="black", main="",xlim=c(0,10))
points(sensitivity$parm, sensitivity$NSE,pch=20,col="black")
axis(2, ylim=c(min(sensitivity$NSE),max(sensitivity$NSE)),col="black",lwd=2)
mtext(2,text="NSE",line=2)

par(new=T)
plot(sensitivity$parm, sensitivity$Bias, axes=F, ylim=c(min(sensitivity$Bias),max(sensitivity$Bias)), xlab="", ylab="", type="l",lty=2, main="",xlim=c(0,10),lwd=2)
axis(2, ylim=c(min(sensitivity$Bias),max(sensitivity$Bias)),lwd=2,line=3.5)
points(sensitivity$parm, sensitivity$Bias,pch=20)
mtext(2,text="Bias",line=5.5)

par(new=T)
plot(sensitivity$parm, sensitivity$RMSE, axes=F, ylim=c(min(sensitivity$RMSE),max(sensitivity$RMSE)), xlab="", ylab="", type="l",lty=3, main="",xlim=c(0,10),lwd=2)
axis(2, ylim=c(min(sensitivity$RMSE),max(sensitivity$RMSE)),lwd=2,line=7)

points(sensitivity$parm, sensitivity$RMSE,pch=20)
mtext(2,text="RMSE",line=9)


axis(1,pretty(range(sensitivity$parm),10))
mtext(" tr factor ",side=1,col="black",line=2) ## change title 
legend(x=5,y=1.9,legend=c("NSE","Bias","RMSE"),lty=c(1,2,3)) ## change legend location


#########testing ##########
pr=t(matrix(scan("rain.dat", what="",skip=3),nrow=32,ncol=12054))
time_a= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_c= seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all) # get overlap time interval
observd_pr=(pr[date_overlap,25])
plot(observd_pr)











pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=29+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later
lines(simu_rain, type="o", col="blue",ylim=rev(range(simu_rain)))
plot(calibrated_flow$simu, type="o", pch=22, lty=2, col="red",ylim=(range(calibrated_flow$simu)))

par(mar=c(2, 4, 0, 6)+0.25)
plot(time, calibrated_flow$simu, axes=T, ylim=c(0,60), xlab="", ylab="",type="l",lty=1,col="black", main="")
points(time, calibrated_flow$simu,pch=20,col="black")
axis(2, ylim=c(0,30),col="black",lwd=2)
mtext(2,text="Stream flow(m3/s)",line=2)

par(new=T)
plot(time, calibrated_flow$observ, axes=T, ylim=c(0,60), xlab="", ylab="",type="l",lty=2,col="red", main="")
points(time, calibrated_flow$observ,pch=20,col="red")
axis(2, ylim=c(0,30),col="red",lwd=2)
#mtext(2,text="NSE",line=2)

par(new=T)
plot(time, simu_rain[732:1827,], axes=F, ylim=rev(c(0,60)), col="darkgrey",xlab="", ylab="", type="l",lty=3, main="",lwd=2)
axis(4, ylim=rev(range(simu_rain[732:1827,])),col="darkgrey",lwd=1,line=1.5)
#points(time, simu_rain[732:1827,],pch=20)
mtext(4,text="Rainfall (mm/day)",line=-.1)


#axis(1,pretty(range(time),4))
mtext(" tr factor ",side=1,col="black",line=2) ## change title 
legend(15270,40,legend=c("Simulate flow","Observed flow","Precipitation "),lty=c(1,2,3),col=c("black","red","darkgrey")) ## change legend location





###overall water balance######


sim_cum=cumsum(calibrated_flow$simu)
obs_cum=cumsum(calibrated_flow$observ)
plot(time,obs_cum, type="o", col="red",ylim=range(obs_cum),lty=1,xlab="time(days)",ylab="cumulative Streamflow(m3/s)")

# Graph trucks with red dashed line and square points
lines(time,sim_cum, type="o", pch=22, lty=2, col="black")

legend(15270,800,legend=c("Observed flow","Simulatedflow"),lty=c(1,2),col=c("red","black"))





################TOPNET MODDEL RUN WITH CLIMATE CHANGE DATA  ################################

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final_DAYMET\\A1_TOPNETRUN_CC")
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
time_cali= seq(as.Date("2006/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
calibrated_flow=data.frame(simu=simu_flow[732:1827,],observ=observd_flow[732:1827,]) ##take only from 2010/01/01---2012/12/31
time= seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
plot(time,calibrated_flow$observ, type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")
lines(time,calibrated_flow$simu, type="o", pch=22, lty=2, col="red")
legend(14650, max(calibrated_flow$observ), c("observed flow","simulated flow"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);
CE=NSE(calibrated_flow$simu,calibrated_flow$observ)
Bias=pbias(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error=rmse(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE)










ev=scan("Evaporation_mm.txt", what="")
#l=basin_number+2
l=25+2#for A2
ev1=ev[seq(l,length(ev),1)] ## need to change theis things later
simu_eva=matrix(as.numeric(ev1[seq(2,length(ev1),l-1)])) ##need to change this later
par(mfrow=c(3,1))
plot(simu_rain,type='l')
plot(observd_flow,col="blue",type='l')
plot(simu_flow,col="red",type='l')
lines(simu_flow,col="red")
lines(observd_flow,col="blue")
lines(simu_eva,col="yellow")
max(simu_rain)
max(simu_eva)




