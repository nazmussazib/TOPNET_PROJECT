library(hydroGOF)
library(zoo)
library(plyr)
library(nsga2R)
require(hydroTSM)
##historcial run with daymet driven data #######


setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C22_Watershed_final2/C22_TOPNETRUN")



setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\B21_NC\\B21_TOPNETRUN - Copy")

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C21_Watershed_final\\C21_TOPNETRUN")

system(paste("topnet_modified"))
ff=scan("FlowAtStreamNodes_cms.txt", what="")
#l=basin_number+2
l=59+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later

sf=scan("streamflow_calibration.dat", what="")
sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_his= seq(as.Date("1986/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_his,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
#simu_flow[simu_flow<0.01]=0
sim_dm_flow=matrix(simu_flow[366:7670])
obs_flow=matrix(observd_flow)

sf_obs_mon<- data.frame(monthlyfunction(obs_flow, FUN=mean, na.rm=TRUE,dates=time_his))
sf_mod_dm_mon<- data.frame(monthlyfunction(sim_dm_flow, FUN=mean, na.rm=TRUE,dates=time_his))



plot(seq(1,12,1),sf_obs_mon)
lines(seq(1,12,1),sf_mod_dm_mon,col='black',lty=5)
lines(seq(1,12,1),sf_mod_dm_mon,col='black',lty=5,type="o")

time_his= seq(as.Date("1986/1/1"), as.Date("2005/12/31"), "day")
time= seq(as.Date("1999/1/1"), as.Date("2005/12/31"), "day")
date_overlap2=match(time,time_his,) # get overlap time interval
CE=NSE(sim_dm_flow,obs_flow)
bias=sum(sim_dm_flow[date_overlap2])/sum(obs_flow[date_overlap2])
CE=NSE(sim_dm_flow[date_overlap2],obs_flow[date_overlap2])
plot(obs_flow)
lines(sim_dm_flow)



###historical driven with CanESM2 data ###########






###historical driven with CCSM4 data ###########


###historical driven with GFDL-CM3 data ###########


###historical driven with CNRM-CM5/MIROC5 data ###########



##historical driven with 

##calibration plot###

time_c= seq(as.Date("1999/1/1"), as.Date("2002/12/31"), "day") # create time series based on start and end date
plot(time_c,cv_flow$observ, type="o", col="blue",,xlab=" Time(days)",ylab="stream flow (m3/s)",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)
lines(time_c,cv_flow$simu, type="o", pch=22, lty=2, col="red")
legend('topright', max(calibrated_flow$observ), c("observed flow","simulated flow"), bty="n",cex=1.2, col=c("blue","red"), pch=21:22, lty=1:2,pt.cex=1.2);
CE_c=NSE(cv_flow$simu,cv_flow$observ)
Bias_c=sum(cv_flow$simu[1:1460])/sum(cv_flow$observ[1:1460]) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error_c=rmse(cv_flow$simu[1:1460],cv_flow$observ[1:1460], na.rm=TRUE)
text(14250, 5, paste("Bias=",format(Bias_c,digit=2)),cex = 1.25)
text(14250, 4.6, paste("NSE=",format(CE_c,digit=2)),cex = 1.25)
text(14250, 4.2, paste("MSE=",format(Mean_error_c,digit=2)),cex =1.25)

###cumulative rainfall and run off ####

pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=29+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later



plot(time_c,cumsum(0.2*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$simu),col='red')

plot(time_c,cumsum(simu_rain[731:2191]),ylab="cumulative rainfall/flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.2*cv_flow$simu),col='red')
legend(14650, 800, c("rainfall","observed flow","simulated flow"), cex=0.8, col=c("black","blue","red"), pch=21, lty=1);
