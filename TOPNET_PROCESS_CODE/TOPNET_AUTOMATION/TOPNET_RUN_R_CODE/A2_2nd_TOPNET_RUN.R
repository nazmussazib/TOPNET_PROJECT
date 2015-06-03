

library(hydroGOF)
library(zoo)
library(plyr)


##First get all input parameter files to RUN TOPNET model


##copy files from WD directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed_2nd/A2_WD")
targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed_2nd/A2_TOPNETRUN")
file.copy(paste (origindir, "distribution.txt", sep = "/"), paste(targetdir, "distribution.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "nodelinks.txt", sep = "/"), paste(targetdir, "nodelinks.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchlink.txt", sep = "/"), paste(targetdir, "rchlink.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchareas.txt", sep = "/"), paste(targetdir, "rchareas.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchproperties.txt", sep = "/"), paste(targetdir, "rchproperties.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

##copy files from Soil data directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed_2nd/A2_SD")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file.copy(paste (origindir, "basinpars.txt", sep = "/"), paste(targetdir, "basinpars.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

## copy files from Climate directory
origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_watershed_2nd/A2_CD_present/A2_CD_Daymet")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file.copy(paste (origindir, "rain.dat", sep = "/"), paste(targetdir, "rain.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "tmaxtmintdew.dat", sep = "/"), paste(targetdir, "tmaxtmintdew.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rainweights.txt", sep = "/"), paste(targetdir, "rainweights.txt", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "wind.dat", sep = "/"), paste(targetdir, "wind.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "clipar.dat", sep = "/"), paste(targetdir, "clipar.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "latlongfromxy.txt", sep = "/"), paste(targetdir, "latlongfromxy.txt", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "streamflow_calibration.dat", sep = "/"), paste(targetdir, "streamflow_calibration.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "bndryflow.dat", sep = "/"), paste(targetdir, "bndryflow.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "interpweight.dat", sep = "/"), paste(targetdir, "interpweight.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)
#file.copy(paste (origindir, "rainweights.txt", sep = "/"), paste(targetdir, "interpweight.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

##copy other parameter files


origindir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/Parameter_copy")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file_list=list.files(path = "E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/Parameter_copy", pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,
                     ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

copyfiles=function(filestocopy){
  file.copy(paste (origindir, filestocopy , sep = "/"), paste(targetdir, filestocopy , sep = "/"),
            recursive=FALSE,copy.mode = TRUE)}
lapply(file_list,copyfiles)







##########Before Run model


##Need to change modelspc file,basin number, reach number gauege number (automation future)
##Copy rainweights.txt as interpweight.dat
##change straemflow_calibration.dat 1 line
#change bndryflow.dat in 1 line

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_TOPNETRUN")

#trying to do some calibration:


calibration=function(x){
  f_parm=x[1]
  k_parm=x[2]
  dth1_parm=x[3]
  dth2_parm=x[4]
  soilc_parm=x[5]
  c_parm=1
  psif_parm=1
  chv_parm=1
  cc_parm=x[6]
  cr_parm=x[7]
  sro_parm=1
  cvo_parm=1
  n_parm=1
  T_parm=x[8]
  
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
  l=113+2
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
  CE=NSE(calibrated_flow$simu,calibrated_flow$observ,na.rm=TRUE)
  return(CE)
  
}

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
xe1=(c(0.1,0.1,0.5,0.5,0.75,0.1,0.25,0.1))
xe2=(c(1000,1000,1,1,10,10,10,1000))
hydroPSO(fn=calibration,lower=xe1,upper=xe2,control=list(MinMax='max'))


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
Bias=pbias(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error=rmse(calibrated_flow$simu,calibrated_flow$observ, na.rm=TRUE)


############sensitivity analysis

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_PRISM\\A2_TOPNETRUN")
parm=seq(0.1,10,0.5)   #seq(1,128.5,10), seq(1,477.2,38)  

#
CE=NA
Bias=NA
Mean_error=NA
for ( i in 1:length(parm)) {
  
  f_parm=1 ##128.5
  k_parm=1  ##477.02
  dth1_parm= #0.862
  dth2_parm=1#0.808
  soilc_parm=1
  c_parm=1
  psif_parm=1
  chv_parm=1
  cc_parm=1 #136.41
  cr_parm=1 #0.478
  sro_parm=1
  cvo_parm=1
  n_parm=parm[i]
  T_parm=1
  
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
  l=113+2
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






