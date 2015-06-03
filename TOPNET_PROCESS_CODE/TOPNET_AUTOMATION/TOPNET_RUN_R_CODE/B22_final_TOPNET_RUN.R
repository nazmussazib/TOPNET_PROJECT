

library(hydroGOF)
library(zoo)
library(plyr)


##First get all input parameter files to RUN TOPNET model


##copy files from WD directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/B22_Watershed_new/B22_WD")
targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/B22_Watershed_new/B22_TOPNETRUN")
file.copy(paste (origindir, "distribution.txt", sep = "/"), paste(targetdir, "distribution.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "nodelinks.txt", sep = "/"), paste(targetdir, "nodelinks.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchlink.txt", sep = "/"), paste(targetdir, "rchlink.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchareas.txt", sep = "/"), paste(targetdir, "rchareas.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchproperties.txt", sep = "/"), paste(targetdir, "rchproperties.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

##copy files from Soil data directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/B22_Watershed_new/B22_SD")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file.copy(paste (origindir, "basinpars.txt", sep = "/"), paste(targetdir, "basinpars.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

## copy files from Climate directory
origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/B22_Watershed_new/B22_CD")
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

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\All_wareshed_others\\B22_Watershed\\B22_TOPNETRUN")
setwd("C:\\B22_watershed_final\\B22_TOPNETRUN")
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
  cat(sprintf("%d  %s", 19970101, "start_date"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 240000, "start_hour"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 86400, "timestep(86400 s = 1 day)"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %d  %s",2191,2191, "nsteps  (11325 is 31 years, 19591001 to 19901002.  16894 is 46 years 19591001 to 20051231)"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
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
  l=43+2
  ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
  simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
  sf=scan("streamflow_calibration.dat", what="")
  sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
  obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
  
  time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
  time_warm_cali= seq(as.Date("1997/1/1"), as.Date("2002/12/31"), "day") # create time series based on start and end date
  date_overlap=match(time_warm_cali,time_all) # get overlap time interval
  observd_flow=matrix(obs_flow[date_overlap])
  observd_flow[observd_flow<0] <- NA
  calibrated_flow=data.frame(simu=simu_flow[731:2191,],observ=observd_flow[731:2191,]) ##take only from 2010/01/01---2012/12/31
  time= seq(as.Date("1999/1/1"), as.Date("2002/12/31"), "day") # create time series based on start and end date
  plot(time,calibrated_flow$observ, type="o", col="blue",,xlab=" Time(days)",ylab="stream flow (m3/s)",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)
  lines(time,calibrated_flow$simu, type="o", pch=22, lty=2, col="red")
  #legend(14650, max(calibrated_flow$observ), c("observed flow","simulated flow"), cex=1, col=c("blue","red"), pch=21:22, lty=1:2,pt.cex=1);
  CE=NSE(calibrated_flow$simu,calibrated_flow$observ,na.rm=TRUE)
  BIAS=sum(calibrated_flow$simu,na.rm=TRUE)/sum(calibrated_flow$observ,na.rm=TRUE)
  return(CE)
  
}

library(hydroPSO)
xe1=(c(0.1,1,0.5,0.5,0.5,0.1,0.75,0.4))
xe2=(c(25,500,1,2,3,10,10,40))
hydroPSO(fn=calibration,lower=xe1,upper=xe2,control=list(MinMax='max'))
resu=nsga2R(fn=calibration, 8, 2, lowerBounds = xe1, upperBounds = xe2,
            popSize = 100)






#library(rgenoud)
#xe1=(c(0.01,0.1,0.5,0.6,1,1,0.01))
#xe2=(c(500,1000,1,1,500,100,50))
#xd=cbind(xe1,xe2)
#claw1 <- genoud(calibration, nvars=7,pop.size=100,Domains=xd,max=TRUE)

library(hydroPSO)
xe1=(c(0.1,1,0.5,0.5,0.5,0.01,0.75,0.1))
xe2=(c(50,800,1,2,3,20,10,50))
hydroPSO(fn=calibration,lower=xe1,upper=xe2,control=list(MinMax='max'))




########statistices########
##After calibration , need to do some statitical analysis of the results
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\B22_Watershed_final\\B22_TOPNETRUN")

system(paste("topnet_modified"))
ff=scan("FlowAtStreamNodes_cms.txt", what="")
#l=basin_number+2
l=43+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
sf=scan("streamflow_calibration.dat", what="")
sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later

#time_all= seq(as.Date("2007/1/1"), as.Date("2013/12/27"), "day") #create time series
time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_cv= seq(as.Date("1999/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cv,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA
cv_flow=data.frame(simu=simu_flow[731:3287,],observ=observd_flow) ##take only from 2010/01/01---2012/12/31

##calibration plot###
time_c= seq(as.Date("1999/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
plot(time_c,cv_flow$observ, type="o", col="blue",,xlab=" Time(days)",ylab="stream flow (m3/s)",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)
lines(time_c,cv_flow$simu, type="o", pch=22, lty=2, col="red")
lines(rep(time_c[1461],181)  ,seq(0,180,1),col="grey60",lty=2)
#legend('topleft', max(calibrated_flow$observ), c("observed flow","simulated flow"), bty="n",cex=1.2, col=c("blue","red"), pch=21:22, lty=1:2,pt.cex=1.2);
CE_c=NSE(cv_flow$simu[1:1460],cv_flow$observ[1:1460])
Bias_c=sum(cv_flow$simu[1:1460])/sum(cv_flow$observ[1:1460]) ##PBIAS=100*[sum(sim-obs)/sum(obs)]

CE_v=NSE(cv_flow$simu[1461:2550],cv_flow$observ[1461:2550])
Bias_v=sum(cv_flow$simu[1461:2550])/sum(cv_flow$observ[1461:2550]) ##PBIAS=100*[sum(sim-obs)/sum(obs)]

text(time_c[1100],160, "B22",cex = 2)
#text(time_c[150],610, "Calibration",cex = 1.25)
text(time_c[400],150, paste("Bias=",format(Bias_c,digit=2)),cex = 1.25)
text(time_c[400],140, paste("NSE=",format(CE_c,digit=2)),cex = 1.25)
text(time_c[1750],150, paste("Bias=",format(Bias_v,digit=2)),cex = 1.25)
text(time_c[1750],140, paste("NSE=",format(CE_v,digit=2)),cex = 1.25)




pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=45+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later



plot(time_c,cumsum(0.11*cv_flow$observ[1:1096]),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.11*cv_flow$simu[1:1096]),col='red')

plot(time_c,cumsum(simu_rain[731:1826]),ylab="cumulative rainfall/flow (mm)")
lines(time_c,cumsum(0.11*cv_flow$observ[1:1096]),col='blue',ylab="cumulative flow (mm)")
lines(time_c,cumsum(0.11*cv_flow$simu[1:1096]),col='red')
legend(14650, 800, c("rainfall","observed flow","simulated flow"), cex=0.8, col=c("black","blue","red"), pch=21, lty=1);








##validation plot###



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
time_cv= seq(as.Date("2004/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cv,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- 0
simu_flow[simu_flow<0] <- 0
cv_flow=data.frame(simu=simu_flow[366:1096],observ=observd_flow) ##take only from 2010/01/01---2012/12/31


time_v= seq(as.Date("2004/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
plot(time_v,cv_flow$observ, type="o", col="blue",cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)#,,xlab=" Time days",ylab="stream flow (m3/s)")
lines(time_v,cv_flow$simu, type="o", pch=22, lty=2, col="red")
#legend(14650, max(cv_flow$observ), c("observed flow","simulated flow"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);
CE_v=NSE(cv_flow$simu,cv_flow$observ)
Bias_v=sum(cv_flow$simu)/sum(cv_flow$observ) ##PBIAS=100*[sum(sim-obs)/sum(obs)]
Mean_error_v=rmse(cv_flow$simu,cv_flow$observ, na.rm=TRUE)
text(15400,49, paste("Bias=",format(Bias_v,digit=2)),cex = 1.25)
text(15400,44, paste("NSE=",format(CE_v,digit=2)),cex = 1.25)
text(15400,39, paste("MSE=",format(Mean_error_v,digit=2)),cex =1.25)







plot(time_v,cumsum(0.11*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_v,cumsum(0.11*cv_flow$simu),col='red')

pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=45+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later



plot(time_v,cumsum(simu_rain),ylab="cumulative rainfall/flow (mm)")
lines(time_v,cumsum(0.11*cv_flow$observ),col='blue',ylab="cumulative flow (mm)")
lines(time_v,cumsum(0.11*cv_flow$simu),col='red')
legend(14650, 800, c("rainfall","observed flow","simulated flow"), cex=0.8, col=c("black","blue","red"), pch=21, lty=1);


















pr=scan("Precipitation_mm.txt", what="")
#l=basin_number+2
l=45+2#for A2
pr1=pr[seq(l,length(pr),1)] ## need to change theis things later
simu_rain=matrix(as.numeric(pr1[seq(2,length(pr1),l-1)])) ##need to change this later
plot(time_c,simu_rain[731:1826], type="o", col="red")

plot(cv_flow$simu, type="o", pch=22, lty=2, col="red",ylim=(range(cv_flow$simu)))
plot(time_cv,cv_flow$observ, type="o", pch=22, lty=2, col="red",ylim=(range(cv_flow$observ)))
plot(time_cv[1:365],cumsum(simu_rain[732:1096]), type="o", col="red")
lines(time_cv[1:365],cumsum(0.154*cv_flow$observ[1:365]), type="o", col="red")
lines(time_cv[1:365],cumsum(0.154*cv_flow$simu[1:365]), type="o", col="red")
par(mfrow=c(2,1))
plot(time_c,simu_rain[732:1827])
plot(time_c,cv_flow$observ)

par(mar=c(2, 4, 0, 6)+0.25)
plot(time, calibrated_flow$simu, axes=T, ylim=c(0,300), xlab="", ylab="",type="l",lty=1,col="red", main="")
points(time, calibrated_flow$simu,pch=20,col="red")
axis(2, ylim=c(0,30),col="red",lwd=2)
mtext(2,text="Stream flow(m3/s)",line=2)

par(new=T)
plot(time, calibrated_flow$observ, axes=T, ylim=c(0,300), xlab="", ylab="",type="l",lty=2,col="blue", main="")
points(time, calibrated_flow$observ,pch=20,col="blue")
axis(2, ylim=c(0,600),col="blue",lwd=2)
#mtext(2,text="NSE",line=2)

par(new=T)
plot(time, simu_rain[732:1827,], axes=F, ylim=rev(c(0,200)), col="darkgrey",xlab="", ylab="", type="l",lty=3, main="",lwd=2)
axis(4, ylim=rev(range(simu_rain[732:1827,])),col="darkgrey",lwd=1,line=1.5)
#points(time, simu_rain[732:1827,],pch=20)
mtext(4,text="Rainfall (mm/day)",line=-.1)


#axis(1,pretty(range(time),4))
mtext(" tr factor ",side=1,col="black",line=2) ## change title 
legend(15270,70,legend=c("Simulate flow","Observed flow","Precipitation "),lty=c(1,2,3),col=c("red","blue","darkgrey")) ## change legend location





###overall water balance######


sim_cum=cumsum(calibrated_flow$simu)
obs_cum=cumsum(calibrated_flow$observ)
sim_precp=cumsum(simu_rain[732:1827,])
plot(time,sim_precp, type="o", col="red",ylim=range(sim_precp),lty=1,xlab="time(days)",ylab="cumulative precipitation(mm/day)")

plot(time,obs_cum, type="o", col="blue",ylim=range(obs_cum),lty=1,xlab="time(days)",ylab="cumulative Streamflow(m3/s)")

# Graph trucks with red dashed line and square points
lines(time,sim_cum, type="o", pch=22, lty=2, col="red")
legend(15270,400,legend=c("Observed flow","Simulatedflow"),lty=c(1,2),col=c("blue","red"))
###overall water balance######


sim_cum=cumsum(calibrated_flow$simu)
obs_cum=cumsum(calibrated_flow$observ)
plot(time,obs_cum, type="o", col="red",ylim=range(obs_cum),lty=1,xlab="time(days)",ylab="cumulative Streamflow(m3/s)")

# Graph trucks with red dashed line and square points
lines(time,sim_cum, type="o", pch=22, lty=2, col="black")

legend(15270,800,legend=c("Observed flow","Simulatedflow"),lty=c(1,2),col=c("red","black"))


##cumulative precipitation#########
pr_daymet=readLines('rain.dat')[4:12057]
ng=readLines('rain.dat')[3]
ng=as.numeric(as.matrix(unlist(strsplit(ng, split=" ")))[2])
pr1=as.matrix(unlist(strsplit(pr_daymet, split=" ")))
pr=matrix(pr1[which(!pr1 == "" )],nrow=ng+2,ncol=12054)
pr=pr[-(ng+2),]
pr=(t(pr[-(ng+1),]))

time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_cali= seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all,) # get overlap time interval
pr_cali=rowMeans(matrix(as.numeric(pr[date_overlap,]),nrow=1096,ncol=ng)) ## change number of row based on date time

plot(cumsum(pr_cali))
lines(cumsum(simu_rain[732:1827,]))

























