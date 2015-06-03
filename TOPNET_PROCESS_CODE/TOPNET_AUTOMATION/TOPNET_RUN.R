
##First get all input parameter files to RUN TOPNET model


##copy files from WD directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C1_Watershed/C1_WD")
targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C1_Watershed/C1_TOPNETRUN")
file.copy(paste (origindir, "distribution.txt", sep = "/"), paste(targetdir, "distribution.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "nodelinks.txt", sep = "/"), paste(targetdir, "nodelinks.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchlink.txt", sep = "/"), paste(targetdir, "rchlink.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchareas.txt", sep = "/"), paste(targetdir, "rchareas.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rchproperties.txt", sep = "/"), paste(targetdir, "rchproperties.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

##copy files from Soil data directory

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C1_Watershed/C1_SD")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file.copy(paste (origindir, "basinpars.txt", sep = "/"), paste(targetdir, "basinpars.txt" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

## copy files from Climate directory
origindir <- c("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C1_Watershed/C1_CD/C1_CD_present")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file.copy(paste (origindir, "rain.dat", sep = "/"), paste(targetdir, "rain.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "tmaxtmintdew.dat", sep = "/"), paste(targetdir, "tmaxtmintdew.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "rainweights.txt", sep = "/"), paste(targetdir, "rainweights.txt", sep = "/"), recursive=FALSE,copy.mode = TRUE)
file.copy(paste (origindir, "wind.dat", sep = "/"), paste(targetdir, "wind.dat", sep = "/"), recursive=FALSE,copy.mode = TRUE)

#file.copy(paste (origindir, "rainweights.txt", sep = "/"), paste(targetdir, "interpweight.dat" , sep = "/"), recursive=FALSE,copy.mode = TRUE)

##copy other parameter files
##Need to work on that to automate other parameter files

origindir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/Parameter_copy")
#targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/TOPNET_RUN")
file_list=list.files(path = "E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/Parameter_copy", pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

copyfiles=function(filestocopy){
  file.copy(paste (origindir, filestocopy , sep = "/"), paste(targetdir, filestocopy , sep = "/"),
            recursive=FALSE,copy.mode = TRUE)}
lapply(file_list,copyfiles)


##Need to change modelspc file,basin number, reach number gauege number (automation future)
##Need to fix latlon,clipar.dat (need to work)
##Need to copy rainweight as interpweight.dat


##Run model

library(hydroGOF)
library(zoo)
library(plyr)

setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed/A2_TOPNETRUN")

#add one line in  stream flow calibration.dat file 

##trying to do some calibration:


# f_parm=0.00386435871791497
# k_parm=29.919154179363
# T_parm=0.138224504368551
# dth1_param=0.00815346493069726
#   cr_param=0.13271671710971
#   albedo_param=0.350020779453418
#   sro_param=0.676514203795983

calibration=function(x){
  f_parm=x[1]
  k_parm=x[2]
  dth1_parm=x[3]
  dth2_parm=x[4]
  psif_parm=x[5]
  cc_parm=x[6]
  cr_parm=x[7]
  albedo_parm=x[8]
  sro_parm=x[9]
  cvo_parm=x[10]
  T_parm=x[11]
  n_parm=x[12]
  zbaro_parm=x[13]
  
  sink('topinp.dat')
  cat(sprintf("%d  %s", 20100101, "start_date"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 240000, "start_hour"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 86400, "timestep(86400 s = 1 day)"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %d  %s", 365,365, "nsteps  (11325 is 31 years, 19591001 to 19901002.  16894 is 46 years 19591001 to 20051231)"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 1, "detailstart"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d  %s", 0, "detailend"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%d %d %d %s", 0,0,0, "Detail/Debug Output Control (3 flags.  OutputYes, Basin, IrrigDrainCase.  The first is 0 or 1 to indicate if topsbd_v7.txt is to be written or not.  The second flag controls whether detail output should be written for one basin or all basins. A value of 0 (with OutputYes=1) results in all basins being output. The third flac controls whether detail output should be written for one or all irrigation drainage cases.  A value of 0 implies all cases, otherwise just the case indicated is output (1: no irrig, no drainage, 2: tile no irrig, 3: ditch no irrig, 4: irrig no drainage, 5: irrig tile, 6: irrig ditch) "),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", f_parm, "f"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", k_parm, "Ko"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f %s", dth1_parm, "DTH1"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f %s", dth2_parm, "DTH2"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s",1, "SOILC"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f %s", 1, "C"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s",psif_parm, "PSIF"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", 1, "CHV"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  
  cat(sprintf("%f  %s",cc_parm, "CC"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", cr_parm, "CR"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", albedo_parm, "Albedo"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", sro_parm, "SRO"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", zbaro_parm, "ZBARO"),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", cvo_parm, "CVO"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", n_parm, "n"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f %s", T_parm, "Transmissivity "),file='topinp.dat',append=TRUE); cat("\n", file="topinp.dat", append=TRUE)
  cat(sprintf("%f  %s", 0.5, "ImperviousFraction"),file='topinp.dat',append=TRUE) ;cat("\n", file="topinp.dat", append=TRUE)
  sink()
  
  
  
  system(paste("topnet_modified"))
  
  
  ff=scan("FlowAtStreamNodes_cms.txt", what="")
  #l=basin_number+2
  l=157 #for A2
  ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
  simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
  sf=scan("streamflow_calibration.dat", what="")
  sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
  obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
  
  time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
  time_cali= seq(as.Date("2010/1/1"), as.Date("2010/12/31"), "day") # create time series based on start and end date
  date_overlap=match(time_cali,time_all,) # get overlap time interval
  observd_flow=matrix(obs_flow[date_overlap])
  calibrated_flow=data.frame(simu=simu_flow,observ=observd_flow)
  plot(observd_flow, type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")
  lines(simu_flow, type="o", pch=22, lty=2, col="red")
  
  legend(1, 25, c("observed flow","simulated flow"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);
  res=NSE(simu_flow,observd_flow)
  return(res)
}



library(rgenoud)
xe1=data.matrix(c(0.001,0.1,0.01,0.01,0.01,0.01,0.001,0.001,0.01,0.01,0.001,0.001,0.01))
xe2=data.matrix(c(100,500,10,10,10,10,10,10,10,10,100,1,10))
xd=cbind(xe1,xe2)
claw1 <- genoud(calibration, nvars=13,pop.size=30,Domains=xd,max=TRUE)
j <- list(fnscale=-1)

optim(c(0.001,0.1,0.01,0.01,0.01,0.01,0.001,0.001,0.01,0.01,0.001,0.001,0.01),calibration,j)




