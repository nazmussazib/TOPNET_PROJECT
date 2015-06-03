
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/getData.r")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/WY_conv.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/PCM.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/Q7MaxMin.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/timings.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/TPTH.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/flowrev.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/BFI_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/COVyr.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/MEANyr.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/zeroday.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/q167.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/q167_lp3.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/COV.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/qmean_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/qmean_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/SI.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/flw_puls_evnt_5_95.R")

##Run model for historical and projection period for different watershed #######
library(hydroGOF)
library(zoo)
library(plyr)
library(nsga2R)
require(hydroTSM)
##historcial run with daymet driven data #######

###change the directory for each watershed #########
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
w=1
###reading all streamflow file for different model #########

ff=scan("FlowAtStreamNodes_cms.txt", what="")
#l=basin_number+2
l=29+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later

sf=scan("streamflow_calibration.dat", what="")
sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_his= seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_his,time_all,) # get overlap time interval
observd_flow=matrix(obs_flow[date_overlap])
observd_flow[observd_flow<0] <- NA

sim_dm_flow=matrix(simu_flow[367:9497,])
obs_flow=matrix(observd_flow)

##comparing monthly streamflow########
dates_proj=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")

sf_obs_mon<- data.frame(monthlyfunction(obs_flow, FUN=mean, na.rm=TRUE,dates=time_his))
sf_mod_dm_mon<- data.frame(monthlyfunction(sim_dm_flow, FUN=mean, na.rm=TRUE,dates=time_his))
sf_mod_proj<- data.frame(monthlyfunction(simu_flow, FUN=mean, na.rm=TRUE,dates=dates_proj))
