
###########NLDAS rainfall comparison #############
setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed/A2_CD/A2_CD_present/A2_NLDAS/A2_N_Rainfall")
rain_nldas=readLines('rain.dat')[4:1829]
sf=as.matrix(unlist(strsplit(rain_nldas, split=" ")))
df=matrix(sf[which(!sf == "" )],nrow=46,ncol=1826)
df=df[-46,]
df=t(df[-45,])
NLDAS_rain=rowMeans(matrix(as.numeric(df),nrow=1826,ncol=44))


#########Daymet rainfall data ##############
setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C21_Watershed/C21_CD/C21_CD_present/C21_Daymet")
pr_daymet=readLines('rain.dat')[4:12057]
pr1=as.matrix(unlist(strsplit(pr_daymet, split=" ")))
pr=matrix(pr1[which(!pr1 == "" )],nrow=23,ncol=12054)
pr=pr[-23,]
pr=t(pr[-22,])
time_all= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
time_cali= seq(as.Date("2008/1/1"), as.Date("2012/12/31"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all,) # get overlap time interval
pr_cali=rowMeans(matrix(as.numeric(pr[date_overlap,]),nrow=1827,ncol=21)) ## change number of row based on date time
lines(pr_cali)
plot(pr_cali)
##########stream flow##########
rf=scan("streamflow_calibration.dat", what="")
rf1=rf[seq(20,length(rf),1)] ## need to change theis things later
rf_flow=(as.numeric(rf1[seq(1,length(rf1),3)])) ##need to change this later
observd_flow=matrix(rf_flow[date_overlap])
lines(observd_flow)

########Comparison#############

par(mfrow=c(3,1))
plot(time_cali,pr_cali, type="o", col="red",,xlab=" Time days",ylab="precipitation mm/day")
plot(time_cali,observd_flow, type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")
plot(time_cali,NLDAS_rain[1:1461], type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")



plot(pr_cali, type="o", pch=22, lty=2, col="red")

lines(cumsum(observd_flow[1:1095]), type="o", col="blue",,xlab=" Time days",ylab="stream flow (m3/s)")
plot(cumsum(NLDAS_rain[1:1095]*3), type="o", pch=22, lty=2, col="red")