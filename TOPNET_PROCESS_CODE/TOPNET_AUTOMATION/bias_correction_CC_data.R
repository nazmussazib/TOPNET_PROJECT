library(ncdf4)
library(proj4)
library(rgdal)
library(raster)
library(shapefiles)
require(spsurvey)
require(geosphere)
require(TTR)
require(hydroTSM)
require(qmap)
require(ggplot2)
require(grid)
require(gridExtra)


Model='CCSM4'
setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
pr.nc = nc_open("BCCAv2_0.125deg_pr_day_CCSM4_rcp45_r2i1p1_20560101-20651231.nc")
latall=ncvar_get(pr.nc ,'latitude')
lonall=ncvar_get(pr.nc ,'longitude')


###change the directory for each watershed #########
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
w=6
setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data",sep=""))##for A1:


##change rain gauge shape file name

lat_lon_rg=read.dbf(paste("rg_",watsed[w],".dbf",sep="")) 
lat_lon_rg=do.call(cbind,lat_lon_rg)
lat_lon_rg1=albersgeod(lat_lon_rg[,1], lat_lon_rg[,2], sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)

lon_all=as.numeric(lonall)
lon_ws=signif(lat_lon_rg1[,1]+360,7)
lon_ID=match(lon_ws,lon_all,)

lat_all=as.numeric(latall)
lat_ws=signif(lat_lon_rg1[,2],7)
lat_ID=match(lat_ws,lat_all)
lat_lon_ID=data.frame(lon_ID=lon_ID,lat_ID=lat_ID)
len=length(lat_ID)

#####DAYMET data for each of the pixel #####################

for(i in 1:len){
  cat("filename",i,".csv", file="latlon.txt", sep="",append=TRUE)
  cat(",", file="latlon.txt", append=TRUE)
  cat(lat_lon_rg1[i,2], lat_lon_rg1[i,1], file="latlon.txt", sep=",",append=TRUE)
  cat(",", file="latlon.txt", append=TRUE)
  cat("ignore stuff",i, file="latlon.txt", sep="",append=TRUE)
  cat("\n", file="latlon.txt", append=TRUE)}

system("java -Xms512m -Xmx1024m -jar daymet_multiple_extraction.jar latlon.txt")

xx="file"
nc=len
for (i in 1:nc){
  xx[i]=paste("filename",i,".csv",sep="")
}
rainfall <- function(file ){
  mydata2 = read.csv(file,skip=6)
  my.data.frame <- mydata2[(mydata2$year%%4==0) & (mydata2$yday ==365), ]
  my.data.frame$yday=my.data.frame$yday+1
  total <- rbind( my.data.frame,mydata2)
  gh= total[with(total, order(year,yday)), ]
  rain=data.frame(rainmm=gh$prcp..mm.day., tmaxcel=gh$tmax..deg.c.,tmincel=gh$tmin..deg.c.,vppa= gh$vp..Pa.)
  
  return(rain)
}

dates_day=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") 
dss=data.frame(date=dates_day)

for ( m in 1:length(xx)){
  hjk=rainfall(xx[m])
  dss=cbind(dss,hjk)
}

dewT_d=matrix(ncol =nc, nrow = nrow(dss))
RH_d=matrix(ncol =nc, nrow = nrow(dss))
avgT_d=matrix(ncol = nc, nrow = nrow(dss))
satVp_d=matrix(ncol = nc, nrow = nrow(dss))
actVp_d=matrix(ncol = nc, nrow = nrow(dss))
precipitation_d=matrix(ncol = nc, nrow = nrow(dss))
tmaxtmintdew_d=matrix(ncol = 1, nrow = nrow(dss))


for (j in 1:(nc)){for (i in 1:nrow(dss))
{
  avgT_d[i,j]=0.5*(dss[i,4*j-1]+dss[i,4*j])
  satVp_d[i,j]=6.112*exp(17.67*avgT_d[i,j]/(avgT_d[i,j]+243.5)) # unit is hecto-pascal
  actVp_d[i,j]=satVp_d[i,j]-dss[i,4*j+1]/10000 # convert vapor pressure deficit pa to hpa
  
  dewT_d[i,j]=(log(actVp_d[i,j]/(6.11)))*243.5/(17.67-log(actVp_d[i,j]/(6.11)))
  RH_d[i,j]=actVp_d[i,j]*100/satVp_d[i,j]
  
}
}

for ( k in 1:(nc)){
  precipitation_d[,k]=dss[,4*k-2]
  tmaxtmintdew_d=cbind(tmaxtmintdew_d,dss[,4*k-1],dss[,4*k],dewT_d[,k])
}

precip_day=data.frame(precipitation_d)
temper_day=data.frame(tmaxtmintdew_d[,-1])


dates_bc=seq(as.Date("2006/1/1"), as.Date("2012/12/31"), "day") 
date_overlap_day=match(dates_bc,dates_day)
pr_DAY=(precip_day[date_overlap_day,])  ##daymet rainfall for 1996-2005 for each station


#####processiing projected rainfall  data #############

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))

rain <- function(file ){
  
  rain.nc = nc_open(file)
  rain_time=as.Date(ncvar_get(rain.nc,'time'),origin="1950-01-01")
  #strDates= as.character(rain_time)
  ra=matrix(NA,nrow=length( rain_time),ncol=len)
  for (i in 1:len){
    start=c(lat_lon_ID[i,1],lat_lon_ID[i,2],1)
    count=c(1,1,length( rain_time))
    ra[,i]=ncvar_get(rain.nc,'pr',start,count)
  }
  radata=data.frame(data=cbind(ra, rain_time-0.5))
  
  return(radata)
}


ng=len
rcp=2
proj_rain_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_pr_day_",Model,"_rcp",sep=""))
precp=data.frame(matrix(NA,nrow=1,ncol=ng+1))
for ( m in 1:length(proj_rain_file)){
  pp=rain(proj_rain_file[m])
  ne=colnames(pp)
  colnames(precp)[]=ne
  precp=rbind(precp,pp)
}

##reshape the data 
precp1=as.matrix((precp[-1,1:len]))
precp_rcp_all=cbind(precp1[1:12783,],precp1[12784:25566,],precp[2:12784,len+1])

dat_1=seq(as.Date("2006/1/1"), as.Date("2012/12/31"), "day")
dat_2=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")
dat_3=seq(as.Date("2090/1/1"), as.Date("2099/12/31"), "day")

match_precp1=match(dat_1,precp_rcp_all[,rcp*len+1])
match_precp2=match(dat_2,precp_rcp_all[,rcp*len+1])
match_precp3=match(dat_3,precp_rcp_all[,rcp*len+1])

###Order of data
#rcp 26/....................rcp26 rcp45.....................rcp45 rcp60...............rcp60 rcp85..................rcp85
precp_rcp_final=rbind(precp_rcp_all[match_precp1,],precp_rcp_all[match_precp2,],precp_rcp_all[match_precp3,])

##plotting to see the difference

par(mfrow=c(3,2))
for (i in 7:10){
  plot(cumsum(pr_DAY[,i]))
  lines(cumsum(precp_rcp_final[1:2557,i]),col='red')
}

######quantile mapping bias correction method##########

toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}

dff=t(matrix(unlist(lapply(dates_bc, toNumerics)),nrow=3,ncol=length(dates_bc)))

proj_dff1=t(matrix(unlist(lapply(dat_1, toNumerics)),nrow=3,ncol=length(dat_1)))
proj_dff2=t(matrix(unlist(lapply(dat_2, toNumerics)),nrow=3,ncol=length(dat_2)))
proj_dff3=t(matrix(unlist(lapply(dat_3, toNumerics)),nrow=3,ncol=length(dat_3)))

ex=rcp*len+1
proj_rain1=cbind(proj_dff1,precp_rcp_final[1:length(dat_1),-ex])
proj_rain2=cbind(proj_dff2,precp_rcp_final[(1+length(dat_1)):(length(dat_2)+length(dat_1)),-ex])
proj_rain3=cbind(proj_dff3,precp_rcp_final[(1+length(dat_2)+length(dat_1)):(length(dat_2)+length(dat_1)+length(dat_3)),-ex])

proj_rain1=data.frame(proj_rain1) ## for year 2006-2012
proj_rain2=data.frame(proj_rain2) ## for year 2056-2065
proj_rain3=data.frame(proj_rain3) ## for year 2090-2099

proj_bc_cu=proj_rain1 ##2006-2012
proj_bc_fu=rbind(proj_rain2,proj_rain3)          ## 2056-65 and 2090-99

##based on monthly data we will determine which month should use
pr_DAY=data.frame(rep(pr_DAY,2))

mon=c(1,2,3,4,5,6,7,8,9,10,11,12)

kk=proj_bc_fu

for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
    
    id=which(proj_bc_cu[,2]==mon[i])
    obs=pr_DAY[id,m]
    sim=proj_bc_cu[id,m+3]
    proj=proj_bc_fu[id,m+3]
    
    h1=sort(sim)
    h2=sort(obs)
    h3=sort(proj)
    rain_qm.fit1 <- fitQmapRQUANT(h1,h3,qstep=0.1,nboot=10,wet.day=1)
    his_corr1 <- doQmapRQUANT(proj,rain_qm.fit1,type="linear")
    
    rain_qm.fit2 <- fitQmapRQUANT(h2,h1,qstep=0.1,nboot=10,wet.day=1)
    his_corr2<- doQmapRQUANT(his_corr1,rain_qm.fit2,type="linear")
    
    #dx=his_corr1-his_corr2
    #proj_bc_fu[id,m+3]=proj_bc_fu[id,m+3]-dx
    proj_bc_fu[id,m+3]=his_corr2
  }
  
  
  print(i)
}

##see the patteen 
plot(proj_bc_fu[proj_bc_fu<0])
proj_bc_fu[proj_bc_fu<0]=0

##check whether really we have any correction

par(mfrow=c(3,2))
for (i in 1:6){
  plot(cumsum(kk[,i+3]))
  lines(cumsum(proj_bc_fu[,i+3]),col='red')
}





############Analysis of Rainfall Results compare to base period 2003-2012 ##########

dates_day=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") 
dates_base=seq(as.Date("2003/1/1"), as.Date("2012/12/31"), "day") ##base line period for 2003-2012

date_overlap_base=match(dates_base,dates_day) 
pr_DAY_base=(precip_day[date_overlap_base,])
pr_daymet_mean_base=rowMeans(pr_DAY_base)
pr_daymet_mean_base=matrix(pr_daymet_mean_base)
rain_dm_mon_base<- (data.frame(monthlyfunction(pr_daymet_mean_base, FUN=sum, na.rm=TRUE,dates=dates_base)))/10  ##as base period is 10 years

rain_bcca_mon_corr_56_65<- data.frame(monthlyfunction(proj_bc_fu[1:3653,seq(4,rcp*len+3,1)], FUN=sum, na.rm=TRUE,dates=dat_2))/10
rain_bcca_mon_corr_90_99<- data.frame(monthlyfunction(proj_bc_fu[3654:7305,seq(4,rcp*len+3,1)], FUN=sum, na.rm=TRUE,dates=dat_3))/10



par(mfrow=c(2,2))
barplot(colMeans(rain_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),]),ylab="Monthly Rainfall(mm)", main="RCP4.5 56-65",ylim=c(0,500))
barplot(colMeans(rain_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),]),main="RCP8.5",ylim=c(0,500))

barplot(colMeans(rain_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),]),ylab="Monthly Rainfall(mm)", main="RCP4.5 90-99",ylim=c(0,500))
barplot(colMeans(rain_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),]),main="RCP8.5",ylim=c(0,500))




par(mfrow=c(2,2))
barplot((colMeans(rain_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),])-data.matrix(rain_dm_mon_base)),ylab="Rainfall difference(mm)",main="RCP4.5 56-65",ylim=c(-40,40))
barplot(colMeans(rain_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),])-data.matrix(rain_dm_mon_base),main="RCP8.5",ylim=c(-40,40))

barplot((colMeans(rain_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),])-data.matrix(rain_dm_mon_base)),ylab="Rainfall difference(mm)",main="RCP4.5 90-99",ylim=c(-40,40))
barplot(colMeans(rain_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),])-data.matrix(rain_dm_mon_base),main="RCP8.5",ylim=c(-40,40))





#############WRITING RAIN.DAT for DIFFERNECT RCP SCENARIO#################


strDates=rbind(data.frame(date=dat_2),data.frame(date=dat_3))
gh=gsub("-","", strDates$date, fixed=TRUE)
day=data.frame(time=gh)
hour=rep.int(240000, nrow(day))

rcp_name=c("rcp4.5","rcp8.5")
nc=ng

###change the directory for each watershed #########
setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data",sep=""))##for A1:


precp_rcp_bias_corr=proj_bc_fu[,seq(4,rcp*len+3,1)]
for ( j in 1:rcp){
  precipp=data.frame(precp_rcp_bias_corr[,seq((nc*j-nc+1),nc*j,1)])
  
  #precipp=data.frame(precp_rcp_final[,seq((nc*j-nc+1),nc*j,1)])
  
  ##change here as date of 2056-2065 and 2090-2099
  
  precipp[] <- lapply(precipp, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  precipp=data.frame(precipp,day,hour)
  
  
  ##writing rain.dat file
  file=paste("rain_",rcp_name[j],".dat",sep="")
  sink(file) 
  cat(sprintf("This file provides daily precipitation rate for each rain station"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  cat(sprintf("Precipitation is provided in mm/day"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  sites=seq(1,(nc),1)
  cat(sprintf("%s %d ", "ver2",nc),file=file,append=TRUE) 
  cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  write.table(precipp, file =file,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  sink()
  
}



##################TEMPERATURE DATA PROCESSING#########################


setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))

tmax_obs=(tmaxtmintdew_d[date_overlap_day,seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_obs=(tmaxtmintdew_d[date_overlap_day,seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_obs=(tmaxtmintdew_d[date_overlap_day,seq(4,dim(tmaxtmintdew_d)[2],3)])

tmax <- function(file ){
  
  tmax.nc = nc_open(file)
  tmax_time=as.Date(ncvar_get(tmax.nc,'time'),origin="1950-01-01")
  #strDates= as.character(rain_time)
  tmax=matrix(NA,nrow=length(tmax_time),ncol=len)
  for (i in 1:len){
    start=c(lat_lon_ID[i,1],lat_lon_ID[i,2],1)
    count=c(1,1,length( tmax_time))
    tmax[,i]=ncvar_get(tmax.nc,'tasmax',start,count)
  }
  tmaxdata=data.frame(data=cbind(tmax, tmax_time-0.5))
  
  return(tmaxdata)
}


tmin <- function(file ){
  
  tmin.nc = nc_open(file)
  tmin_time=as.Date(ncvar_get(tmin.nc,'time'),origin="1950-01-01")
  #strDates= as.character(rain_time)
  tmin=matrix(NA,nrow=length(tmin_time),ncol=len)
  for (i in 1:len){
    start=c(lat_lon_ID[i,1],lat_lon_ID[i,2],1)
    count=c(1,1,length( tmin_time))
    tmin[,i]=ncvar_get(tmin.nc,'tasmin',start,count)
  }
  tmindata=data.frame(data=cbind(tmin, tmin_time-0.5))
  
  return(tmindata)
}



#############Reading projected data for##########

proj_tmax_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmax_day_",Model,"_rcp",sep=""))
proj_tmin_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmin_day_",Model,"_rcp",sep=""))

proj_tmaximum=data.frame(matrix(NA,nrow=1,ncol=len+1))
proj_tminimum=data.frame(matrix(NA,nrow=1,ncol=len+1))

for ( m in 1:length(proj_tmax_file)){
  tmx=tmax(proj_tmax_file[m])
  ne=colnames(tmx)
  colnames(proj_tmaximum)[]=ne
  proj_tmaximum=rbind(proj_tmaximum,tmx)
  tmn=tmin(proj_tmin_file[m])
  nen=colnames(tmn)
  colnames(proj_tminimum)[]=nen
  proj_tminimum=rbind(proj_tminimum,tmn)
  
}



proj_tmaximum1=as.matrix((proj_tmaximum[-1,1:len]))
proj_tminimum1=as.matrix((proj_tminimum[-1,1:len]))

tmax_rcp_all=cbind(proj_tmaximum1[1:12783,],proj_tmaximum1[12784:25566,],proj_tmaximum[2:12784,len+1])
tmin_rcp_all=cbind(proj_tminimum1[1:12783,],proj_tminimum1[12784:25566,],proj_tminimum[2:12784,len+1])



###Order of data
#rcp 26/....................rcp26 rcp45.....................rcp45 rcp60...............rcp60 rcp85..................rcp85
tmax_rcp_final=rbind(tmax_rcp_all[match_precp1,],tmax_rcp_all[match_precp2,],tmax_rcp_all[match_precp3,])
tmin_rcp_final=rbind(tmin_rcp_all[match_precp1,],tmin_rcp_all[match_precp2,],tmin_rcp_all[match_precp3,])

#########BIAS CORRECTION FOR TEMPERATURE#############

proj_tmax1=cbind(proj_dff1,tmax_rcp_final[1:length(dat_1),-ex])
proj_tmax2=cbind(proj_dff2,tmax_rcp_final[(1+length(dat_1)):(length(dat_2)+length(dat_1)),-ex])
proj_tmax3=cbind(proj_dff3,tmax_rcp_final[(1+length(dat_2)+length(dat_1)):(length(dat_2)+length(dat_1)+length(dat_3)),-ex])

proj_tmin1=cbind(proj_dff1,tmin_rcp_final[1:length(dat_1),-ex])
proj_tmin2=cbind(proj_dff2,tmin_rcp_final[(1+length(dat_1)):(length(dat_2)+length(dat_1)),-ex])
proj_tmin3=cbind(proj_dff3,tmin_rcp_final[(1+length(dat_2)+length(dat_1)):(length(dat_2)+length(dat_1)+length(dat_3)),-ex])


proj_tmax1=data.frame(proj_tmax1)
proj_tmax2=data.frame(proj_tmax2)
proj_tmax3=data.frame(proj_tmax3)

proj_tmin1=data.frame(proj_tmin1)
proj_tmin2=data.frame(proj_tmin2)
proj_tmin3=data.frame(proj_tmin3)

proj_bc_cu_tmax=proj_tmax1 ##2006-2012
proj_bc_cu_tmin=proj_tmin1 ##2006-2012

proj_bc_fu_tmax=rbind(proj_tmax2,proj_tmax3) 
proj_bc_fu_tmin=rbind(proj_tmin2,proj_tmin3) 

tmax_obs=data.frame(tmax_obs)
tmax_DAY=tmax_obs
tmin_obs=data.frame(tmin_obs)
tmin_DAY=tmin_obs
tdew_obs=data.frame(tdew_obs)
tdew_DAY=tdew_obs
rh_obs=data.frame(rh_DAY)

###########all data######################

obs_tmax=tmax_DAY
obs_tmin=tmin_DAY
obs_tdew=tdew_DAY
future_tmax=proj_bc_fu_tmax
future_tmin=proj_bc_fu_tmin
future_tdew=dewT_fu


###format time#########

tmax_DAY=data.frame(rep(obs_tmax,2))
tmin_DAY=data.frame(rep(obs_tmin,2))
tdew_DAY=data.frame(rep(obs_tdew,2))
rh_DAY=data.frame(rep(rh_DAY,2))


mon=c(1,2,3,4,5,6,7,8,9,10,11,12)

kk=proj_bc_fu

for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
    
    id=which(proj_bc_cu_tmax[,2]==mon[i])
    obs=tmax_DAY[id,m]
    sim=proj_bc_cu_tmax[id,m+3]
    proj=proj_bc_fu_tmax[id,m+3]
    
    h1=sort(sim)
    h2=sort(obs)
    h3=sort(proj)
    
    rain_qm.fit1 <- fitQmapRQUANT(h1,h3,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr1 <- doQmapRQUANT(proj,rain_qm.fit1,type="linear")
    
    rain_qm.fit2 <- fitQmapRQUANT(h2,h1,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr2<- doQmapRQUANT(his_corr1,rain_qm.fit2,type="linear")
    
    
    proj_bc_fu_tmax[id,m+3]=his_corr2
    
    
    
    
    obsmn=tmin_DAY[id,m]
    simmn=proj_bc_cu_tmin[id,m+3]
    projmn=proj_bc_fu_tmin[id,m+3]
    
    h1mn=sort(simmn)
    h2mn=sort(obsmn)
    h3mn=sort(projmn)
    
    rain_qm.fit1mn <- fitQmapRQUANT(h1mn,h3mn,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr1mn <- doQmapRQUANT(projmn,rain_qm.fit1mn,type="linear")
    
    rain_qm.fit2mn <- fitQmapRQUANT(h2mn,h1mn,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr2mn<- doQmapRQUANT(his_corr1mn,rain_qm.fit2mn,type="linear")
   proj_bc_fu_tmin[id,m+3]=his_corr2mn
    
    
  }
  
  
  print(i)
}






















































































#############make seperate correction for dewpoint temperature###################

###processing All humiditydata#########
proj_rh_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*hur_day_",Model,"_rcp",sep=""))
##get index of the nearest lat,lon of the grid ###
rh.nc = nc_open(proj_rh_file[1])
rh_latall=ncvar_get(rh.nc,'lat')
rh_lonall=ncvar_get(rh.nc,'lon')
len=length(lon_ws)
rh_lon_ID=matrix(NA,nrow=len,ncol=1)
rh_lat_ID=matrix(NA,nrow=len,ncol=1)
for ( i in 1:len){
  rh_lon_ID[i,1]=which.min(abs(rh_lonall-lon_ws[i])) 
  rh_lat_ID[i,1]=which.min(abs(rh_latall-lat_ws[i])) 
}
rh_lat_lon_ID=cbind(rh_lon_ID,rh_lat_ID)
rh_ID=unique(rh_lat_lon_ID)

##create time series for the data first

hur <- function(file ){
  
  hur.nc = nc_open(file)
  hur_time=as.Date(ncvar_get(hur.nc,'time'),origin="2005-01-01")
  #strDates= as.character(hur_time)
  rh=matrix(NA,nrow=length(hur_time),ncol=1)
  for( i in 1:1){
    start=c(rh_ID[i,1],rh_ID[i,2],3,1)
    count=c(1,1,1,length(hur_time))
    rh[,i]=ncvar_get(hur.nc,'hur',start,count)
  }
  
  rhdata=data.frame(data=cbind(rh, hur_time-0.5))
  return(rhdata)
}


proj_rhumd=data.frame(matrix(NA,nrow=1,ncol=1+1))
for ( m in 1:length(proj_rh_file)){
  rd=hur(proj_rh_file[m])
  ne=colnames(rd)
  colnames(proj_rhumd)[]=ne
  proj_rhumd=rbind(proj_rhumd,rd)
}


########USING ONLY ONE RH DATA FOR SIMPLICITY############################

proj_rhumd=proj_rhumd[-1,]
rh_rcp_all=cbind(proj_rhumd[1:12775,1],proj_rhumd[12776:25550,1],proj_rhumd[1:12775,2]) ##as no data for 2.6 so use only for 4.5
match_rh1=match(dat_1,rh_rcp_all[,3])
match_rh2=match(dat_2,rh_rcp_all[,3])
match_rh3=match(dat_3,rh_rcp_all[,3])

rh_rcp_match=rbind(rh_rcp_all[match_rh1,],rh_rcp_all[match_rh2,],rh_rcp_all[match_rh2,])                 ##calculating tdew from relative humidty and average temperature#############

tavg_fu=0.5*(proj_bc_fu_tmax[,(4:(rcp*ng+3))]+proj_bc_fu_tmin[,(4:(rcp*ng+3))])
tavg_cu=0.5*(proj_bc_cu_tmax[,(4:(rcp*ng+3))]+proj_bc_cu_tmin[,(4:(rcp*ng+3))])

RH=rh_rcp_match[1:9862,1:2]
RH[RH>100]=100
RH_ng_cu=cbind(matrix(rep(RH[1:2557,1],ng),nrow=length(dat_1),ncol=ng),matrix(rep(RH[1:2557,2],ng),nrow=length(dat_1),ncol=ng))
RH_ng_fu=cbind(matrix(rep(RH[2558:9862,1],ng),nrow=7305,ncol=ng),matrix(rep(RH[2558:9862,2],ng),nrow=7305,ncol=ng))

dewT_cu=243.04*(log(RH_ng_cu/100)+((17.625*tavg_cu)/(243.04+tavg_cu)))/(17.625-log(RH_ng_cu/100)-((17.625*tavg_cu)/(243.04+tavg_cu)))
dewT_fu=243.04*(log(RH_ng_fu/100)+((17.625*tavg_fu)/(243.04+tavg_fu)))/(17.625-log(RH_ng_fu/100)-((17.625*tavg_fu)/(243.04+tavg_fu)))
dewT_fu2=243.04*(log(kk/100)+((17.625*tavg_fu)/(243.04+tavg_fu)))/(17.625-log(kk/100)-((17.625*tavg_fu)/(243.04+tavg_fu)))

rh_DAY=data.frame((RH_d[date_overlap_day,]))
rh_DAY=data.frame(rep(rh_DAY,2))

mon=c(1,2,3,4,5,6,7,8,9,10,11,12)

kk=dewT_fu

for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
    
    id=which(proj_bc_cu[,2]==mon[i])
    obs=tdew_DAY[id,m]
    sim=dewT_cu[id,m]
    proj=dewT_fu[id,m]
    
    h1=sort(sim)
    h2=sort(obs)
    h3=sort(proj)
    
    rain_qm.fit1 <- fitQmapRQUANT(h1,h3,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr1 <- doQmapRQUANT(proj,rain_qm.fit1,type="tricub")
    
    rain_qm.fit2 <- fitQmapRQUANT(h2,h1,qstep=0.1,nboot=10,wet.day=FALSE)
    his_corr2<- doQmapRQUANT(his_corr1,rain_qm.fit2,type="tricub")
    
    
    dewT_fu[id,m]=his_corr2
    
    
    
   
    
  }
  
  
  print(i)
}






















###temperature analysis######


tmax_base=(tmaxtmintdew_d[date_overlap_base,seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_base=(tmaxtmintdew_d[date_overlap_base,seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_base=(tmaxtmintdew_d[date_overlap_base,seq(4,dim(tmaxtmintdew_d)[2],3)])

tmax_dm_mon<- data.frame(monthlyfunction(tmax_base, FUN=mean, na.rm=TRUE,dates=dates_base))
tmin_dm_mon<- data.frame(monthlyfunction(tmin_base, FUN=mean, na.rm=TRUE,dates=dates_base))
tdew_dm_mon<- data.frame(monthlyfunction(tdew_base, FUN=mean, na.rm=TRUE,dates=dates_base))


tmax_bcca_mon_corr_56_65<- data.frame(monthlyfunction(proj_bc_fu_tmax[1:3653,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tmax_bcca_mon_corr_90_99<- data.frame(monthlyfunction(proj_bc_fu_tmax[3654:7305,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_3))


tmin_bcca_mon_corr_56_65<- data.frame(monthlyfunction(proj_bc_fu_tmin[1:3653,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tmin_bcca_mon_corr_90_99<- data.frame(monthlyfunction(proj_bc_fu_tmin[3654:7305,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_3))

tdew_bcca_mon_corr_56_65<- data.frame(monthlyfunction(dewT_fu[1:3653,seq(1,rcp*ng,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tdew_bcca_mon_corr_90_99<- data.frame(monthlyfunction(dewT_fu[3654:7305,seq(1,rcp*ng,1)], FUN=mean, na.rm=TRUE,dates=dat_3))

plot(colMeans(tdew_dm_mon),ylim=c(-5,20))
lines(colMeans(tdew_bcca_mon_corr_56_65[1:10,]))

plot(colMeans(tmax_dm_mon),ylim=c(-5,27))
lines(colMeans(tmax_bcca_mon_corr_56_65[1:10,]))

plot(colMeans(tmin_dm_mon),ylim=c(-5,27))
lines(colMeans(tmin_bcca_mon_corr_56_65[1:10,]))


plot(colMeans(tdew_dm_mon),ylim=c(-5,20))
lines(colMeans(tdew_bcca_mon_corr_56_65[11:20,]))

plot(colMeans(tmax_dm_mon),ylim=c(-5,27))
lines(colMeans(tmax_bcca_mon_corr_56_65[11:20,]))

plot(colMeans(tmin_dm_mon),ylim=c(-5,27))
lines(colMeans(tmin_bcca_mon_corr_56_65[11:20,]))





################TEMPERATUE ANALYSIS COMPARE TO BASELINE PERIOD##########

#


par(mfrow=c(2,2))
barplot(colMeans(tmax_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),]),ylab="Monthly Rainfall(mm)", main="RCP2.6")
barplot(colMeans(tmax_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),]),main="RCP4.5")
barplot((colMeans(tmax_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),])-colMeans(tmax_dm_mon)),ylab="Rainfall difference(mm)",main="RCP2.6")
barplot(colMeans(tmax_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),])-colMeans(tmax_dm_mon),main="RCP4.5")


par(mfrow=c(2,2))
barplot((0.5*(colMeans(tmax_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),])+colMeans(tmin_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),]))),ylab=expression(paste("Avg Temperature " ^ 0,"C",sep="")),main="RCP4.5",ylim=c(-5,25))
barplot((0.5*(colMeans(tmax_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),])+colMeans(tmin_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),]))),main="RCP8.5",ylim=c(-5,25))



avgT_56_65_4=(0.5*(colMeans(tmax_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),])+colMeans(tmin_bcca_mon_corr_56_65[(ng*1-ng+1):(ng*1),])))
avgT_56_65_8=(0.5*(colMeans(tmax_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),])+colMeans(tmin_bcca_mon_corr_56_65[(ng*2-ng+1):(ng*2),])))

avgT_90_99_4=(0.5*(colMeans(tmax_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),])+colMeans(tmin_bcca_mon_corr_90_99[(ng*1-ng+1):(ng*1),])))
avgT_90_99_8=(0.5*(colMeans(tmax_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),])+colMeans(tmin_bcca_mon_corr_90_99[(ng*2-ng+1):(ng*2),])))


avgT_dm=0.5*(colMeans(tmax_dm_mon)+colMeans(tmin_dm_mon))


par(mfrow=c(2,2))
barplot((avgT_56_65_4-avgT_dm),ylab="Monthly Rainfall(mm)", main="RCP4.5")
barplot((avgT_56_65_8-avgT_dm),ylab="Monthly Rainfall(mm)", main="RCP8.5")
barplot((avgT_90_99_4-avgT_dm),ylab="Monthly Rainfall(mm)", main="RCP4.5")
barplot((avgT_90_99_8-avgT_dm),ylab="Monthly Rainfall(mm)", main="RCP8.5")



tmaxtmintdew=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:(rcp*len)){
  tmaxtmintdew=cbind(tmaxtmintdew,proj_bc_fu_tmax[,k+3],proj_bc_fu_tmin[,k+3],dewT_fu[,k]+4)
}

temper=data.frame(tmaxtmintdew[,-1])

nc=ng
for ( j in 1:rcp){
  
  temperpp=data.frame(temper[,seq((nc*j*3-nc*3+1),(nc*j*3),1)])
  temperpp[] <- lapply(temperpp, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  temperpp=data.frame(temperpp,day,hour)
  
  
  ##writing rain.dat file
  file=paste("tmaxtmintdew_",rcp_name[j],".dat",sep="")
  sink(file) 
  cat(sprintf("This file provides daily values of Tmax/Tmin/Tdew for each temperature station"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  cat(sprintf("Temperature is provided in degrees Celcius"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  sites=seq(1,(nc),1)
  cat(sprintf("%s %d ", "ver2",nc),file=file,append=TRUE) 
  cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  write.table(temperpp, file =file,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  sink()
  
}


#########creating clipar.dat for different rcp###################



proj_station_monthly_56_65<- data.frame(monthlyfunction(tavg[1:3653,], FUN=mean, na.rm=TRUE,dates=dat_1))
proj_station_monthly_90_99<- data.frame(monthlyfunction(tavg[3654:7305,], FUN=mean, na.rm=TRUE,dates=dat_2))






#########cahnge year 56 and 65 or 90 o 99
for( j in 1:rcp){
  sta_ID=seq(1,nc,1)
  sta_ele=rep(-999,nc)
  prj_station_monthly=cbind(seq(1,nc,1),lat_lon_rg1[,2],lat_lon_rg1[,1],sta_ele,proj_station_monthly_90_99[seq((ng*j-ng+1),(ng*j),1),])
  
  
  prj_station_monthly[] <- lapply(prj_station_monthly, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  
  #writng clipar.dat
  file=paste("clipar_90_99",rcp_name[j],".dat",sep="")
  sink("clipar.dat") 
  cat(sprintf("This file provides meta-data for each temperature station"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  cat(sprintf("Temperature ranges are in Degrees Celcius"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  sites=seq(1,(nc),1)
  cat(sprintf("%d %s ", nc,"! Number of temperature stations"),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  ##please change standard longitude according to watershed location
  std_longitude=-120.00;
  cat(sprintf("%3.2f %s ",std_longitude,"!Standard Longitude of time zone used in local time calculations"),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  cat(sprintf("'Station_id  Lat    Lon     Elev_m   Jan    Feb    Mar     Apr       May    Jun    Jul      Aug     Sep    Oct      Nov    Dec"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  write.table(prj_station_monthly, file =file,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  sink()
  
}





#####################PROCESSING WIND DATA AND NO BIAS CORRECTION#################################



ua_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*ua_day_",Model,"_rcp",sep=""))
va_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*va_day_",Model,"_rcp",sep=""))



##get index of the nearest lat,lon of the grid ###
ua.nc = nc_open(ua_file[1])
ua_latall=ncvar_get(ua.nc,'lat')
ua_lonall=ncvar_get(ua.nc,'lon')
len=length(lon_ws)
ua_lon_ID=matrix(NA,nrow=len,ncol=1)
ua_lat_ID=matrix(NA,nrow=len,ncol=1)
for ( i in 1:len){
  ua_lon_ID[i,1]=which.min(abs(ua_lonall-lon_ws[i])) 
  ua_lat_ID[i,1]=which.min(abs(ua_latall-lat_ws[i])) 
}
ua_lat_lon_ID=cbind(ua_lon_ID,ua_lat_ID)
ua_ID=unique(ua_lat_lon_ID)



##get index of the nearest lat,lon of the
uawind <- function(file ){
  
  ua.nc = nc_open(file)
  ua_time=as.Date(ncvar_get(ua.nc,'time'),origin="2005-01-01")
  #strDates= as.character(hur_time)
  #ua=matrix(NA,nrow=length(ua_time),ncol=length(ua_ID[,1]))
  ua=matrix(NA,nrow=length(ua_time),ncol=1)
  
  for( i in 1:1){
    start=c(ua_ID[i,1],ua_ID[i,2],3,1)
    count=c(1,1,1,length(ua_time))
    ua[,i]=ncvar_get(ua.nc,'ua',start,count)
  }
  uadata=data.frame(data=cbind(ua, ua_time-0.5))
  return(uadata)
}




uad=data.frame(matrix(NA,nrow=1,ncol=1+1))

for ( m in 1:length(ua_file)){
  ud=uawind(ua_file[m])
  ne=colnames(ud)
  colnames(uad)[]=ne
  uad=rbind(uad,ud)
}


##get index of the nearest lat,lon of the grid ###
va.nc = nc_open(va_file[1])
va_latall=ncvar_get(va.nc,'lat')
va_lonall=ncvar_get(va.nc,'lon')
len=length(lon_ws)
va_lon_ID=matrix(NA,nrow=len,ncol=1)
va_lat_ID=matrix(NA,nrow=len,ncol=1)
for ( i in 1:len){
  va_lon_ID[i,1]=which.min(abs(va_lonall-lon_ws[i])) 
  va_lat_ID[i,1]=which.min(abs(va_latall-lat_ws[i])) 
}
va_lat_lon_ID=cbind(va_lon_ID,va_lat_ID)
va_ID=unique(va_lat_lon_ID)

##get index of the nearest lat,lon of the
vawind <- function(file ){
  
  va.nc = nc_open(file)
  va_time=as.Date(ncvar_get(va.nc,'time'),origin="2005-01-01")
  #strDates= as.character(hur_time)
  va=matrix(NA,nrow=length(va_time),ncol=1)
  for( i in 1:1){
    start=c(va_ID[i,1],va_ID[i,2],3,1)
    count=c(1,1,1,length(va_time))
    va[,i]=ncvar_get(va.nc,'va',start,count)
  }
  vadata=data.frame(data=cbind(va, va_time-0.5))
  return(vadata)
}

###it seems that file based on th rcp###
vad=data.frame(matrix(NA,nrow=1,ncol=1+1))
for ( m in 1:length(va_file)){
  vd=vawind(va_file[m])
  ne=colnames(vd)
  colnames(vad)[]=ne
  vad=rbind(vad,vd)
}

all_wind=sqrt(((uad[,1]))^2+((vad[,1]))^2)
all_wind=data.frame(wind=all_wind)

wind_data=cbind(all_wind[2:9491,],all_wind[9492:18981,],uad[2:9491,2])

match_w1=match(dat_1,wind_data[,3])
match_w2=match(dat_2,wind_data[,3])
wind_data=data.frame(wind_data)
wind_rcp_data=rbind(wind_data[match_w1,],wind_data[match_w2,])

##convert at 2m height
##we took wind speed at 85000 pa level that means 1.41km avove the ground

##Reference

##But we need wind speed at 2m avpve ground

##equation taken form http://www.fao.org/docrep/x0490e/x0490e07.htm
wind_rcp_data_mod=wind_rcp_data[,1:2]*4.87/(log(67.8*3.6*1000-5.42)) ##1.2km*(100000-850000)=3.6km=3.6*1000 m


############WRITING WIND DATA###################




rcp=2
rcp_name_w=c("rcp4.5","rcp8.5")
for (j in 1:rcp){
  wind=data.frame(wind_rcp_data_mod[,j])
  wind[] <- lapply(wind, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
  wind=data.frame(wind,day,hour)
  file=paste("wind_",rcp_name_w[j],".dat",sep="")
  sink(file)
  cat(sprintf("This file provides daily values of wind speed for each wind station"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  cat(sprintf("Wind speed is provided in m/sec"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  sites=seq(1,1,1) ## need to automate this one
  cat(sprintf("%s %d ", "ver2",1),file=file,append=TRUE) 
  cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  write.table(wind, file =file,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  sink()
}




####processing rainweight file #############
####change the directory############

setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C1_watershed_final\\C1_CC_data")
system ("rainweight  -w demC1me.tif     -rg rg_C1.shp -ar annrain.tif  -tri triout.tif -wt weights.txt")   


##write rain weight according to modelspc format
rw= read.table("weights.txt", sep=",", col.names=c("Basiin", "gauge","weight"), fill=FALSE, strip.white=FALSE)
rw=rw[-1,]
rw_frame=data.frame(rw)
rw_uniq=as.numeric(as.character((unique(rw_frame$Basiin))))
basin_par= as.matrix(read.table("nodelinks.txt", sep=",", col.names=c('NodeId', 'DownNodeId', 'DrainId', 'ProjNodeId', 'DOutFlag', 'ReachId', 'Area', 'AreaTotal', 'X', 'Y'), fill=FALSE, strip.white=FALSE))
basin_par=basin_par[-1,]
drain_ID=as.numeric(basin_par[,3])
sink("rainweights.txt") 
cat(sprintf("Subcatchment - raingauge relationship"),file='rainweights.txt',append=TRUE)
cat("\n", file="rainweights.txt", append=TRUE)
for (i in 1:length(drain_ID)){
  tt=which(drain_ID[i]==rw_frame$Basiin, arr.ind = TRUE)
  k=as.numeric(length(tt))
  s=as.numeric(as.character(rw_frame$weight[tt]))
  g=as.numeric(as.character(rw_frame$gauge[tt]))
  cat(sprintf( "%d", i),(sprintf( "%d", k*(-1))),(sprintf( " %d % 1.6f", g,s)),file='rainweights.txt',append=TRUE)
  cat("\n", file="rainweights.txt", append=TRUE)
}

sink()



###########write bndryflow.dat################


###writing boundy flow 

bndry_flow=data.frame(x=rep(-999,nrow(day)))
bndry_flow[] <- lapply(bndry_flow, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
bndry_flow=data.matrix(bndry_flow)
bndry_flow=data.frame(bndry_flow,day,hour)

sink('bndryflow.dat')
cat(sprintf("This file provides mean daily values of streamflow"),file='bndryflow.dat',append=TRUE)
cat("\n", file="bndryflow.dat", append=TRUE)
cat(sprintf("Flow values are provided in m3/sec"),file='bndryflow.dat',append=TRUE)
cat("\n", file="bndryflow.dat", append=TRUE)
sites=seq(1,1,1)
cat(sprintf("%s %d ", "ver2",1),file='bndryflow.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='bndryflow.dat',append=TRUE) 
cat("\n", file="bndryflow.dat", append=TRUE)
write.table(bndry_flow, file = "bndryflow.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()












