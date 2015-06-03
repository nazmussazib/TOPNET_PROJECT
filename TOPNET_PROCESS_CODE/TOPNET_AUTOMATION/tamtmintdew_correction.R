###########tmaxtmintdewcorrection#############
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



#####processiing temperature  data #############

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))

toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}

dff=t(matrix(unlist(lapply(dates_bc, toNumerics)),nrow=3,ncol=length(dates_bc)))



############Analysis of Rainfall Results compare to base period 2003-2012 ##########

dates_day=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") 
dates_base=seq(as.Date("2003/1/1"), as.Date("2012/12/31"), "day") ##base line period for 2003-2012

date_overlap_base=match(dates_base,dates_day) 



##################TEMPERATURE DATA PROCESSING#########################

dates_obs=seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day") 
date_overlap_obs=match(dates_obs,dates_day)
tmax_obs=(tmaxtmintdew_d[date_overlap_obs,seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_obs=(tmaxtmintdew_d[date_overlap_obs,seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_obs=(tmaxtmintdew_d[date_overlap_obs,seq(4,dim(tmaxtmintdew_d)[2],3)])


dates_obs1=seq(as.Date("2006/1/1"), as.Date("2012/12/31"), "day") 
date_overlap_obs1=match(dates_obs1,dates_day)
tmax_obs1=(tmaxtmintdew_d[date_overlap_obs1,seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_obs1=(tmaxtmintdew_d[date_overlap_obs1,seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_obs1=(tmaxtmintdew_d[date_overlap_obs1,seq(4,dim(tmaxtmintdew_d)[2],3)])


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

hist_tmax_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmax_day_",Model,"_historical_",sep=""))
hist_tmin_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmin_day_",Model,"_historical_",sep=""))

proj_tmax_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmax_day_",Model,"_rcp",sep=""))
proj_tmin_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_tasmin_day_",Model,"_rcp",sep=""))

his_tmaximum=data.frame(matrix(NA,nrow=1,ncol=len+1))
his_tminimum=data.frame(matrix(NA,nrow=1,ncol=len+1))
proj_tmaximum=data.frame(matrix(NA,nrow=1,ncol=len+1))
proj_tminimum=data.frame(matrix(NA,nrow=1,ncol=len+1))



for ( m in 1:length(hist_tmax_file)){
  tmx=tmax(hist_tmax_file[m])
  ne=colnames(tmx)
  colnames(his_tmaximum)[]=ne
  his_tmaximum=rbind(his_tmaximum,tmx)
  tmn=tmin(hist_tmin_file[m])
  nen=colnames(tmn)
  colnames(his_tminimum)[]=nen
  his_tminimum=rbind(his_tminimum,tmn)
  
}


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


his_tmaximum1=as.matrix((his_tmaximum[-1,1:len]))
his_tminimum1=as.matrix((his_tminimum[-1,1:len]))
proj_tmaximum1=as.matrix((proj_tmaximum[-1,1:len]))
proj_tminimum1=as.matrix((proj_tminimum[-1,1:len]))




tmax_rcp_all=cbind(proj_tmaximum1[1:12783,],proj_tmaximum1[12784:25566,],proj_tmaximum[2:12784,len+1])
tmin_rcp_all=cbind(proj_tminimum1[1:12783,],proj_tminimum1[12784:25566,],proj_tminimum[2:12784,len+1])

rcp=2

dat_1=seq(as.Date("2006/1/1"), as.Date("2012/12/31"), "day")
dat_2=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")
dat_3=seq(as.Date("2090/1/1"), as.Date("2099/12/31"), "day")
proj_dff1=t(matrix(unlist(lapply(dat_1, toNumerics)),nrow=3,ncol=length(dat_1)))
proj_dff2=t(matrix(unlist(lapply(dat_2, toNumerics)),nrow=3,ncol=length(dat_2)))
proj_dff3=t(matrix(unlist(lapply(dat_3, toNumerics)),nrow=3,ncol=length(dat_3)))

match_tmax1=match(dat_1,tmax_rcp_all[,rcp*len+1])
match_tmax2=match(dat_2,tmax_rcp_all[,rcp*len+1])
match_tmax3=match(dat_3,tmax_rcp_all[,rcp*len+1])

###Order of data
#rcp 26/....................rcp26 rcp45.....................rcp45 rcp60...............rcp60 rcp85..................rcp85
tmax_rcp_final=rbind(tmax_rcp_all[match_tmax1,],tmax_rcp_all[match_tmax2,],tmax_rcp_all[match_tmax3,])
tmin_rcp_final=rbind(tmin_rcp_all[match_tmax1,],tmin_rcp_all[match_tmax2,],tmin_rcp_all[match_tmax3,])

#########BIAS CORRECTION FOR TEMPERATURE#############
ex=rcp*len+1
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



######temperature corection############

obs1_tmax=data.frame(tmax_obs1)
obs1_tmin=data.frame(tmin_obs1)
tmax_DAY1=data.frame(rep(obs1_tmax,2))
tmin_DAY1=data.frame(rep(obs1_tmin,2))
dtr_obs=tmax_DAY1-tmin_DAY1
dtr_sim=proj_bc_cu_tmax-proj_bc_cu_tmin
dtr_proj=proj_bc_fu_tmax-proj_bc_fu_tmin
dtr_proj1=dtr_proj[1:3653,]
dtr_proj2=dtr_proj[3654:7305,]



mon=c(1,2,3,4,5,6,7,8,9,10,11,12)
ng=len
for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
 
    
    id=which(proj_bc_cu_tmax[,2]==mon[i])
    obs=tmax_DAY1[id,m]
    sim=proj_bc_cu_tmax[id,m+3]
    id_proj1=which(proj_bc_fu_tmax[1:3653,2]==mon[i])
    id_proj2=which(proj_bc_fu_tmax[3654:7305,2]==mon[i])
    proj1=proj_tmax2[id_proj1,m+3]
    proj2=proj_tmax3[id_proj2,m+3]
    
    a1_1=mean(proj1)
    a1_2=mean(proj2)
    a2=mean(obs)
    a3=mean(sim)
    #a2=a2+0.5*(a1_1+a1_2)-a3
    
    b1_1=sd(proj1)
    b1_2=sd(proj2)
    b2=sd(obs)
    b3=sd(sim)
    #b2=b2*0.5*(b1_1+b1_2)/b3
    
    yu1=pnorm(proj1,a1_1,b1_1)
    yu2=pnorm(proj2,a1_2,b1_2)
    
    su1=qnorm(yu1,a2,b2)
    su2=qnorm(yu2,a2,b2)
    
    ju1=qnorm(yu1,a3,b3)
    ju2=qnorm(yu2,a3,b3)
    
    proj_corr1=proj1+su1-ju1
    proj_corr2=proj2+su2-ju2
    
    proj_tmax2[id_proj1,m+3]=proj_corr1
    proj_tmax3[id_proj2,m+3]=proj_corr2
    
   
    
    
   
    obsdr=dtr_obs[id,m]
    simdr=dtr_sim[id,m+3]
   
    dtr1=dtr_proj1[id_proj1,m+3]
   dtr2=dtr_proj2[id_proj2,m+3]
    
    a1_1_dr=mean(dtr1)
    a1_2_dr=mean(dtr2)
    a2_dr=mean(obsdr)
    a3_dr=mean(simdr)
    #a2_dr=a2_dr+0.5*(a1_1_dr+a1_2_dr)-a3_dr
    
    b1_1_dr=sd(dtr1)
    b1_2_dr=sd(dtr2)
    b2_dr=sd(obsdr)
    b3_dr=sd(simdr)
    #b2_dr=b2_dr*0.5*(b1_1_dr+b1_2_dr)/b3_dr
    
    yu1_dr=pnorm(dtr1,a1_1_dr,b1_1_dr)
    yu2_dr=pnorm(dtr2,a1_2_dr,b1_2_dr)
    
    su1_dr=qnorm(yu1_dr,a2_dr,b2_dr)
    su2_dr=qnorm(yu2_dr,a2_dr,b2_dr)
    
     ju1_dr=qnorm(yu1_dr,a3_dr,b3_dr)
    ju2_dr=qnorm(yu2_dr,a3_dr,b3_dr)
     
     proj_corr1_dr= dtr1+su1_dr-ju1_dr
     proj_corr2_dr= dtr2+su2_dr-ju2_dr
     
    dtr_proj1[id_proj1,m+3]=proj_corr1_dr
    dtr_proj2[id_proj2,m+3]=proj_corr2_dr
    
    
  }
  
  
  print(i)
}

tmax_bc_proj=rbind(proj_tmax2,proj_tmax3)
dtr_bc_proj=rbind(dtr_proj1,dtr_proj2)
tmin_bc_proj=tmax_bc_proj-dtr_bc_proj





#######lets test with based on the relation ship between tdew and tmin #############


plot(tdew_obs1[,1],tmin_obs1[,1])

obs1_tdew=data.frame(tdew_obs1)
tdew_DAY1=data.frame(rep(obs1_tdew,2))


mon=c(1,2,3,4,5,6,7,8,9,10,11,12)
ng=len
for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
    
    
    id=which(proj_bc_cu_tmax[,2]==mon[i])
    obs=tdew_DAY1[id,m]
    sim=tmin_DAY1[id,m]
    
    ll=lm(sim~obs)
    a1=ll$coefficients[1]
    a2=ll$coefficients[2]
    id_proj1=which(proj_bc_fu_tmax[1:3653,2]==mon[i])
    id_proj2=which(proj_bc_fu_tmax[3654:7305,2]==mon[i])
    proj1=tdew_proj[id_proj1,m+3]
    proj2=tdew_proj[id_proj2,m+3]
 
    
#     a1_1=mean(proj1)
#     a1_2=mean(proj2)
#     a2=mean(obs)
#     a3=mean(sim)
#     #a2=a2+0.5*(a1_1+a1_2)-a3
#     
#     b1_1=sd(proj1)
#     b1_2=sd(proj2)
#     b2=sd(obs)
#     b3=sd(sim)
#     #b2=b2*0.5*(b1_1+b1_2)/b3
#     
#     yu1=pnorm(proj1,a1_1,b1_1)
#     yu2=pnorm(proj2,a1_2,b1_2)
#     
#     su1=qnorm(yu1,a3,b3)
#     su2=qnorm(yu2,a3,b3)
#    
#     ku1=pnorm(su1,mean(su1),sd(su1))
#     ku2=pnorm(su2,mean(su2),sd(su2))
#     
#     ju1=qnorm(ku1,a2,b2)
#     ju2=qnorm(ku2,a2,b2)
#     
   # proj_corr1=ju1
  #  proj_corr2=ju2


      proj_corr1=proj1*a2+a1
      proj_corr2=proj2*a2+a1
    tdew_proj[id_proj1,m+3]=proj_corr1
    tdew_proj[id_proj2,m+3]=proj_corr2
    
    
    
    
    
    
  }
  
  
  print(i)
}















###processing All humiditydata#########
proj_rh_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*hur_day_",Model,"_rcp",sep=""))
hist_rh_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*hur_day_",Model,"_historical_",sep=""))

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


his_hur <- function(file ){
  
  hur.nc = nc_open(file)
  hur_time=as.Date(ncvar_get(hur.nc,'time'),origin="1850-01-01")
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



his_rhumd=data.frame(matrix(NA,nrow=1,ncol=1+1))
for ( m in 1:length(hist_rh_file)){
  rd=his_hur(hist_rh_file[m])
  ne=colnames(rd)
  colnames(his_rhumd)[]=ne
  his_rhumd=rbind(his_rhumd,rd)
}


proj_rhumd=data.frame(matrix(NA,nrow=1,ncol=1+1))
for ( m in 1:length(proj_rh_file)){
  rd=hur(proj_rh_file[m])
  ne=colnames(rd)
  colnames(proj_rhumd)[]=ne
  proj_rhumd=rbind(proj_rhumd,rd)
}


########USING ONLY ONE RH DATA FOR SIMPLICITY############################
his_rhumd=his_rhumd[-1,]
match_his=match(dates_obs,his_rhumd[,2])
hist_rh=his_rhumd[match_his,1]
no_data=which(is.na(hist_rh))
hist_rh[no_data]=mean(hist_rh,na.rm=TRUE)


proj_rhumd=proj_rhumd[-1,]
rh_rcp_all=cbind(proj_rhumd[1:12775,1],proj_rhumd[12776:25550,1],proj_rhumd[1:12775,2]) ##as no data for 2.6 so use only for 4.5
match_rh1=match(dat_1,rh_rcp_all[,3])
match_rh2=match(dat_2,rh_rcp_all[,3])
match_rh3=match(dat_3,rh_rcp_all[,3])

rh_rcp_match=rbind(rh_rcp_all[match_rh1,],rh_rcp_all[match_rh2,],rh_rcp_all[match_rh2,])                 ##calculating tdew from relative humidty and average temperature#############

tavg_fu=0.5*(proj_bc_fu_tmax[,(4:(rcp*ng+3))]+tmin[,(4:(rcp*ng+3))])
tavg_cu=0.5*(proj_bc_cu_tmax[,(4:(rcp*ng+3))]+proj_bc_cu_tmin[,(4:(rcp*ng+3))])

RH=rh_rcp_match[1:9862,1:2]
RH[RH>100]=100
RH_his=hist_rh
RH_ng_his=cbind(matrix(rep(RH_his,ng),nrow=length(dates_obs),ncol=ng))
RH_ng_cu=cbind(matrix(rep(RH[1:2557,1],ng),nrow=length(dat_1),ncol=ng),matrix(rep(RH[1:2557,2],ng),nrow=length(dat_1),ncol=ng))
RH_ng_fu=cbind(matrix(rep(RH[2558:9862,1],ng),nrow=7305,ncol=ng),matrix(rep(RH[2558:9862,2],ng),nrow=7305,ncol=ng))

tavg_his=0.5*(his_tmaximum1+his_tminimum1)
dewT_his=243.04*(log(RH_ng_his/100)+((17.625*tavg_his)/(243.04+tavg_his)))/(17.625-log(RH_ng_his/100)-((17.625*tavg_his)/(243.04+tavg_his)))
dewT_cu=243.04*(log(RH_ng_cu/100)+((17.625*tavg_cu)/(243.04+tavg_cu)))/(17.625-log(RH_ng_cu/100)-((17.625*tavg_cu)/(243.04+tavg_cu)))
dewT_fu=243.04*(log(RH_ng_fu/100)+((17.625*tavg_fu)/(243.04+tavg_fu)))/(17.625-log(RH_ng_fu/100)-((17.625*tavg_fu)/(243.04+tavg_fu)))


tdew_obs=data.frame(tdew_obs1)
tdew_DAY=data.frame(rep(tdew_obs,2))
obs_tdew=tdew_DAY
sim_tdew=dewT_his
future_tdew=dewT_fu






obs1_tmax=data.frame(tmax_obs1)
obs1_tmin=data.frame(tmin_obs1)
tmax_DAY1=data.frame(rep(obs1_tmax,2))
tmin_DAY1=data.frame(rep(obs1_tmin,2))
dtr_obs=tmax_DAY1-tmin_DAY1
dtr_sim=proj_bc_cu_tmax-proj_bc_cu_tmin
dtr_proj=proj_bc_fu_tmax-proj_bc_fu_tmin



mon=c(1,2,3,4,5,6,7,8,9,10,11,12)

kk=proj_bc_fu
ng=10
for (m in 1:(ng*rcp)){
  
  for (i in 1:length(mon)) {
    
    
    id=which(proj_bc_cu_tmax[,2]==mon[i])
    obs=tmax_DAY1[id,m]
    sim=proj_bc_cu_tmax[id,m+3]
    id_proj1=which(proj_bc_fu_tmax[1:3653,2]==mon[i])
    id_proj2=which(proj_bc_fu_tmax[3654:7305,2]==mon[i])
    proj1=proj_bc_fu_tmax[id_proj1,m+3]
    proj2=proj_bc_fu_tmax[id_proj2,m+3]
    
    a1_1=mean(proj1)
    a1_2=mean(proj2)
    a2=mean(obs)
    a3=mean(sim)
    #a2=a2+0.5*(a1_1+a1_2)-a3
    
    b1_1=sd(proj1)
    b1_2=sd(proj2)
    b2=sd(obs)
    b3=sd(sim)
    #b2=b2*0.5*(b1_1+b1_2)/b3
    
    yu1=pnorm(proj1,a1_1,b1_1)
    yu2=pnorm(proj2,a1_2,b1_2)
    
    su1=qnorm(yu1,a2,b2)
    su2=qnorm(yu2,a2,b2)
    
    ju1=qnorm(yu1,a3,b3)
    ju2=qnorm(yu2,a3,b3)
    
    proj_corr1=proj1+su1-ju1
    proj_corr2=proj2+su2-ju2
    
    proj_bc_fu_tmax[id_proj1,m+3]=  proj_corr1
    proj_bc_fu_tmax[id_proj2,m+3]= proj_corr2
    
    
    
    
    
    
    
    
  }
  
  
  print(i)
}





















###format time########
his_dym=t(matrix(unlist(lapply(dates_obs, toNumerics)),nrow=3,ncol=length(dates_obs)))

dym=his_dym[367:731,2:3]

for(i in 1:len){
  
  for ( j in 1:365){
    
    
    
    ##########correcting tdew########
    # #     
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    ob_tdew=obs_tdew[id_his,i]
    md_tdew=sim_tdew[id_his,i]
    me_md_td=mean(md_tdew)
    id_proj=which(proj_bc_fu_tmax[,2]==dym[j,1] & proj_bc_fu_tmax[,3]==dym[j,2])
    td_1=future_tdew[id_proj,i]
    td_2=future_tdew[id_proj,i+10]
    
    me_td1=mean(td_1)
    me_td2=mean(td_2)
    
    detrend_td1=td_1+me_md_td-me_td1
    detrend_td2=td_2+me_md_td-me_td2
    
    
    td_h1=sort(ob_tdew)
    td_h2=sort(md_tdew)
    tdewfit=fitQmapRQUANT(td_h1,td_h2,qstep=0.1,nboot=10,wet.day=FALSE)
    td1_corr=doQmapRQUANT(detrend_td1,tdewfit,type="tricub")
    td2_corr=doQmapRQUANT(detrend_td2,tdewfit,type="tricub")
    
    # for ( m in 1: length(detrend_td1)){
    #   
    #   
    #   id_td1=which(td_h1>detrend_td1[m])
    #   t1=td_h1[id_td1[1]];if(length(t1)==0)t1=NA
    #   t2=td_h1[(id_td1[1]-1)];if(length(t2)==0)t2=NA
    #   
    #   id_td2=which(td_h2>detrend_td1[m])
    #   t3=td_h2[id_td2[1]];if(length(t3)==0)t3=NA
    #   t4=td_h2[(id_td2[1]-1)];if(length(t4)==0)t4=NA
    #   
    #   id_td3=which(td_h1>detrend_td2[m])
    #   t11=td_h1[id_td3[1]];if(length(t11)==0)t11=NA
    #   t22=td_h1[(id_td3[1]-1)];if(length(t22)==0)t22=NA
    #   
    #   id_td4=which(td_h2>detrend_td2[m])
    #   t33=td_h2[id_td4[1]];if(length(t33)==0)t33=NA
    #   t44=td_h2[(id_td4[1]-1)];if(length(t44)==0)t44=NA
    #   if(is.na(t1)==TRUE|| is.na(t2)==TRUE ||is.na(t3)==TRUE||is.na(t4)==TRUE){td1_corr[m]=td_1[m]}else {td1_corr[m]=td_1[m]+(detrend_td1[m]-t4)*(t1-t2)/(t3-t4)}
    #   if(is.na(t11)==TRUE||is.na(t22)==TRUE||is.na(t33)==TRUE||is.na(t44)==TRUE){td2_corr[m]=td_2[m]}else {td2_corr[m]=td_2[m]+(detrend_td2[m]-t44)*(t11-t22)/(t33-t44)}
    #   
    #   
    #   
    #   
    
    
    
    future_tdew[id_proj,i]=td1_corr
    future_tdew[id_proj,i+10]=td2_corr
    
    
  }
  
}






###temperature analysis######


tmax_base=(tmaxtmintdew_d[date_overlap_base,seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_base=(tmaxtmintdew_d[date_overlap_base,seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_base=(tmaxtmintdew_d[date_overlap_base,seq(4,dim(tmaxtmintdew_d)[2],3)])

tmax_dm_mon<- data.frame(monthlyfunction(tmax_base, FUN=mean, na.rm=TRUE,dates=dates_base))
tmin_dm_mon<- data.frame(monthlyfunction(tmin_base, FUN=mean, na.rm=TRUE,dates=dates_base))
tdew_dm_mon<- data.frame(monthlyfunction(tdew_base, FUN=mean, na.rm=TRUE,dates=dates_base))


tmax_bcca_mon_corr_56_65<- data.frame(monthlyfunction(tmax_bc_proj[1:3653,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tmax_bcca_mon_corr_90_99<- data.frame(monthlyfunction(tmax_bc_proj[3654:7305,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_3))


tmin_bcca_mon_corr_56_65<- data.frame(monthlyfunction(tmin_bc_proj[1:3653,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tmin_bcca_mon_corr_90_99<- data.frame(monthlyfunction(tmin_bc_proj[3654:7305,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_3))

tdew_bcca_mon_corr_56_65<- data.frame(monthlyfunction(tdew_proj[1:3653,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_2))
tdew_bcca_mon_corr_90_99<- data.frame(monthlyfunction(tdew_proj[3654:7305,seq(4,rcp*ng+3,1)], FUN=mean, na.rm=TRUE,dates=dat_3))
par(mfrow=c(2,2))
plot(colMeans(tdew_dm_mon),ylim=c(-5,20))
lines(colMeans(tdew_bcca_mon_corr_56_65[1:10,])+3)

plot(colMeans(tmax_dm_mon),ylim=c(-5,27))
lines(colMeans(tmax_bcca_mon_corr_56_65[1:10,]))

plot(colMeans(tmin_dm_mon),ylim=c(-5,27))
lines(colMeans(tmin_bcca_mon_corr_56_65[1:10,]))


plot(colMeans(tdew_dm_mon),ylim=c(-5,20))
lines(colMeans(tdew_bcca_mon_corr_56_65[11:20,])+3)

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

strDates=rbind(data.frame(date=dat_2),data.frame(date=dat_3))
gh=gsub("-","", strDates$date, fixed=TRUE)
day=data.frame(time=gh)
hour=rep.int(240000, nrow(day))

rcp_name=c("rcp4.5","rcp8.5")
nc=ng


tmaxtmintdew=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:(rcp*len)){
  tmaxtmintdew=cbind(tmaxtmintdew,tmax_bc_proj[,k+3],tmin_bc_proj[,k+3],tmin_bc_proj[,k+3]+4)
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













