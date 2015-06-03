#############Historical precipitaion bias correction$##############
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

Mod='MIROC5'
Model=Mod[1]
setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
pr.nc = nc_open("BCCA_0.125deg_tasmax_day_MIROC5_historical_r3i1p1_19800101-19891231.nc")
latall=ncvar_get(pr.nc ,'latitude')
lonall=ncvar_get(pr.nc ,'longitude')


###change the directory for each watershed #########
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
w=1
setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data\\",Model,sep=""))##for A1:


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
  actVp_d[i,j]=log(dss[i,4*j+1]/(1000*0.6108))# convert vapor pressure to kpa
  dewT_d[i,j]=(actVp_d[i,j]*237.3)/(17.27-actVp_d[i,j])
}
}

for ( k in 1:(nc)){
  precipitation_d[,k]=dss[,4*k-2]
  tmaxtmintdew_d=cbind(tmaxtmintdew_d,dss[,4*k-1],dss[,4*k],dewT_d[,k])
}
dates_bc=seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day") 
precip_day=data.frame(precipitation_d)
pr_obs_his=precip_day[1:length(dates_bc),]
tmax_obs_his=(tmaxtmintdew_d[1:length(dates_bc),seq(2,dim(tmaxtmintdew_d)[2],3)])
tmin_obs_his=(tmaxtmintdew_d[1:length(dates_bc),seq(3,dim(tmaxtmintdew_d)[2],3)])
tdew_obs_his=(tmaxtmintdew_d[1:length(dates_bc),seq(4,dim(tmaxtmintdew_d)[2],3)])

#####processiing historical rainfall  data #############

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
his_rain_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_pr_day_",Model,"_historical_",sep=""))
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
his_precp=data.frame(matrix(NA,nrow=1,ncol=ng+1))
for ( m in 1:length(his_rain_file)){
  pp=rain(his_rain_file[m])
  ne=colnames(pp)
  colnames(his_precp)[]=ne
  his_precp=rbind(his_precp,pp)
}


his_precp1=as.matrix((his_precp[-1,1:len]))

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

his_tmaximum=data.frame(matrix(NA,nrow=1,ncol=len+1))
his_tminimum=data.frame(matrix(NA,nrow=1,ncol=len+1))

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
his_tmaximum1=as.matrix((his_tmaximum[-1,1:len]))
his_tminimum1=as.matrix((his_tminimum[-1,1:len]))

pr_mod_his=his_precp1
tmax_mod_his=his_tmaximum1
tmin_mod_his=his_tminimum1



######quantile mapping bias correction method##########

toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}


his_dym=t(matrix(unlist(lapply(dates_bc, toNumerics)),nrow=3,ncol=length(dates_bc)))

dym=his_dym[367:731,2:3]

rainobs=data.frame(pr_obs_his)
rainsim=data.frame(pr_mod_his)

tmaxobs=data.frame(tmax_obs_his)
tmaxsim=data.frame(tmax_mod_his)

tminobs=data.frame(tmin_obs_his)
tminsim=data.frame(tmin_mod_his)

dtrobs=tmaxobs-tminobs
dtrsim=tmaxsim-tminsim


###rainfall correction###########

for(i in 1:(len)){
  
  for ( j in 1:365){
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    obs=rainobs[id_his,i]
    sim=rainsim[id_his,i] 
    obs1=sort(obs)
    sim1=sort(sim)
    rain_qm.fit1 <- fitQmapRQUANT(obs1,sim1,qstep=0.1,nboot=1,wet.day=0)
    his_corr1 <- doQmapRQUANT(sim,rain_qm.fit1,type="tricub")
    rainsim[id_his,i]= his_corr1
  }
  print(i)
}

###plotting######
plot(cumsum(rowMeans(pr_obs_his)))
lines(cumsum(rowMeans(pr_mod_his)),col='red')
lines(cumsum(rowMeans(rainsim)),col='green')

####monthly rainfall comparison###

pr_dm_mon<- data.frame(monthlyfunction(pr_obs_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26
pr_mod_mon<- data.frame(monthlyfunction(pr_mod_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26
pr_mod_mon_corr<- data.frame(monthlyfunction(rainsim, FUN=sum, na.rm=TRUE,dates=dates_bc))/26



plot(colMeans(pr_dm_mon),ylim=c(0,75))
lines(colMeans(pr_mod_mon),col='red')
lines(colMeans(pr_mod_mon_corr),col='green')

###temperature correction############
for(i in 1:(len)){
  
  for ( j in 1:365){
    
    
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    txobs=tmaxobs[id_his,i]
    txsim=tmaxsim[id_his,i]
    drobs=dtrobs[id_his,i]
    drsim=dtrsim[id_his,i]
    
    
    obs1=sort(txobs)
    sim1=sort(txsim)
    obs2=sort(drobs)
    sim2=sort(drsim)
    
    
    tmaxfit <- fitQmapRQUANT(obs1,sim1,qstep=0.01,nboot=1,wet.day=FALSE)
    corr1 <- doQmapRQUANT(txsim,tmaxfit,type="tricub")
    
    dtrfit <- fitQmapRQUANT(obs2,sim2,qstep=0.01,nboot=1,wet.day=FALSE)
    corr2<- doQmapRQUANT(drsim,dtrfit,type="tricub")
    
    tmaxsim[id_his,i]= corr1
    dtrsim[id_his,i]= corr2
    
    
  }
  print(i)
}

tmax_final=tmaxsim
dtr_final=dtrsim
tmin_final=tmax_final-dtr_final


###estimating tdew from tmin #########


tmax_dm_mon<- data.frame(monthlyfunction(tmax_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_dm_mon<- data.frame(monthlyfunction(tmin_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tdew_dm_mon<- data.frame(monthlyfunction(tdew_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

tmax_sim_mon<- data.frame(monthlyfunction(tmax_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_sim_mon<- data.frame(monthlyfunction(tmin_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

tmax_sim_mon_cor<- data.frame(monthlyfunction(tmax_final, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_sim_mon_cor<- data.frame(monthlyfunction(tmin_final, FUN=mean, na.rm=TRUE,dates=dates_bc))



par(mfrow=c(2,2))
plot(colMeans(tmax_dm_mon),ylim=c(-5,35))
lines(colMeans(tmax_sim_mon))
lines(colMeans(tmax_sim_mon_cor),col='red')

plot(colMeans(tmin_dm_mon),ylim=c(-15,25))
lines(colMeans(tmin_sim_mon))
lines(colMeans(tmin_sim_mon_cor),col='red')


tmaxtmintdew=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:len){
  tmaxtmintdew=cbind(tmaxtmintdew,tmax_final[,k],tmin_final[,k],tmin_final[,k]-0.5)
}

temper=data.frame(tmaxtmintdew[,-1])
precip=data.frame(rainsim)

precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
strDates=rbind(data.frame(date=dates_bc))
gh=gsub("-","", strDates$date, fixed=TRUE)
day=data.frame(time=gh)
hour=rep.int(240000, nrow(day))
precip=data.frame(precip,day,hour)
temper=data.frame(temper,day,hour)


##writing rain.dat file
nc=ng
sink("rain.dat") 
cat(sprintf("This file provides daily precipitation rate for each rain station"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
cat(sprintf("Precipitation is provided in mm/day"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file='rain.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='rain.dat',append=TRUE) 
cat("\n", file="rain.dat", append=TRUE)
write.table(precip, file = "rain.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()






##writing tmaxtmintdew.dat file

sink('tmaxtmintdew.dat')
cat(sprintf("This file provides daily values of Tmax/Tmin/Tdew for each temperature station"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
cat(sprintf("Temperature is provided in degrees Celcius"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file='tmaxtmintdew.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='tmaxtmintdew.dat',append=TRUE) 
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
write.table(temper, file = "tmaxtmintdew.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()




######correcting tdew########
##Estimating tdewfrom specifcic humidity and pressure ######

hist_huss_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("huss_day_",Model,"_historical_",sep=""))
hist_psl_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("psl_day_",Model,"_historical_",sep=""))



##get index of the nearest lat,lon of the grid ###
sh.nc = nc_open(hist_huss_file[1])
sp.nc=nc_open(hist_psl_file[1])
sh_latall=ncvar_get(sh.nc,'lat')
sh_lonall=ncvar_get(sh.nc,'lon')
len=length(lon_ws)
sh_lon_ID=matrix(NA,nrow=len,ncol=1)
sh_lat_ID=matrix(NA,nrow=len,ncol=1)
for ( i in 1:len){
  sh_lon_ID[i,1]=which.min(abs(sh_lonall-lon_ws[i])) 
  sh_lat_ID[i,1]=which.min(abs(sh_latall-lat_ws[i])) 
}
sh_lat_lon_ID=cbind(sh_lon_ID,sh_lat_ID)
sh_ID=unique(sh_lat_lon_ID)

##now extract data from each of the file that have same index#########

##create time series for the data first

hus <- function(file ){
  
  hus.nc = nc_open(file)
  hus_time=as.Date(ncvar_get(hus.nc,'time'),origin="1850-01-01")
  #strDates= as.character(hur_time)
  sh=matrix(NA,nrow=length(hus_time),ncol=length(sh_ID[,1]))
  for( i in 1:length(sh_ID[,1])){
    start=c(sh_ID[i,1],sh_ID[i,2],1)
    count=c(1,1,length(hus_time))
    sh[,i]=ncvar_get(hus.nc,'huss',start,count)
  }
  
  shdata=data.frame(data=cbind(sh, hus_time-0.5))
  return(shdata)
}
shumd=data.frame(matrix(NA,nrow=1,ncol=1+length(sh_ID[,1])))
for ( m in 1:length(hist_huss_file)){
  sd=hus(hist_huss_file[m])
  ne=colnames(sd)
  colnames(shumd)[]=ne
  shumd=rbind(shumd,sd)
}

psl <- function(file ){
  
  hus.nc = nc_open(file)
  hus_time=as.Date(ncvar_get(hus.nc,'time'),origin="1850-01-01")
  #strDates= as.character(hur_time)
  sh=matrix(NA,nrow=length(hus_time),ncol=length(sh_ID[,1]))
  for( i in 1:length(sh_ID[,1])){
    start=c(sh_ID[i,1],sh_ID[i,2],1)
    count=c(1,1,length(hus_time))
    sh[,i]=ncvar_get(hus.nc,'psl',start,count)
  }
  
  shdata=data.frame(data=cbind(sh, hus_time-0.5))
  return(shdata)
}
slp=data.frame(matrix(NA,nrow=1,ncol=1+length(sh_ID[,1])))
for ( m in 1:length(hist_psl_file)){
  sd=psl(hist_psl_file[m])
  ne=colnames(sd)
  colnames(slp)[]=ne
  slp=rbind(slp,sd)
}

slp=slp[-1,]
##fill gaps for leap year #####
##for huss

##finding leap year
dates_hus=seq(as.Date("1980/1/1"), as.Date("2009/12/31"), "day") 
dym_data=t(matrix(unlist(lapply(dates_hus, toNumerics)),nrow=3,ncol=length(dates_hus)))
dym_lp=dym_data[,2:3]
lp_id=which(dym_lp[,1]==2 & dym_lp[,2]==29)
dym_without_leap=dym_data[-lp_id,]
huss_mat=matrix(NA,nrow=length(dates_hus),ncol=1)
huss_mat[lp_id]=-999
shumd=shumd[-1,]
huss_mat[is.na(huss_mat)]=shumd[,1]
mtach_huss=match(dates_bc,dates_hus)
huss_mat_cp=huss_mat[mtach_huss]

##for psl
dates_psl=seq(as.Date("1980/1/1"), as.Date("2009/12/31"), "day") 
dym_psl=t(matrix(unlist(lapply(dates_psl, toNumerics)),nrow=3,ncol=length(dates_psl)))
dym_lp1=dym_psl[,2:3]
lp_id1=which(dym_lp1[,1]==2 & dym_lp1[,2]==29)
spl_mat=matrix(NA,nrow=length(dates_psl),ncol=1)
spl_mat[lp_id1]=-999
spl_mat[is.na(spl_mat)]=slp[,1]

match_spl_hist=match(dates_bc,dates_psl)

spl_mod_his=data.frame(spl_mat[match_spl_hist]/100) ##100 for pa to mb
huss_mod_his=data.frame(huss_mat_cp)

#####convert sea level pressure to surface pressure ####

Elevation<- function(file ){
  station_ele = as.matrix(read.csv(file))[3]
  ele = (as.numeric((unlist(strsplit(station_ele, split=' ', fixed=TRUE))[2])))
  return(ele)
}
setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data\\",Model,sep=""))##for A1:

Ele_station=matrix(ncol =1, nrow = nc)

for ( m in 1:length(xx)){
  
  Ele_station[m,]=Elevation(xx[m])
  
}

tavg_mod_his=0.5*(tmax_mod_his+tmin_mod_his)+273.15
elevat=t(data.frame(rep(data.frame(Ele_station),9497)))
huss_mod_his=data.frame(rep(huss_mod_his,13))
spl_mod_his=data.frame(rep(data.frame(spl_mod_his),13))
sf_mod_his=(spl_mod_his*exp(-elevat/(tavg_mod_his*29.263)))  ## 100 for pa to mb
vp_mod_his=huss_mod_his*sf_mod_his/(0.622+0.378*huss_mod_his)
Td_mod_his_uncor= (log(vp_mod_his/6.112))*243.5/(17.67-log(vp_mod_his/6.112)) 
vp_obs_his=data.frame(actVp_d[1:length(dates_bc),])
for(i in 1:(len)){
  
  for ( j in 1:365){
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    obs=vp_obs_his[id_his,i]
    sim=vp_mod_his[id_his,i] 
    obs1=sort(obs)
    sim1=sort(sim)
    rain_qm.fit1 <- fitQmapRQUANT(obs1,sim1,qstep=0.1,nboot=1,wet.day=FALSE)
    his_corr1 <- doQmapRQUANT(sim,rain_qm.fit1,type="tricub")
    vp_mod_his[id_his,i]= his_corr1
  }
  print(i)
}

Td_mod_his = (log(vp_mod_his/6.112))*243.5/(17.67-log(vp_mod_his/6.112)) 
tdew_mod_his=Td_mod_his 
tdew_mean=mean(colMeans(tdew_mod_his,na.rm=TRUE))
tdew_mod_his[is.na(tdew_mod_his)]=tdew_mean
tdew_sim_mon<- data.frame(monthlyfunction(Td_mod_his_uncor, FUN=mean, na.rm=TRUE,dates=dates_bc))
tdew_sim_mon_cor<- data.frame(monthlyfunction(tdew_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))


par(mfrow=c(2,2))
plot(colMeans(tdew_dm_mon),ylim=c(-5,35))
lines(colMeans(tdew_sim_mon))
lines(colMeans(tdew_sim_mon_cor),col='red')

tmaxtmintdew=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:len){
  tmaxtmintdew=cbind(tmaxtmintdew,tmax_final[,k],tmin_final[,k],tdew_mod_his[,k])
}

temper=data.frame(tmaxtmintdew[,-1])
precip=data.frame(rainsim)

precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
strDates=rbind(data.frame(date=dates_bc))
gh=gsub("-","", strDates$date, fixed=TRUE)
day=data.frame(time=gh)
hour=rep.int(240000, nrow(day))
precip=data.frame(precip,day,hour)
temper=data.frame(temper,day,hour)


##writing rain.dat file
nc=ng
sink("rain.dat") 
cat(sprintf("This file provides daily precipitation rate for each rain station"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
cat(sprintf("Precipitation is provided in mm/day"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file='rain.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='rain.dat',append=TRUE) 
cat("\n", file="rain.dat", append=TRUE)
write.table(precip, file = "rain.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()


##writing tmaxtmintdew.dat file

sink('tmaxtmintdew.dat')
cat(sprintf("This file provides daily values of Tmax/Tmin/Tdew for each temperature station"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
cat(sprintf("Temperature is provided in degrees Celcius"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file='tmaxtmintdew.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='tmaxtmintdew.dat',append=TRUE) 
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
write.table(temper, file = "tmaxtmintdew.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()




############correcting wind data#########################
##reading historical wind data###########

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
sfc_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*sfcWind_day_",Model,"_historical",sep=""))
#get index of the nearest lat,lon of the grid ###
sf.nc = nc_open(sfc_file[1])
sa_latall=ncvar_get(sf.nc,'lat')
sa_lonall=ncvar_get(sf.nc,'lon')
len=length(lon_ws)
sa_lon_ID=matrix(NA,nrow=len,ncol=1)
sa_lat_ID=matrix(NA,nrow=len,ncol=1)
for ( i in 1:len){
  sa_lon_ID[i,1]=which.min(abs(sa_lonall-lon_ws[i])) 
  sa_lat_ID[i,1]=which.min(abs(sa_latall-lat_ws[i])) 
}
sa_lat_lon_ID=cbind(sa_lon_ID,sa_lat_ID)
sa_ID=unique(sa_lat_lon_ID)



##get index of the nearest lat,lon of the

wind <- function(file ){
  
  w.nc = nc_open(file)
  w_time=as.Date(ncvar_get(w.nc,'time'),origin="1850-01-01")
  #strDates= as.character(hur_time)
  #ua=matrix(NA,nrow=length(ua_time),ncol=length(ua_ID[,1]))
  w=matrix(NA,nrow=length(w_time),ncol=1)
  
  for( i in 1:1){
    start=c(sa_ID[1,1],sa_ID[1,2],1)
    count=c(1,1,length(w_time))
    w[,i]=ncvar_get(w.nc,'sfcWind',start,count)
  }
  wdata=data.frame(data=cbind(w, w_time-0.5))
  return(wdata)
}

ws=data.frame(matrix(NA,nrow=1,ncol=1+1))

for ( m in 1:length(sfc_file)){
  wd=wind(sfc_file[m])
  ne=colnames(wd)
  colnames(ws)[]=ne
  ws=rbind(ws,wd)
}

###filling gap for leap year########

##finding leap year
dates_win=seq(as.Date("1980/1/1"), as.Date("2009/12/31"), "day") 
dym_wind=t(matrix(unlist(lapply(dates_win, toNumerics)),nrow=3,ncol=length(dates_win)))
dym_wn=dym_wind[,2:3]
wn_id=which(dym_wn[,1]==2 & dym_wn[,2]==29)
dym_without_leapwn=dym_wind[-wn_id,]
wind_mat=matrix(NA,nrow=length(dates_win),ncol=1)
wind_mat[wn_id]=-999
ws=ws[-1,]
wind_mat[is.na(wind_mat)]=ws[,1]
mtach_wind=match(dates_bc,dates_win)
wind_mat_cp=data.frame(wind_mat[mtach_wind])

wn_mean=mean(colMeans(wind_mat_cp,na.rm=TRUE))
wind_mat_cp[wind_mat_cp<0]=wn_mean
all_wind=data.frame(wind=wind_mat_cp)
wind_mod_his=all_wind

setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data\\",Model,sep=""))
wind_obs_his=read.table('wind_obs.dat',skip=3)
plot(wind_mod_his[,1],wind_obs_his[,1])

####bias correction of wind data########

for(i in 1:1){
  
  for ( j in 1:365){
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    obs=wind_obs_his[id_his,i]
    sim=wind_mod_his[id_his,i] 
    obs1=sort(obs)
    sim1=sort(sim)
    rain_qm.fit1 <- fitQmapRQUANT(obs1,sim1,qstep=0.1,nboot=1,wet.day=FALSE)
    his_corr1 <- doQmapRQUANT(sim,rain_qm.fit1,type="tricub")
    wind_mod_his[id_his,i]= his_corr1
  }
  print(i)
}

wind_obs_his=matrix(wind_obs_his[,1])
wind_mod_his=data.frame(wind_mod_his[,1])
wn_obs_mon<- data.frame(monthlyfunction(wind_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc)) 
wn_sim_mon<- data.frame(monthlyfunction(all_wind, FUN=mean, na.rm=TRUE,dates=dates_bc))
wn_sim_mon_cor<- data.frame(monthlyfunction(wind_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc)) 

par(mfrow=c(2,2))
plot(colMeans(wn_obs_mon))
lines(colMeans(wn_sim_mon))
lines(colMeans(wn_sim_mon_cor),col='red')

wind_his <- lapply(wind_mod_his, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
wind_speed=data.frame(wind_his,day,hour) ##Need to change dss



##Comments: Number of Stream flow station data and wind station data should be similar ( I experienced that it does not work if the number of stations are different)
file_win="wind1.dat"
sink(file_win)
cat(sprintf("This file provides daily values of wind speed for each wind station"),file=file_win,append=TRUE)
cat("\n", file=file_win, append=TRUE)
cat(sprintf("Wind speed is provided in m/sec"),file=file_win,append=TRUE)
cat("\n", file=file_win, append=TRUE)
sites=seq(1,1,1) ## need to automate this one
cat(sprintf("%s %d ", "ver2",1),file=file_win,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=file_win,append=TRUE) 
cat("\n", file=file_win, append=TRUE)
write.table(wind_speed, file = file_win,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()
















































