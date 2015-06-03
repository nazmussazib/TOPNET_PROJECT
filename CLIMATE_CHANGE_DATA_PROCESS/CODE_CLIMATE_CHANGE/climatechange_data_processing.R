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

Mod=c('CCSM4','CanESM2','GFDL-CM3','MPI-ESM-MR','MRI-CGCM3','CNRM-CM5','MIROC5')
Model=Mod[4]
setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
pr.nc = nc_open(paste("BCCA_0.125deg_tasmax_day_",Model,"_rcp26_r1i1p1_20400101-20491231.nc",sep=""))
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

###creating clipar.dat file for particular watershed 

##space for rcreating cliapar.dat file


dewT_d=matrix(ncol =nc, nrow = nrow(dss))
avgT_d=matrix(ncol = nc, nrow = nrow(dss))
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

#####processiing historical and projected rainfall  data #############

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
his_rain_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_pr_day_",Model,"_historical_",sep=""))
proj_rain_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*_pr_day_",Model,"_rcp",sep=""))
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
his_precp=data.frame(matrix(NA,nrow=1,ncol=ng+1))
for ( m in 1:length(his_rain_file)){
  pp=rain(his_rain_file[m])
  ne=colnames(pp)
  colnames(his_precp)[]=ne
  his_precp=rbind(his_precp,pp)
}


precp=data.frame(matrix(NA,nrow=1,ncol=ng+1))
for ( m in 1:length(proj_rain_file)){
  pp=rain(proj_rain_file[m])
  ne=colnames(pp)
  colnames(precp)[]=ne
  precp=rbind(precp,pp)
}


his_precp1=as.matrix((his_precp[-1,1:len]))
precp1=as.matrix((precp[-1,1:len]))
precp_rcp_all=cbind(precp1[1:(nrow(precp1)/2),],precp1[(1+nrow(precp1)/2):nrow(precp1),])


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
his_tmaximum1=as.matrix((his_tmaximum[-1,1:len]))
his_tminimum1=as.matrix((his_tminimum[-1,1:len]))


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
tmax_rcp_all=cbind(proj_tmaximum1[1:(nrow(proj_tmaximum1)/2),],proj_tmaximum1[(1+(nrow(proj_tmaximum1)/2)):nrow(proj_tmaximum1),])
tmin_rcp_all=cbind(proj_tminimum1[1:(nrow(proj_tminimum1)/2),],proj_tminimum1[(1+(nrow(proj_tminimum1)/2)):nrow(proj_tminimum1),])


######quantile mapping bias correction method##########

toNumerics <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
  list(year = year, month = month, day = day)
}

dat_1=seq(as.Date("2040/1/1"), as.Date("2069/12/31"), "day")
dat_2=seq(as.Date("2070/1/1"), as.Date("2099/12/31"), "day")
proj_dff1=t(matrix(unlist(lapply(dat_1, toNumerics)),nrow=3,ncol=length(dat_1)))
proj_dff2=t(matrix(unlist(lapply(dat_2, toNumerics)),nrow=3,ncol=length(dat_2)))
proj_diff=rbind(proj_dff1,proj_dff2)
proj_rain1=cbind(proj_dff1,precp_rcp_all[1:length(dat_1),1:len]) ##rcp2.6 45
proj_rain2=cbind(proj_dff1,precp_rcp_all[1:length(dat_1),(len+1):(len*2)])  ##rcp8.5 45
proj_rain3=cbind(proj_dff2,precp_rcp_all[(length(dat_1)+1):nrow(proj_diff),1:len]) ##rcp 2.6 76
proj_rain4=cbind(proj_dff2,precp_rcp_all[(length(dat_1)+1):nrow(proj_diff),(len+1):(len*2)]) ##rcp 8.5 76

proj_tmax1=cbind(proj_dff1,tmax_rcp_all[1:length(dat_1),1:len]) ##rcp2.6 45
proj_tmax2=cbind(proj_dff1,tmax_rcp_all[1:length(dat_1),(len+1):(len*2)])  ##rcp8.5 45
proj_tmax3=cbind(proj_dff2,tmax_rcp_all[(length(dat_1)+1):nrow(proj_diff),1:len]) ##rcp 2.6 76
proj_tmax4=cbind(proj_dff2,tmax_rcp_all[(length(dat_1)+1):nrow(proj_diff),(len+1):(len*2)]) ##rcp 8.5 76

proj_tmin1=cbind(proj_dff1,tmin_rcp_all[1:length(dat_1),1:len]) ##rcp2.6 45
proj_tmin2=cbind(proj_dff1,tmin_rcp_all[1:length(dat_1),(len+1):(len*2)])  ##rcp8.5 45
proj_tmin3=cbind(proj_dff2,tmin_rcp_all[(length(dat_1)+1):nrow(proj_diff),1:len]) ##rcp 2.6 76
proj_tmin4=cbind(proj_dff2,tmin_rcp_all[(length(dat_1)+1):nrow(proj_diff),(len+1):(len*2)]) ##rcp 8.5 76


pr_mod_his=his_precp1
tmax_mod_his=his_tmaximum1
tmin_mod_his=his_tminimum1
rainobs=data.frame(pr_obs_his)
rainsim=data.frame(pr_mod_his)
tmaxobs=data.frame(tmax_obs_his)
tmaxsim=data.frame(tmax_mod_his)
tminobs=data.frame(tmin_obs_his)
tminsim=data.frame(tmin_mod_his)
dtrobs=tmaxobs-tminobs
dtrsim=tmaxsim-tminsim

proj_dtr1=proj_tmax1-proj_tmin1
proj_dtr2=proj_tmax2-proj_tmin2
proj_dtr3=proj_tmax3-proj_tmin3
proj_dtr4=proj_tmax4-proj_tmin4

his_dym=t(matrix(unlist(lapply(dates_bc, toNumerics)),nrow=3,ncol=length(dates_bc)))
dym=his_dym[367:731,2:3]



######before correction that is wihtout bias correction (just to check whether i am doing right) ###########

pr_dm_mon<- data.frame(monthlyfunction(pr_obs_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26
pr_mod_mon<- data.frame(monthlyfunction(pr_mod_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26


a=proj_rain1[,4:16]
b=proj_rain2[,4:16]
c=proj_rain3[,4:16]
d=proj_rain4[,4:16]
pr_proj_mon11=data.frame(monthlyfunction(a, FUN=sum, na.rm=TRUE,dates=dat_1))/20
pr_proj_mon22=data.frame(monthlyfunction(b, FUN=sum, na.rm=TRUE,dates=dat_1))/20
pr_proj_mon33=data.frame(monthlyfunction(c, FUN=sum, na.rm=TRUE,dates=dat_2))/20
pr_proj_mon44=data.frame(monthlyfunction(d, FUN=sum, na.rm=TRUE,dates=dat_2))/20


op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(dates_bc,cumsum(rowMeans(pr_obs_his)),xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o")
lines(dates_bc,cumsum(rowMeans(pr_mod_his)),col='grey',lty=1,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(colMeans(pr_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylab="Monthly Rainfall (mm)")
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=10*0:max(colMeans(pr_dm_mon)),ylab="Monthly Rainfall(mm)")
lines(colMeans(pr_mod_mon),col='grey',lty=1,lwd=2,type="o")
title(xlab="Month", col.lab=rgb(0,0,0))
title(ylab="Monthly Rainfall (mm)", col.lab=rgb(0,0,0))

op <- par(mfrow = c(2,2),mar=c(4,4,1,1), oma=c(1.5,2,1,1))
barplot((((colMeans(pr_proj_mon11)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP2.6 46-65")
barplot((((colMeans(pr_proj_mon22)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP8.5 46-65")
barplot((((colMeans(pr_proj_mon33)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP2.6 76-95")
barplot((((colMeans(pr_proj_mon44)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP8.5 76-95")





###rainfall correction###########

for(i in 1:(len)){
  
  for ( j in 1:12){
    
    id_his=which(his_dym[,2]==j)
    id_proj1=which(proj_dff1[,2]==j)
    id_proj2=which(proj_dff2[,2]==j)
    
    obs=rainobs[id_his,i]
    sim=rainsim[id_his,i]
    
    ps=startberngamma(obs)
    ts=startberngamma(sim)
    
    p2=pberngamma(sim,prob=ts$prob,scale=ts$scale,shape=ts$shape);his_corr=qberngamma(p2,prob=ps$prob,scale=ps$scale,shape=ps$shape)
    rainsim[id_his,i]= his_corr
    
    
    proj1=proj_rain1[id_proj1,i+3];proj_corr1=pberngamma(proj1,prob=ts$prob,scale=ts$scale,shape=ts$shape);proj_corr2=qberngamma(proj_corr1,prob=ps$prob,scale=ps$scale,shape=ps$shape);
    proj2=proj_rain2[id_proj1,i+3];proj_corr3=pberngamma(proj2,prob=ts$prob,scale=ts$scale,shape=ts$shape);proj_corr4=qberngamma(proj_corr3,prob=ps$prob,scale=ps$scale,shape=ps$shape);
    proj3=proj_rain3[id_proj2,i+3];proj_corr5=pberngamma(proj3,prob=ts$prob,scale=ts$scale,shape=ts$shape);proj_corr6=qberngamma(proj_corr5,prob=ps$prob,scale=ps$scale,shape=ps$shape);
    proj4=proj_rain4[id_proj2,i+3];proj_corr7=pberngamma(proj4,prob=ts$prob,scale=ts$scale,shape=ts$shape);proj_corr8=qberngamma(proj_corr7,prob=ps$prob,scale=ps$scale,shape=ps$shape);
    
    proj_rain1[id_proj1,i+3]=proj_corr2
    proj_rain2[id_proj1,i+3]=proj_corr4
    proj_rain3[id_proj2,i+3]=proj_corr6
    proj_rain4[id_proj2,i+3]=proj_corr8
    
    
    
  }
}





###########Rainfall analysis and comparison #####################

##historical rainfall comparison ######  

pr_dm_mon<- data.frame(monthlyfunction(pr_obs_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26
pr_mod_mon<- data.frame(monthlyfunction(pr_mod_his, FUN=sum, na.rm=TRUE,dates=dates_bc))/26
pr_mod_mon_corr<- data.frame(monthlyfunction(rainsim, FUN=sum, na.rm=TRUE,dates=dates_bc))/26

pr_proj_mon1=data.frame(monthlyfunction(proj_rain1[,4:16], FUN=sum, na.rm=TRUE,dates=dat_1))/20
pr_proj_mon2=data.frame(monthlyfunction(proj_rain2[,4:16], FUN=sum, na.rm=TRUE,dates=dat_1))/20
pr_proj_mon3=data.frame(monthlyfunction(proj_rain3[,4:16], FUN=sum, na.rm=TRUE,dates=dat_2))/20
pr_proj_mon4=data.frame(monthlyfunction(proj_rain4[,4:16], FUN=sum, na.rm=TRUE,dates=dat_2))/20


op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(dates_bc,cumsum(rowMeans(pr_obs_his)),xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o")
lines(dates_bc,cumsum(rowMeans(pr_mod_his)),col='grey',lty=1,lwd=3)
lines(dates_bc,cumsum(rowMeans(rainsim)),col='grey',lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(colMeans(pr_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylab="Monthly Rainfall (mm)")
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=10*0:max(colMeans(pr_dm_mon)),ylab="Monthly Rainfall(mm)")
lines(colMeans(pr_mod_mon),col='grey',lty=1,lwd=2,type="o")
lines(colMeans(pr_mod_mon_corr),col='grey',lty=3,lwd=2,type="o")
title(xlab="Month", col.lab=rgb(0,0,0))
title(ylab="Monthly Rainfall (mm)", col.lab=rgb(0,0,0))

op <- par(mfrow = c(2,2),mar=c(4,4,1,1), oma=c(1.5,2,1,1))
barplot((((colMeans(pr_proj_mon1)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP2.6 46-65")
barplot((((colMeans(pr_proj_mon2)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP8.5 46-65")
barplot((((colMeans(pr_proj_mon3)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP2.6 76-95")
barplot((((colMeans(pr_proj_mon4)/colMeans(pr_dm_mon))-1)*100),ylab="Rainfall difference(mm)",main="RCP8.5 76-95")

###temperature correction ###############


####before temperature correction without bias correction how it looks just for checking whethe rdoing correct bias correction %%%%%%%%%%
tmax_dm_mon<- data.frame(monthlyfunction(tmax_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_dm_mon<- data.frame(monthlyfunction(tmin_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tdew_dm_mon<- data.frame(monthlyfunction(tdew_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

tmax_sim_mon<- data.frame(monthlyfunction(tmax_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_sim_mon<- data.frame(monthlyfunction(tmin_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

avgT_mod1=0.5*(colMeans(tmax_dm_mon)+colMeans(tmin_dm_mon))

avgT_mod2=0.5*(colMeans(tmax_sim_mon)+colMeans(tmin_sim_mon))

op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(colMeans(tmax_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylim=c(min(colMeans(tmax_dm_mon)-2), max(colMeans(tmax_dm_mon))+5))
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=5*0:8)
lines(colMeans(tmax_sim_mon),col='grey',lty=1,lwd=2,type="o")

title(ylab="Tmax(C)", col.lab=rgb(0,0,0))
legend('topleft',bty="n",c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))

plot(colMeans(tmin_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylim=c(min(colMeans(tmin_dm_mon)-2), max(colMeans(tmin_dm_mon))+5))
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=seq(-15,15,5))
lines(colMeans(tmin_sim_mon),col='grey',lty=1,lwd=2,type="o")
title(ylab="Tmin(C)", col.lab=rgb(0,0,0))


#####Temperature change analysis##########
tmax_proj_mon1=data.frame(monthlyfunction(proj_tmax1[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmax_proj_mon2=data.frame(monthlyfunction(proj_tmax2[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmax_proj_mon3=data.frame(monthlyfunction(proj_tmax3[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))
tmax_proj_mon4=data.frame(monthlyfunction(proj_tmax4[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))

tmin_proj_mon1=data.frame(monthlyfunction(proj_tmin1[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmin_proj_mon2=data.frame(monthlyfunction(proj_tmin2[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmin_proj_mon3=data.frame(monthlyfunction(proj_tmin3[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))
tmin_proj_mon4=data.frame(monthlyfunction(proj_tmin4[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))

avgT_mod_proj1=0.5*(colMeans(tmax_proj_mon1)+colMeans(tmin_proj_mon1))
avgT_mod_proj2=0.5*(colMeans(tmax_proj_mon2)+colMeans(tmin_proj_mon2))
avgT_mod_proj3=0.5*(colMeans(tmax_proj_mon3)+colMeans(tmin_proj_mon3))
avgT_mod_proj4=0.5*(colMeans(tmax_proj_mon4)+colMeans(tmin_proj_mon4))

op <- par(mfrow = c(2,2),mar=c(4,4,1,1), oma=c(1.5,2,1,1))
barplot((avgT_mod_proj1-avgT_mod1),ylab="Change of Temperature(c)", main="RCP2.6 46-65")
barplot((avgT_mod_proj2-avgT_mod1),ylab="Change of Temperature(c)", main="RCP8.5 46-65")
barplot((avgT_mod_proj3-avgT_mod1),ylab="Change of Temperature(c)", main="RCP2.6 76-95")
barplot((avgT_mod_proj4-avgT_mod1),ylab="Change of Temperature(c)", main="RCP8.5 76-95")

for(i in 1:len){
  
  for ( j in 1:12){
    
    id_his=which(his_dym[,2]==j)
    id_proj1=which(proj_dff1[,2]==j )
    id_proj2=which(proj_dff2[,2]==j )
    
    obs=(tmaxobs[id_his,i]);
    
    sim=(tmaxsim[id_his,i]);
    
    qm.fit <- fitQmapRQUANT(obs,sim,qstep=0.1,nboot=1,wet.day=FALSE)
    qm <- doQmapRQUANT(sim,qm.fit,type="linear"); his_corr=qm
    
    
    
    proj1=(proj_tmax1[id_proj1,i+3]);proj_corr2=doQmapRQUANT(proj1,qm.fit,type="linear");
    proj2=(proj_tmax2[id_proj1,i+3]);proj_corr4=doQmapRQUANT(proj2,qm.fit,type="linear");
    proj3=(proj_tmax3[id_proj2,i+3]);proj_corr6=doQmapRQUANT(proj3,qm.fit,type="linear");
    proj4=(proj_tmax4[id_proj2,i+3]);proj_corr8=doQmapRQUANT(proj4,qm.fit,type="linear");
    
    
    
    obsdr=(dtrobs[id_his,i]);
    simdr=(dtrsim[id_his,i]);
    qm.fitdr <- fitQmapRQUANT(obsdr,simdr,qstep=0.1,nboot=1,wet.day=0)
    qmdr <- doQmapRQUANT(simdr,qm.fitdr,type="linear"); his_corrdr=qmdr
    
    
    
    
    proj1dr=(proj_dtr1[id_proj1,i+3]);proj_corr2dr=doQmapRQUANT(proj1dr,qm.fitdr,type="linear");
    proj2dr=(proj_dtr2[id_proj1,i+3]);proj_corr4dr=doQmapRQUANT(proj2dr,qm.fitdr,type="linear");
    proj3dr=(proj_dtr3[id_proj2,i+3]);proj_corr6dr=doQmapRQUANT(proj3dr,qm.fitdr,type="linear");
    proj4dr=(proj_dtr4[id_proj2,i+3]);proj_corr8dr=doQmapRQUANT(proj4dr,qm.fitdr,type="linear");
    
    
    
    
    tmaxsim[id_his,i]= his_corr
    dtrsim[id_his,i]= his_corrdr
    proj_tmax1[id_proj1,i+3]=proj_corr2
    proj_tmax2[id_proj1,i+3]=proj_corr4
    proj_tmax3[id_proj2,i+3]=proj_corr6
    proj_tmax4[id_proj2,i+3]=proj_corr8
    proj_dtr1[id_proj1,i+3]= proj_corr2dr
    proj_dtr2[id_proj1,i+3]=proj_corr4dr
    proj_dtr3[id_proj2,i+3]=proj_corr6dr
    proj_dtr4[id_proj2,i+3]=proj_corr8dr
    
    
  }
  print(i)
}



tmax_final=tmaxsim
dtr_final=dtrsim
tmin_final=tmax_final-dtr_final
tmin_proj1=proj_tmax1-proj_dtr1
tmin_proj2=proj_tmax2-proj_dtr2
tmin_proj3=proj_tmax3-proj_dtr3
tmin_proj4=proj_tmax4-proj_dtr4

##########Temperature analysis compare to base line period#######

tmax_dm_mon<- data.frame(monthlyfunction(tmax_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_dm_mon<- data.frame(monthlyfunction(tmin_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tdew_dm_mon<- data.frame(monthlyfunction(tdew_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

tmax_sim_mon<- data.frame(monthlyfunction(tmax_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_sim_mon<- data.frame(monthlyfunction(tmin_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc))

tmax_sim_mon_cor<- data.frame(monthlyfunction(tmax_final, FUN=mean, na.rm=TRUE,dates=dates_bc))
tmin_sim_mon_cor<- data.frame(monthlyfunction(tmin_final, FUN=mean, na.rm=TRUE,dates=dates_bc))
avgT_mod=0.5*(colMeans(tmax_sim_mon_cor)+colMeans(tmin_sim_mon_cor))

op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(colMeans(tmax_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylim=c(min(colMeans(tmax_dm_mon)-2), max(colMeans(tmax_dm_mon))+5))
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=5*0:8)
lines(colMeans(tmax_sim_mon),col='grey',lty=1,lwd=2,type="o")
lines(colMeans(tmax_sim_mon_cor),col='grey',lty=3,lwd=2,type="o")
title(ylab="Tmax(C)", col.lab=rgb(0,0,0))
legend('topleft',bty="n",c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))

plot(colMeans(tmin_dm_mon),axes=FALSE, ann=FALSE,lty=1,lwd=2,type="o",ylim=c(min(colMeans(tmin_dm_mon)-2), max(colMeans(tmin_dm_mon))+5))
axis(1, at=1:12, lab= colnames(pr_dm_mon));box();axis(2, las=1, at=seq(-15,15,5))
lines(colMeans(tmin_sim_mon),col='grey',lty=1,lwd=2,type="o")
lines(colMeans(tmin_sim_mon_cor),col='grey',lty=3,lwd=2,type="o")
title(ylab="Tmin(C)", col.lab=rgb(0,0,0))


#####Temperature change analysis##########
tmax_proj_mon1=data.frame(monthlyfunction(proj_tmax1[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmax_proj_mon2=data.frame(monthlyfunction(proj_tmax2[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmax_proj_mon3=data.frame(monthlyfunction(proj_tmax3[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))
tmax_proj_mon4=data.frame(monthlyfunction(proj_tmax4[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))

tmin_proj_mon1=data.frame(monthlyfunction(tmin_proj1[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmin_proj_mon2=data.frame(monthlyfunction(tmin_proj2[,4:16], FUN=mean, na.rm=TRUE,dates=dat_1))
tmin_proj_mon3=data.frame(monthlyfunction(tmin_proj3[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))
tmin_proj_mon4=data.frame(monthlyfunction(tmin_proj4[,4:16], FUN=mean, na.rm=TRUE,dates=dat_2))

avgT_mod_proj1=0.5*(colMeans(tmax_proj_mon1)+colMeans(tmin_proj_mon1))
avgT_mod_proj2=0.5*(colMeans(tmax_proj_mon2)+colMeans(tmin_proj_mon2))
avgT_mod_proj3=0.5*(colMeans(tmax_proj_mon3)+colMeans(tmin_proj_mon3))
avgT_mod_proj4=0.5*(colMeans(tmax_proj_mon4)+colMeans(tmin_proj_mon4))

op <- par(mfrow = c(2,2),mar=c(4,4,1,1), oma=c(1.5,2,1,1))
barplot((avgT_mod_proj1-avgT_mod),ylab="Change of Temperature(c)", main="RCP2.6 46-65")
barplot((avgT_mod_proj2-avgT_mod),ylab="Change of Temperature(c)", main="RCP8.5 46-65")
barplot((avgT_mod_proj3-avgT_mod),ylab="Change of Temperature(c)", main="RCP2.6 76-95")
barplot((avgT_mod_proj4-avgT_mod),ylab="Change of Temperature(c)", main="RCP8.5 76-95")


##writing rain.data and tmaxtmintdew.dat for historical data #########


tmaxtmintdew_his=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:len){
  tmaxtmintdew_his=cbind(tmaxtmintdew_his,tmax_final[,k],tmin_final[,k],tmin_final[,k]-0.5)
}

temper_his=data.frame(tmaxtmintdew_his[,-1])
precip_his=data.frame(rainsim[,1:(ncol(rainsim))])

precip_his[] <- lapply(precip_his, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
temper_his[] <- lapply(temper_his, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
strDates_his=rbind(data.frame(date=dates_bc))
gh_his=gsub("-","", strDates_his$date, fixed=TRUE)
day_his=data.frame(time=gh_his)
hour_his=rep.int(240000, nrow(day_his))
precip_his=data.frame(precip_his,day_his,hour_his)
temper_his=data.frame(temper_his,day_his,hour_his)

setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data\\",Model,sep=""))##for A1:

##writing rain.dat file
ra_file_his=paste('rain_his_',Model,".dat",sep="")
sink(ra_file_his) 
cat(sprintf("This file provides daily precipitation rate for each rain station"),file=ra_file_his,append=TRUE)
cat("\n", file=ra_file_his, append=TRUE)
cat(sprintf("Precipitation is provided in mm/day"),file=ra_file_his,append=TRUE)
cat("\n", file=ra_file_his, append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file=ra_file_his,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=ra_file_his,append=TRUE) 
cat("\n", file=ra_file_his, append=TRUE)
write.table(precip_his, file =ra_file_his,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()



##writing tmaxtmintdew.dat file
tmp_file_his=paste('tmaxtmintdew_his_',Model,".dat",sep="")
sink(tmp_file_his)
cat(sprintf("This file provides daily values of Tmax/Tmin/Tdew for each temperature station"),file=tmp_file_his,append=TRUE)
cat("\n", file=tmp_file_his, append=TRUE)
cat(sprintf("Temperature is provided in degrees Celcius"),file=tmp_file_his,append=TRUE)
cat("\n", file=tmp_file_his, append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%s %d ", "ver2",nc),file=tmp_file_his,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=tmp_file_his,append=TRUE) 
cat("\n", file=tmp_file_his, append=TRUE)
write.table(temper_his, file = tmp_file_his,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()



##########writing rain.dat and tmaxtmintdew.dat for future scenarion###################
tmin_proj1=proj_tmax1-proj_dtr1
tmin_proj2=proj_tmax2-proj_dtr2
tmin_proj3=proj_tmax3-proj_dtr3
tmin_proj4=proj_tmax4-proj_dtr4

tmax_final_proj=cbind(proj_diff,rbind(cbind(proj_tmax1[,4:(len+3)],proj_tmax2[,4:(len+3)]),cbind(proj_tmax3[,4:(len+3)],proj_tmax4[,4:(len+3)])))
tmin_final_proj=cbind(proj_diff,rbind(cbind(tmin_proj1[,4:(len+3)],tmin_proj2[,4:(len+3)]),cbind(tmin_proj3[,4:(len+3)],tmin_proj4[,4:(len+3)])))
rain_final_proj=cbind(proj_diff,rbind(cbind(proj_rain1[,4:(len+3)],proj_rain2[,4:(len+3)]),cbind(proj_rain3[,4:(len+3)],proj_rain4[,4:(len+3)])))


tmaxtmintdew_proj=data.frame(matrix(NA,nrow=1,ncol=1))
for ( k in 1:(rcp*len)){
  tmaxtmintdew_proj=cbind(tmaxtmintdew_proj,tmax_final_proj[,k+3],tmin_final_proj[,k+3],tmin_final_proj[,k+3]-0.5)
}

temper_proj=data.frame(tmaxtmintdew_proj[,-1])
precip_proj=data.frame(rain_final_proj[,4:(len*2+3)])

precip_proj[] <- lapply(precip_proj, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
temper_proj[] <- lapply(temper_proj, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
strDates_proj=rbind(data.frame(date=dat_1),data.frame(date=dat_2))
gh_proj=gsub("-","", strDates_proj$date, fixed=TRUE)
day_proj=data.frame(time=gh_proj)
hour_proj=rep.int(240000, nrow(day_proj))
rcp_name=c("rcp2.6","rcp8.5")


for ( j in 1:rcp){
  precipp=data.frame(precip_proj[,seq((nc*j-nc+1),nc*j,1)])
  
  #precipp=data.frame(precp_rcp_final[,seq((nc*j-nc+1),nc*j,1)])
  
  ##change here as date of 2056-2065 and 2090-2099
  
  precipp[] <- lapply(precipp, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  precipp=data.frame(precipp,day_proj,hour_proj)
  
  
  ##writing rain.dat file
  file=paste("rain_",rcp_name[j],"_",Model,".dat",sep="")
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


for ( j in 1:rcp){
  
  temperpp=data.frame(temper_proj[,seq((nc*j*3-nc*3+1),(nc*j*3),1)])
  temperpp[] <- lapply(temperpp, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  temperpp=data.frame(temperpp,day_proj,hour_proj)
  
  file=paste("tmaxtmintdew_",rcp_name[j],"_",Model,".dat",sep="")
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


###writing boundy flow for historical
bndry_flow=data.frame(x=rep(-999,nrow(day_his)))
bndry_flow[] <- lapply(bndry_flow, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
bndry_flow=data.matrix(bndry_flow)
bndry_flow_his=data.frame(bndry_flow,day_his,hour_his)
bd_his="bndryflow_his.dat"
sink(bd_his)
cat(sprintf("This file provides mean daily values of streamflow"),file=bd_his,append=TRUE)
cat("\n", file=bd_his, append=TRUE)
cat(sprintf("Flow values are provided in m3/sec"),file=bd_his,append=TRUE)
cat("\n", file=bd_his, append=TRUE)
sites=seq(1,1,1)
cat(sprintf("%s %d ", "ver2",1),file=bd_his,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=bd_his,append=TRUE) 
cat("\n", file=bd_his, append=TRUE)
write.table(bndry_flow_his, file =bd_his,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()

bndry_flow=data.frame(x=rep(-999,nrow(day_proj)))
bndry_flow[] <- lapply(bndry_flow, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
bndry_flow=data.matrix(bndry_flow)
bndry_flow_proj=data.frame(bndry_flow,day_proj,hour_proj)


###writing boundy flow for projection
bd_proj="bndryflow_proj.dat"
sink(bd_proj)
cat(sprintf("This file provides mean daily values of streamflow"),file=bd_proj,append=TRUE)
cat("\n", file=bd_proj, append=TRUE)
cat(sprintf("Flow values are provided in m3/sec"),file=bd_proj,append=TRUE)
cat("\n", file=bd_proj, append=TRUE)
sites=seq(1,1,1)
cat(sprintf("%s %d ", "ver2",1),file=bd_proj,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=bd_proj,append=TRUE) 
cat("\n", file=bd_proj, append=TRUE)
write.table(bndry_flow_proj, file =bd_proj,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()




##########wind data processing ###################
##wind data format is different for different model so should change according to the model data format

setwd(paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""))
hist_sfc=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*sfcWind_day_",Model,"_historical",sep=""))
#get index of the nearest lat,lon of the grid ###
sf.nc = nc_open(hist_sfc[1])
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

ws_his=data.frame(matrix(NA,nrow=1,ncol=1+1))

for ( m in 1:length(hist_sfc)){
  wd=wind(hist_sfc[m])
  ne=colnames(wd)
  colnames(ws_his)[]=ne
  ws_his=rbind(ws_his,wd)
}

###filling gap for leap year########

##finding leap year
dates_win_his=seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day") 
dym_wind_his=t(matrix(unlist(lapply(dates_win_his, toNumerics)),nrow=3,ncol=length(dates_win_his)))
dym_wn_his=dym_wind_his[,2:3]
wn_id_his=which(dym_wn_his[,1]==2 & dym_wn_his[,2]==29)
dym_without_leapwn_his=dym_wind_his[-wn_id_his,]
wind_mat_his=matrix(NA,nrow=length(dates_win_his),ncol=1)
wind_mat_his[wn_id_his]=-999
ws_his=ws_his[-1,]
wind_mat_his[is.na(wind_mat_his)]=ws_his[,1]
mtach_wind_his=match(dates_bc,dates_win_his)
wind_mat_cp_his=data.frame(wind_mat_his[mtach_wind_his])

wn_mean_his=mean(colMeans(wind_mat_cp_his,na.rm=TRUE))
wind_mat_cp_his[wind_mat_cp_his<0]=wn_mean_his
all_wind_his=data.frame(wind=wind_mat_cp_his)
wind_mod_his=all_wind_his

##########processing projected wind data ################

sfc_proj=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("*sfcWind_day_",Model,"_rcp",sep=""))
#get index of the nearest lat,lon of the grid ###
wspj=data.frame(matrix(NA,nrow=1,ncol=1+1))

for ( m in 1:length(sfc_proj)){
  wd=wind(sfc_proj[m])
  ne=colnames(wd)
  colnames(wspj)[]=ne
  wspj=rbind(wspj,wd)
}
wspj=wspj[-1,]
wind_rcp_all=cbind(wspj[1:(nrow(wspj)/2),1],wspj[(1+(nrow(wspj)/2)):nrow(wspj),1])

###filling gap for leap year########

##finding leap year
dates_win=seq(as.Date("2040/1/1"), as.Date("2100/12/31"), "day") 
dym_wind=t(matrix(unlist(lapply(dates_win, toNumerics)),nrow=3,ncol=length(dates_win)))
dym_wind=proj_diff
dym_wn=dym_wind[,2:3]
wn_id=which(dym_wn[,1]==2 & dym_wn[,2]==29)
dym_without_leapwn=dym_wind[-wn_id,]
wind_mat=matrix(NA,nrow=nrow(dym_wind),ncol=2)
wind_mat[wn_id,1:2]=-999
wind_mat[is.na(wind_mat)]=wind_rcp_all[,1:2]
wn_meanpj=mean(colMeans(wind_mat[,1:2],na.rm=TRUE))
wind_mat[wind_mat<0]=wn_meanpj
all_wind_proj=data.frame(wind=wind_mat)
wind_mod_proj=all_wind_proj
wind_mod_proj=wind_rcp_all ## in MPI wind data is not missing


setwd(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_watershed_final\\",watsed[w],"_CC_data\\",Model,sep=""))
wind_obs_his=read.table('wind_obs.dat',skip=3)
plot(wind_mod_his[,1],wind_obs_his[,1])
wind_obs_his=data.frame(rep(data.frame(wind_obs_his),2))
wind_obs_his=data.frame(wind_obs_his[,c(1,4)])
wind_mod_his=data.frame(rep(data.frame(wind_mod_his),2))

####bias correction of wind data########

for(i in 1:rcp){
  
  for ( j in 1:365){
    id_his=which(his_dym[,2]==dym[j,1] & his_dym[,3]==dym[j,2])
    id_proj=which(proj_diff[,2]==dym[j,1] & proj_diff[,3]==dym[j,2])
    obs=wind_obs_his[id_his,i]
    sim=wind_mod_his[id_his,i] 
    proj=wind_mod_proj[id_proj,i]
    obs1=sort(obs)
    sim1=sort(sim)
    wind_qm.fit <- fitQmapRQUANT(obs1,sim1,qstep=0.1,nboot=1,wet.day=FALSE)
    his_corr <- doQmapRQUANT(sim,wind_qm.fit,type="linear")
    proj_corr <- doQmapRQUANT(proj,wind_qm.fit,type="linear")
    wind_mod_his[id_his,i]= his_corr
    wind_mod_proj[id_proj,i]=proj_corr
  }
  print(i)
}

wind_obs_his=matrix(wind_obs_his[,1])
wind_mod_his=data.frame(wind_mod_his[,1])
wn_obs_mon<- data.frame(monthlyfunction(wind_obs_his, FUN=mean, na.rm=TRUE,dates=dates_bc)) 
wn_sim_mon<- data.frame(monthlyfunction(all_wind_his, FUN=mean, na.rm=TRUE,dates=dates_bc))
wn_sim_mon_cor<- data.frame(monthlyfunction(wind_mod_his, FUN=mean, na.rm=TRUE,dates=dates_bc)) 

par(mfrow=c(2,2))
plot(colMeans(wn_obs_mon))
lines(colMeans(wn_sim_mon))
lines(colMeans(wn_sim_mon_cor),col='red')

wind_his <- lapply(wind_mod_his, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
wind_speed_his=data.frame(wind_his,day_his,hour_his)




#####writing historical and projected wind data

win_file_his=paste("wind_his_",Model,".dat",sep="")
sink(win_file_his)
cat(sprintf("This file provides daily values of wind speed for each wind station"),file=win_file_his,append=TRUE)
cat("\n", file=win_file_his, append=TRUE)
cat(sprintf("Wind speed is provided in m/sec"),file=win_file_his,append=TRUE)
cat("\n", file=win_file_his, append=TRUE)
sites=seq(1,1,1) ## need to automate this one
cat(sprintf("%s %d ", "ver2",1),file=win_file_his,append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=win_file_his,append=TRUE) 
cat("\n", file=win_file_his, append=TRUE)
write.table(wind_speed_his, file = win_file_his,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()


strDates_wind_proj=data.frame(date=dates_win)
strDates_wind_proj=rbind(data.frame(date=dat_1),data.frame(date=dat_2))
gh_wind_proj=gsub("-","", strDates_wind_proj$date, fixed=TRUE)
day_wind_proj=data.frame(time=gh_wind_proj)
hour_wind_proj=rep.int(240000, nrow(day_wind_proj))


for ( j in 1:rcp){
  windp=data.frame(wind_mod_proj[,rcp])
  windp[] <- lapply(windp, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  windp=data.frame(windp,day_wind_proj,hour_wind_proj)
  
  
  
  file=paste("wind_",rcp_name[j],"_",Model,".dat",sep="")
  sink(file) 
  cat(sprintf("This file provides daily values of wind speed for each wind station"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  cat(sprintf("Wind speed is provided in m/sec"),file=file,append=TRUE)
  cat("\n", file=file, append=TRUE)
  sites=seq(1,1,1) ## need to automate this one
  cat(sprintf("%s %d ", "ver2",1),file=file,append=TRUE) 
  cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file=file,append=TRUE) 
  cat("\n", file=file, append=TRUE)
  write.table(windp, file =file,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
  sink()
  
}





















































































######correcting tdew########
##Estimating tdewfrom specifcic humidity and pressure ######

hist_huss_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("hus_day_",Model,"_historical_",sep=""))
hist_psl_file=list.files(path =paste("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\CLIMATE_CHANGE_ALLDATA\\",Model,sep=""),pattern =paste("ps_cfDay_",Model,"_historical_",sep=""))



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
    start=c(sh_ID[i,1],sh_ID[i,2],1,1)
    count=c(1,1,1,length(hus_time))
    sh[,i]=ncvar_get(hus.nc,'hus',start,count)
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
    sh[,i]=ncvar_get(hus.nc,'ps',start,count)
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
dates_hus=seq(as.Date("1971/1/1"), as.Date("2005/12/31"), "day") 
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
dates_psl=seq(as.Date("1979/1/1"), as.Date("2005/12/31"), "day") 
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





############correcting wind data#########################
##reading historical wind data###########















































