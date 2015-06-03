library(proj4)
library(rgdal)
library(raster)
library(shapefiles)
library(RNCEP)
library(zoo)
library(hydroTSM)
require(spsurvey)


setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C22_Watershed_final\\C22_CD')
x <- data.frame(lon=seq(-125.020833333,-66.4791666662 , 0.041666667))
y <- data.frame(lat=seq(49.9375,24.0624999998, -0.041666667))

d1=expand.grid(x=x,y=y)

#writing rain gauge shape file

dd=data.matrix(cbind(d1$x,d1$y))

##changeing projections systems either albers or UTM zone 

#df=project(dd,"+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df=project(dd,"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df2=data.frame(X=df[,1],Y=df[,2])
lots <- SpatialPointsDataFrame( coords = cbind(df2$X,df2$Y), data = df2 ) 
writeOGR( lots, dsn = 'tstShapefile1', layer = 'tstShapefile1', driver='ESRI Shapefile',overwrite=TRUE) 


##make buffer around watershed about 4 km and clip this tstshape file and get the raingauge shape file

lat_lon_rg=read.dbf('rg_C22_PRISM.dbf')
lat_lon_rg=do.call(cbind,lat_lon_rg)

##changing projection either albers or UTM zone

lat_lon_rg1=albersgeod(lat_lon_rg[,1], lat_lon_rg[,2], sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)
lon1=signif(lat_lon_rg1$lon,5)
lon_all=signif(x$lon,5)
match_lon=match(lon1,lon_all)
lat1=signif(lat_lon_rg1$lat,4)
lat_all=signif(y$lat,4)
match_lat=match(lat1,lat_all)
ng=length(match_lat)
my_data=matrix(NA,nrow=1,ncol=ng)

###processing rainfall data from PRISM######
##from 2007-2012
setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Rain')
setwd("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Rain")
setwd('F:\\test')
##unzip all files

filelist=list.files(path = "F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmin",pattern ='*.zip' )
for ( i in 1:length(filelist)){
unzip(filelist[i])
}
##DONE

daily_file=list.files(path = "F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Rain",pattern ="_asc.asc$")
daily_file=list.files(path = "F:\\test",pattern ="_asc.asc$")

dates=seq(as.Date("1997/1/1"), as.Date("2005/12/31"), "day")
strDates= as.character(dates)
gh=gsub("-","", strDates, fixed=TRUE)
dss=data.frame(time=gh)
hr=rep.int(240000, nrow(dss))

#lat_lon_watershed=cbind(match_lat,match_lon)
#mydata=data[match_lat,t(match_lon)]

###change the rain gauge number of the


climate <- function(file){
mydata2=t(matrix(scan(file,skip=6),nrow=1405,ncol=621))
##change the number of gauge in a file  
ng=34
for ( i in 1:ng){my_data[i]=mydata2[match_lat[i],match_lon[i]]}
climate=data.frame(climate=my_data)
return(climate)
}
ng=34
rain_daily=sapply(daily_file,climate,simplify="array")
##change the number of row according to gauge number
Daily_Rainfall=as.numeric(t(matrix(rain_daily,nrow=ng,ncol=length(dates))))
precip=matrix(Daily_Rainfall,length(dates),ng)
precip=data.frame(precip)
precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
precip=data.frame(precip,dss[,1],hr)

####processsing tmax #################
setwd("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmax")
##unzip all files

#filelist=list.files(path = "E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmax",pattern ='*.zip' )
#for ( i in 1:length(filelist)){unzip(filelist[i])}
##DONE
daily_file_tmax=list.files(path = "F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmax",pattern ="_asc.asc$")
tmax_daily=sapply(daily_file_tmax,climate,simplify="array")
##change the number of row according to gauge number
ng=34
Daily_Tmax=as.numeric(t(matrix(tmax_daily,nrow=ng,ncol=length(dates))))
tmax=matrix(Daily_Tmax,length(dates),ng)
tmax=data.frame(tmax)
tmax= lapply(tmax, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
tmax=data.frame(tmax)



####processsing tmin #################
setwd("F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmin")
##unzip all files

#filelist=list.files(path = "E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmin",pattern ='*.zip' )
#for ( i in 1:length(filelist)){unzip(filelist[i])}
##DONE
daily_file_tmin=list.files(path = "F:\\USU_Research_work_update_March_30_2014\\TOPNET PROJECT\\MODEL COMPARISON\\PRISM_daily_data\\Tmin",pattern ="_asc.asc$")
tmin_daily=sapply(daily_file_tmin,climate,simplify="array")
##change the number of row according to gauge number
ng=152
Daily_Tmin=as.numeric(t(matrix(tmin_daily,nrow=ng,ncol=length(dates))))
Daily_Tdew=Daily_Tmin-0.5
tmin=matrix(Daily_Tmin,length(dates),ng)
tdew=matrix(Daily_Tdew,length(dates),ng)
tmin=data.frame(tmin)
tdew=data.frame(tdew)
tmin= lapply(tmin, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
tdew= lapply(tdew, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
tmin=data.frame(tmin)
tdew=data.frame(tdew)
tmax=data.frame(tmax)

tmax_daily=as.matrix(data.frame(tmax),nrow=length(dates),ncol=ng)
tmin_daily=as.matrix(data.frame(tmin),nrow=length(dates),ncol=ng)
tdew_daily=as.matrix(data.frame(tdew),nrow=length(dates),ncol=ng)
ng=34
tmaxtmintdew=matrix(ncol = 1, nrow = nrow(dss))
for ( k in 1:(ng)){
  tmaxtmintdew=cbind(tmaxtmintdew,tmax_daily[,k],tmin_daily[,k],tdew_daily[,k])
}

temper=data.frame(tmaxtmintdew[,-1])
temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
temper=data.frame(temper,dss[,1],hr)




##writing rain.dat file

sink("rain.dat") 
cat(sprintf("This file provides daily precipitation rate for each rain station"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
cat(sprintf("Precipitation is provided in mm/day"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
sites=seq(1,(ng),1)
cat(sprintf("%s %d ", "ver2",ng),file='rain.dat',append=TRUE) 
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
sites=seq(1,(ng),1)
cat(sprintf("%s %d ", "ver2",ng),file='tmaxtmintdew.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='tmaxtmintdew.dat',append=TRUE) 
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
write.table(temper, file = "tmaxtmintdew.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()

###processing wind data#######
library(RNCEP)
dates_wind=data.frame(time=(format(dates,"%Y-%m-%d %H:%M:%S")))


##wind data only for outlet
uwind=matrix(ncol = 1, nrow =2192)
vwind=matrix(ncol = 1, nrow =2192)
## download data for every station, find that they are very close, as it takes long time to download the data, we use only wind data
##for outlet
lat_w=41.53 ##outlet lat
lon_w=-117.417 ##outlet lon
for ( i in 1:2192){
uwind[i,] <- NCEP.interp(variable='uwnd', level=925,lat=lat_w,lon=lon_w,dt=dates_wind$time[i],reanalysis2=TRUE, keep.unpacking.info=TRUE)
vwind[i,] <- NCEP.interp(variable='vwnd', level=925,lat=lat_w,lon=lon_w,dt=dates_wind$time[i],reanalysis2=TRUE, keep.unpacking.info=TRUE)

}

wind_data_daily=((sqrt(uwind^2+vwind^2)))
wind=data.frame(wind=wind_data_daily)
wind <-lapply(wind, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 

wind_speed=data.frame(wind,dss[,1],hr) ##Need to change dss

sink('wind.dat')
cat(sprintf("This file provides daily values of wind speed for each wind station"),file='wind.dat',append=TRUE)
cat("\n", file="wind.dat", append=TRUE)
cat(sprintf("Wind speed is provided in m/sec"),file='wind.dat',append=TRUE)
cat("\n", file="wind.dat", append=TRUE)
sites=seq(1,1,1) ## need to automate this one
cat(sprintf("%s %d ", "ver2",1),file='wind.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='wind.dat',append=TRUE) 
cat("\n", file="wind.dat", append=TRUE)
write.table(wind_speed, file = "wind.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()

setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C22_Watershed_final\\C22_PRISM')

###creating rainweights#############
system ("rainweight  -w demC22me.tif     -rg rg_C22_PRISM.shp -ar annrain.tif  -tri triout.tif -wt weights.txt")   


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


###creating clipar.dat#############

t1=as.matrix(data.frame(tmin))
t1=as.numeric(t1)
t1=matrix(t1,nrow=length(dates),ncol=ng)

t2=as.matrix(data.frame(tmax))
t2=as.numeric(t2)
t2=matrix(t2,nrow=length(dates),ncol=ng)
avgT=0.5*(t1+t2)
station_monthly<- data.frame(monthlyfunction(avgT, FUN=mean, na.rm=TRUE,dates=dates))
sta_ID=seq(1,ng,1)
sta_ele=rep(-999,ng)
station_monthly=cbind(sta_ID,lat_lon_rg1[,2],lat_lon_rg1[,1],sta_ele,station_monthly)
##lapse_mat=cbind(Ele_station,station_annual)
##dddd=lm(lapse_mat[,2]~lapse_mat[,1])
##station_monthly[,4]=lapse_mat[,1]
station_monthly[] <- lapply(station_monthly, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 

#writng clipar.dat
sink("clipar.dat") 
cat(sprintf("This file provides meta-data for each temperature station"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
cat(sprintf("Temperature ranges are in Degrees Celcius"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
sites=seq(1,(ng),1)
cat(sprintf("%d %s ", ng,"! Number of temperature stations"),file='clipar.dat',append=TRUE) 
cat("\n", file="clipar.dat", append=TRUE)
##please change standard longitude according to watershed location

std_longitude=-105.00;
cat(sprintf("%3.2f %s ",std_longitude,"!Standard Longitude of time zone used in local time calculations"),file='clipar.dat',append=TRUE) 
cat("\n", file="clipar.dat", append=TRUE)
cat(sprintf("'Station_id  Lat    Lon     Elev_m   Jan    Feb    Mar     Apr       May    Jun    Jul      Aug     Sep    Oct      Nov    Dec"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
write.table(station_monthly, file = "clipar.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()








