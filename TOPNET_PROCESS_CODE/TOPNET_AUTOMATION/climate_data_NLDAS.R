########processing NLDAS data for rainfall ##############
library(zoo)
library(plyr)
library(hydroTSM)
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Rainfall") # set working directory
   


##Need for data downloading
# boundary box of the study area
bbox=c(46.05,-115.9,46.85,-114.20)
start_time="2008-01-01 00:00" # start time
end_time="2012-12-31 23:00"   #end time
lat=seq(bbox[1],bbox[3],0.125) 
lon=seq(bbox[2],bbox[4],0.125)
Lat_Index = floor((lat - (25.062500))/0.125) # lat index in NLDAS grid
Lon_Index = floor((lon - (-124.9375))/0.125) #lon index in NLDAS grid

x <- seq(bbox[1],bbox[3] , length.out =length(Lat_Index))
y <- seq(bbox[2],bbox[4], length.out = length(Lon_Index) )

x=c(41.4375, 41.5625, 41.6875, 41.8125)##lat NLDAS coordinate
y=c(-117.6875, -117.5625, -117.4375, -117.3125, -117.1875) ##lon NLDAS coordinate
d1 <- expand.grid(y=y,x= x)## to same orientation with processing data
#processing data orientation is 
#lat1                 lat 2
#lon1 lon2 ....lon n  lon1 lon2 ....lon n

library(proj4)
library(rgdal)
library(raster)
library(shapefiles)
#writing rain gauge shape file

##Have to given rain



dd=data.matrix(cbind(d1$y,d1$x))
df=project(dd,"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df2=data.frame(X=df[,1],Y=df[,2])

lots <- SpatialPointsDataFrame( coords = cbind(df2$X,df2$Y), data = df2 ) 
writeOGR( lots, dsn = 'tstShapefile', layer = 'tstShapefile', driver='ESRI Shapefile',overwrite=TRUE) 
##after then you need to add field ID in arcGIS and Export it as a layer then you can run the rain weight 

require(spsurvey)
require(foreign)
lat_lon_rg=read.dbf('rg_A2_NLDAS.dbf')
lat_lon_rg=do.call(cbind,lat_lon_rg)
rg_order=lat_lon_rg[,3]
lat_lon_rg=albersgeod(lat_lon_rg[,1], lat_lon_rg[,2], sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)
len=(nrow(lat_lon_rg))
time_index_all= seq(from = as.POSIXct("1979-01-02 01:00"),to = as.POSIXct("2014-1-19 23:00"), by = "hour") #create time series
time_index_need= seq(from = as.POSIXct(start_time),to = as.POSIXct(end_time), by = "hour") # create time series based on start and end date
date_overlap=match(time_index_need,time_index_all,) # get overlap time interval
t1=date_overlap[1] # start time in  NLDAS time series
t2=date_overlap[length(date_overlap)] # end time index in NLDAS time series
len_timestep=t2-t1+1
num_lat=length(Lat_Index)
num_lon=length(Lon_Index)
# a1=as.character(Lat_Index[1]) # start lat index as character
# a2=as.character(Lat_Index[num_lat]) #end lat index as character
# b1=as.character(Lon_Index[1]) ## start lon index as character
# b2=as.character(Lon_Index[num_lon]) # end lon index as character


##After downloading data using python script we should use below script for processing NLDAS data:


###processign rainfall data ########

num_lat=7
num_lon=14
##read NLDAS data ####
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Rainfall")
##filenames =list.files(path="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_CD\\NLDAS_NOAH",pattern="rain_20+.*txt")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("precipitation_200",i+7,"A2.txt",sep="")
}

dss=matrix(nrow=1,ncol=num_lat*num_lon)
climate <- function(file ){
  mydata=readLines(file)
  info_r_c=unlist(strsplit( mydata[1], split="[][]"))
  time_steps=as.numeric(info_r_c[2])
  mydata1=mydata[2:(time_steps*(num_lat+1))]
  df=mydata1[which(!mydata1 == "" )]
  s1 = unlist(strsplit(df, split=',', fixed=TRUE))
  fg=as.numeric(t(matrix(s1[-seq(1,,num_lon+1,length(s1))],nrow=num_lat*num_lon,ncol=time_steps)))## nrow=num_lat&num_lon, ncol=num_time 5=num_lat+1
  data=matrix(fg,nrow=time_steps,ncol=num_lat*num_lon)
  dss=rbind(dss,data)
  return(dss)
}
rainfall=dss

dates=seq(as.Date("2008/1/1"), as.Date("2012/12/31"), "day")
strDates= as.character(dates)
gh=gsub("-","", strDates, fixed=TRUE)
dss1=data.frame(time=gh)
hr=rep.int(240000, nrow(dss1))



rainfall_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  rainfall=climate(xx[m])
  rainfall_data=rbind(rainfall_data,rainfall)
}
rainfalldata=data.frame(rainfall_data)
rainfalldata=na.omit(rainfalldata)
#replace missing data with NA
rainfalldata[rainfalldata >500000]=NA
precipitation<- rollapply(rainfalldata,24,(sum),by=24,by.column=TRUE,align='right',na.rm=TRUE)
precip=data.frame(precipitation)

# 
# ##find matching data for Bufferingwatershed
# ##need to coding 
# latlonall=data.frame(lon=d1$y,lat=d1$x)
# lat_lon_rg=data.frame(lat=lat_lon_rg$la,lat=lat_lon_rg$lat)
# nodata=matrix(rep(NA),nrow=13,ncol=2)
# nodata=data.frame(lon=nodata[,1],lat=nodata[,2])
# latlonrg=rbind(lat_lon_rg,nodata)
# lat_lon_match=merge(latlonall, lat_lon_rg1,by.x=c("lon", "lat"), by.y=c("lon", "lat"))
# vv=merge( lat_lon_rg, latlonall,by = c("lon","lat"))

##
##for A1
##order should be : 8,9,10,12,13,14,16,17
##rg_order=c(8,9,12,13,14,17,18)
precip=precip[,rg_order]
precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
precip=data.frame(precip,dss1,hr)

# RAIN=precipitation+pprecipitation
# pdates=seq(as.Date("2010/1/1"), as.Date("2012/12/31"), "day")
# plot(pdates,RAIN[732:1827,15])
# lines(pdates,pprecipitation[732:1827,15])
# precip=data.frame(RAIN)
# precip[] <- lapply(RAIN, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
# precip=data.frame(precip,dss1,hr)
##writing rain.dat file

sink("rain.dat") 
cat(sprintf("This file provides daily precipitation rate for each rain station"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
cat(sprintf("Precipitation is provided in mm/day"),file='rain.dat',append=TRUE)
cat("\n", file="rain.dat", append=TRUE)
sites=seq(1,(length(rg_order)),1)
cat(sprintf("%s %d ", "ver2",length(rg_order)),file='rain.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='rain.dat',append=TRUE) 
cat("\n", file="rain.dat", append=TRUE)
write.table(precip, file = "rain.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()


###processign temperature  data ########
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Temperature")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("meantemp_200",i+7,"A2.txt",sep="")
}



temp_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  temp=climate(xx[m])
  temp_data=rbind(temp_data,temp)
}
tempdata=data.frame(temp_data)
tempdata=na.omit(tempdata)
#replace missing data with NA0
tempdata[tempdata >500000]=NA
tmax<- rollapply(tempdata,24,(max),by=24,by.column=TRUE,align='right',na.rm=TRUE)
tmin<- rollapply(tempdata,24,(min),by=24,by.column=TRUE,align='right',na.rm=TRUE)
tavg<- rollapply(tempdata,24,(mean),by=24,by.column=TRUE,align='right',na.rm=TRUE)


###processing pressure data ###
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Pressure")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("pressure_200",i+7,"A2.txt",sep="")
}

press_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  press=climate(xx[m])
  press_data=rbind(press_data,press)
}
pressdata=data.frame(press_data)
pressdata=na.omit(pressdata)
#replace missing data with NA
pressdata[pressdata >500000]=NA


###processing speific humidity data #####
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Sfh")
#filenames =list.files(path="E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/B21_Watershed/B21_CD_present/B21_NLDAS/B21_N_Sfh",pattern="spfh+.*txt")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("sfh_200",i+7,"A2.txt",sep="")
}

sfh_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  sfh=climate(xx[m])
  sfh_data=rbind(sfh_data,sfh)
}
sfhdata=data.frame(sfh_data)
sfhdata=na.omit(sfhdata)
#replace missing data with NA
sfhdata[sfhdata >500000]=NA

####calculating dewT ######
#dewT are caluated using information contains on below link:
#http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html
p=pressdata/100 #convert in mb
q=sfhdata
vapor_pressure=p*q/(0.622+0.378*q)  #unit is in mb
dewT=(log(vapor_pressure/(6.11)))*243.5/(17.67-log(vapor_pressure/(6.11)))
daily_dewT=rollapply(dewT,24,(mean),by=24,by.column=TRUE,align='right',na.rm=TRUE)


tmaxtmintdew=matrix(ncol = 1, nrow = nrow(dss1))
for ( k in 1:(length(rg_order))){
  
  tmaxtmintdew=cbind(tmaxtmintdew,tmax[,rg_order[k]]-273.16,tmin[,rg_order[k]]-273.16,daily_dewT[,rg_order[k]])
}

temper=data.frame(tmaxtmintdew[,-1])
#temper=temper[,rg_order]
temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
temper=data.frame(temper,dss1,hr)

###writing temperature data ################

sink('tmaxtmintdew.dat')
cat(sprintf("This file provides daily values of Tmax/Tmin/Tdew for each temperature station"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
cat(sprintf("Temperature is provided in degrees Celcius"),file='tmaxtmintdew.dat',append=TRUE)
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
sites=seq(1,(length(rg_order)),1)
cat(sprintf("%s %d ", "ver2",length(rg_order)),file='tmaxtmintdew.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='tmaxtmintdew.dat',append=TRUE) 
cat("\n", file="tmaxtmintdew.dat", append=TRUE)
write.table(temper, file = "tmaxtmintdew.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()

###creating clipar.dat#############
ng=length(rg_order)
station_monthly<- data.frame(monthlyfunction(tavg[,rg_order]-273.16, FUN=mean, na.rm=TRUE,dates=dates))
sta_ID=seq(1,ng,1)
sta_ele=rep(-999,ng)
station_monthly=cbind(sta_ID,lat_lon_rg[,2],lat_lon_rg[,1],sta_ele,station_monthly)
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




####processing wind data###################

#uwind data ##
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Uwind")
#filenames =list.files(path="E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/C21_Watershed/C21_CD/C21_CD_present/C21_NLDAS/C21_N_Uwind",pattern="var2+.*txt")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("uwind_200",i+7,"A2.txt",sep="")
}


uwind_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  uwind=climate(xx[m])
  uwind_data=rbind(uwind_data,uwind)
}
uwinddata=data.frame(uwind_data)
uwinddata=na.omit(uwinddata)
#replace missing data with NA
uwinddata[uwinddata>500000]=NA


##vwind data ##
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Vwind")
#filenames =list.files(path="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\B22_Watershed\\B22_CD\\B22_CD_NLDAS\\Uwind",pattern="ugrd+.*txt")
xx='file'
nc=5
for (i in 1:nc){
  xx[i]=paste("vwind_200",i+7,"A2.txt",sep="")
}


vwind_data=matrix(nrow=1,ncol=num_lat*num_lon)
for ( m in 1:length(xx)){
  vwind=climate(xx[m])
  vwind_data=rbind(vwind_data,vwind)
}
vwinddata=data.frame(vwind_data)
vwinddata=na.omit(vwinddata)
#replace missing data with NA
vwinddata[vwinddata >500000]=NA

wind_data=abs(sqrt(vwinddata^2+uwinddata^2))
wind_daily=rollapply(wind_data,24,(mean),by=24,by.column=TRUE,align='right',na.rm=TRUE)

wind=data.frame(wind_daily)
wind=wind[,rg_order]
wind=data.frame(wind=rowMeans(wind))

wind[] <- lapply(wind, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
wind=data.frame(wind,dss1,hr)



###writing wind data ####################


sink('wind.dat')
cat(sprintf("This file provides daily values of wind speed for each wind station"),file='wind.dat',append=TRUE)
cat("\n", file="wind.dat", append=TRUE)
cat(sprintf("Wind speed is provided in m/sec"),file='wind.dat',append=TRUE)
cat("\n", file="wind.dat", append=TRUE)
sites=seq(1,1,1)
cat(sprintf("%s %d ", "ver2",1),file='wind.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='wind.dat',append=TRUE) 
cat("\n", file="wind.dat", append=TRUE)
write.table(wind, file = "wind.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()









###processing rainweight data #############

# library(proj4)
# library(rgdal)
# library(raster)
# 
# #writing rain gaueg shape file
# 
# ##Have to given rain
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_NLDAS\\A2_CD_present\\A2_CD_NLDAS\\Rainfall")
# 
# dd=data.matrix(cbind(d1$y,d1$x))
# df=project(dd,"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
# df2=data.frame(X=df[,1],Y=df[,2])
# 
# lots <- SpatialPointsDataFrame( coords = cbind(df2$X,df2$Y), data = df2 ) 
# writeOGR( lots, dsn = 'tstShapefile', layer = 'tstShapefile', driver='ESRI Shapefile',overwrite=TRUE) 
# 
# 
# ##after then you need to add field ID in arcGIS and Export it as a layer then you can run the rain weight 

system ("rainweight  -w demA22me.tif     -rg rg_A2_NLDAS.shp -ar annrain.tif  -tri triout.tif -wt weights.txt")   


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


##Generatign latlonfrom xy.txt

long_fit=lm(dd[,1]~df[,1])
lo_f1=long_fit$coefficients[1]
lo_f2=long_fit$coefficients[2]
lat_fit=lm(dd[,2]~df[,2])
la_f1=lat_fit$coefficients[1]
la_f2=lat_fit$coefficients[2]
sink("latlongfromxy.txt") 
cat(sprintf("This file provides the parameters for conversion from local X,Y coordinates to latitude and longitude coordinates used in the solar radiation calculations."),file='latlongfromxy.txt',append=TRUE)
cat("\n", file="latlongfromxy.txt", append=TRUE)
cat(sprintf("A linear transformation is assumed to be adequate.Equations used:latitude=(flat)*Y+(clat);longitude=(flong)*X+(clong).See the file latlongfromxy.xls for calculation of these values from rain gage x, y, lat and long coordinates"),file='latlongfromxy.txt',append=TRUE)
cat("\n", file="latlongfromxy.txt", append=TRUE)
cat(sprintf("%g %s %s", la_f2,"flat","Latitude factor"),file='latlongfromxy.txt',append=TRUE) 
cat("\n", file="latlongfromxy.txt", append=TRUE)
cat(sprintf("%1.6f %s %s", la_f1,"clat","Latitude constant"),file='latlongfromxy.txt',append=TRUE) 
cat("\n", file="latlongfromxy.txt", append=TRUE)    
cat(sprintf("%g %s %s", lo_f2,"flong ","Longitude factor"),file='latlongfromxy.txt',append=TRUE) 
cat("\n", file="latlongfromxy.txt", append=TRUE)       
cat(sprintf("%1.6f %s %s", lo_f1,"clong","Longitude constant"),file='latlongfromxy.txt',append=TRUE) 
cat("\n", file="latlongfromxy.txt", append=TRUE)       
sink()    

