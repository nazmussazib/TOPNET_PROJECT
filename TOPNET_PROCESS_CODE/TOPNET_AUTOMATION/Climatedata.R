
##Need to make dynamic with number 

#Input:

#(1)Boundary box for interested area
#(2)Long term annual rainfal  grid
#(3)rain gauge shape file
#(4)model element

#Output:

#(1)rain.txt
#(2)tmaxtmintdew.txt
#(3)rainweights.txt
library(RNCEP)
library(zoo)
library(hydroTSM)
library(rgeos)
library(proj4)
library(rgdal)
library(raster)
library(shapefiles)
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_watershed_final\\A1_CD\\DAYMET")

rs=raster('demA1.tif')
fg=rasterToPoints(rs)
bbox=extent(rs)
l1=abs(floor((bbox[1]-bbox[3])/0.072))
l2=abs(floor((bbox[2]-bbox[4])/0.072))

x <- seq(bbox[1],bbox[3] , length.out = l1)
y <- seq(bbox[2],bbox[4], length.out = l2)
d1 <- expand.grid(x = x, y = y)



#writing rain gauge shape file

##Have to given rain

dd=data.matrix(cbind(d1$y,d1$x))

##changeing projections systems either albers or UTM zone 

#df=project(dd,"+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df=project(dd,"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df2=data.frame(X=df[,1],Y=df[,2])
lots <- SpatialPointsDataFrame( coords = cbind(df2$X,df2$Y), data = df2 ) 
writeOGR( lots, dsn = 'tstShapefile', layer = 'tstShapefile', driver='ESRI Shapefile',overwrite=TRUE) 
##after then you need to add field ID in arcGIS and Export it as a layer then you can run the rain weight 

all_data=readOGR("nws_precip_allpoint.shp",layer="nws_precip_allpoint")

arwatershed=crop(ce,all_data,"cliamte.tif",overwrite=TRUE) # crop for the watershed area but got some pixel value out side boundary
sum_ar_watrshed= overlay(all_data,ce, fun=function(x,y){return(x+y)}) # sumof watershed and croped rainfall 
annual_rain_watershed=overlay(sum_ar_watrshed, ce, fun=function(x,y){return(x-y)}) # subtract (sum of watershed and croped rainfall) to watershed for getting annural rainfall for watrshed
annrain_watershed=writeRaster(annual_rain_watershed,"annrain2.tif",datatype='INT4S',options="COMPRESS=NONE") ## change datatype which will change 64 bit to 32 bit


require(spsurvey)
lat_lon_rg=read.dbf('rg_C21_dm.dbf')
lat_lon_rg=do.call(cbind,lat_lon_rg)

##changing projection either albers or UTM zone

lat_lon_rg1=albersgeod(lat_lon_rg[,1], lat_lon_rg[,2], sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)
#SP <- SpatialPoints(lat_lon_rg, proj4string=CRS("+proj=utm +zone=18"))
#lat_lon_rg1=data.frame(spTransform(SP, CRS("+proj=longlat")))
#lat_lon_rg1=signif(lat_lon_rg1,5)
len=(nrow(lat_lon_rg))
for(i in 1:len){
  cat("filename",i,".csv", file="latlon.txt", sep="",append=TRUE)
  cat(",", file="latlon.txt", append=TRUE)
  cat(lat_lon_rg1[i,2], lat_lon_rg1[i,1], file="latlon.txt", sep=",",append=TRUE)
  cat(",", file="latlon.txt", append=TRUE)
  cat("ignore stuff",i, file="latlon.txt", sep="",append=TRUE)
  cat("\n", file="latlon.txt", append=TRUE)}

system("java -Xms512m -Xmx1024m -jar daymet_multiple_extraction.jar latlon.txt")


xx="file"
nc=33
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


###########calculating lapse rate from station information #######

Elevation<- function(file ){
  station_ele = as.matrix(read.csv(file))[3]
  ele = (as.numeric((unlist(strsplit(station_ele, split=' ', fixed=TRUE))[2])))
  return(ele)
}

Ele_station=matrix(ncol =1, nrow = nc)

dates=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day")
strDates= as.character(dates)
gh=gsub("-","", strDates, fixed=TRUE)
dss=data.frame(time=gh)
hr=rep.int(240000, nrow(dss))



for ( m in 1:length(xx)){
  hjk=rainfall(xx[m])
  Ele_station[m,]=Elevation(xx[m])
  dss=cbind(dss,hjk)
}

# #dewpoint temperature is estimated using information on http://www.srh.noaa.gov/images/epz/wxcalc/vaporPressure.pdf
##formula for dew point is here E:\USU_Research_work\TOPNET PROJECT\Miscellaneous\dewpoint_formula
##source is http://www.srh.noaa.gov/images/epz/wxcalc/
# DAYMET give vapor pressure deficit see: http://daymet.ornl.gov/dataaccess
#conversion from specific humidity to dew point temperture 
#http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html

##provide nc based on the shape file of rain gauge
## nc=number of rain gauge


# 
#  dewT=matrix(ncol =nc, nrow = nrow(dss))
# dewT1=matrix(ncol =nc, nrow = nrow(dss))
# hum=matrix(ncol =nc, nrow = nrow(dss))
#  avgT=matrix(ncol = nc, nrow = nrow(dss))
#  satVp=matrix(ncol = nc, nrow = nrow(dss))
# satVpmx=matrix(ncol = nc, nrow = nrow(dss))
# satVpmn=matrix(ncol = nc, nrow = nrow(dss))
# 
#  actVp=matrix(ncol = nc, nrow = nrow(dss))
# actVp1=matrix(ncol = nc, nrow = nrow(dss))
# precipitation=matrix(ncol = nc, nrow = nrow(dss))
# tmaxtmintdew=matrix(ncol = 1, nrow = nrow(dss))
dewT=matrix(ncol =nc, nrow = nrow(dss))
actVp=matrix(ncol = nc, nrow = nrow(dss))
precipitation=matrix(ncol = nc, nrow = nrow(dss))
tmaxtmintdew=matrix(ncol = 1, nrow = nrow(dss))

for (j in 1:(nc)){for (i in 1:nrow(dss))
{
  
  actVp[i,j]=log(dss[i,(4*j+1)]/(1000*0.6108))# convert vapor pressure to kpa
  dewT[i,j]=(actVp[i,j]*237.3)/(17.27-actVp[i,j])
}
}

min_temp=dss[,seq(4,ncol(dss),4)]
max_temp=dss[,seq(3,ncol(dss),4)]
avgT=0.5*(max_temp+min_temp)
# dewT[dewT>min_temp]=min_temp[dewT>min_temp]
# 
# for (j in 1:(nc)){for (i in 1:nrow(dss))
#   {
#     avgT[i,j]=0.5*(dss[i,4*j-1]+dss[i,4*j])
#     
#     
#     satVpmx[i,j]=6.112*exp(17.67*dss[i,(4*j-1)]/(dss[i,(4*j-1)]+243.5)) # unit is hecto-pascal
#     satVpmn[i,j]=6.112*exp(17.67*dss[i,4*j]/(dss[i,4*j]+243.5)) # unit is hecto-pascal
#     
#     #satVp[i,j]=6.112*exp(17.67*avgT[i,j]/(avgT[i,j]+243.5)) # unit is hecto-pascal
#     
#     satVp[i,j]=0.5*(satVpmx[i,j]+ satVpmn[i,j])
#     
#     
#     actVp[i,j]=(satVp[i,j]-(dss[i,(4*j+1)]/100)) # convert vapor pressure deficit pa to hpa
#     
#     actVp1[i,j]=(dss[i,(4*j+1)]/1000)
#     hum[i,j]=actVp[i,j]/ satVp[i,j]
#    
#    #dewT[i,j]=(log(actVp[i,j]*100/(6.11)))*243.5/(17.67-log(actVp[i,j]*100/(6.11)))
#    dewT[i,j]=(116.91+237.3*log(actVp[i,j]))/(16.78-log(actVp[i,j]))
#    dewT1[i,j]=(116.91+237.3*log(actVp1[i,j]))/(16.78-log(actVp1[i,j]))
#    
# }
#   }
# 
# 
# dewT[is.na(dewT)]=min_temp[is.na(dewT)]



##may be we can work to imporve above code,so that it can taje less time

##finding monthly average for clipar.dat

station_monthly<- data.frame(monthlyfunction(avgT, FUN=mean, na.rm=TRUE,dates=dates))
station_annual<- t(as.matrix(annualfunction(avgT, FUN=mean, na.rm=TRUE,dates=dates)))
sta_ID=seq(1,nc,1)
sta_ele=rep(-999,nc)
station_monthly=cbind(sta_ID,lat_lon_rg1[,2],lat_lon_rg1[,1],sta_ele,station_monthly)
lapse_mat=cbind(Ele_station,station_annual)
dddd=lm(lapse_mat[,2]~lapse_mat[,1])
station_monthly[,4]=lapse_mat[,1]
station_monthly[] <- lapply(station_monthly, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 

#writng clipar.dat
sink("clipar.dat") 
cat(sprintf("This file provides meta-data for each temperature station"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
cat(sprintf("Temperature ranges are in Degrees Celcius"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
sites=seq(1,(nc),1)
cat(sprintf("%d %s ", nc,"! Number of temperature stations"),file='clipar.dat',append=TRUE) 
cat("\n", file="clipar.dat", append=TRUE)
##please change standard longitude according to watershed location

std_longitude=-120.00;
cat(sprintf("%3.2f %s ",std_longitude,"!Standard Longitude of time zone used in local time calculations"),file='clipar.dat',append=TRUE) 
cat("\n", file="clipar.dat", append=TRUE)
cat(sprintf("'Station_id  Lat    Lon     Elev_m   Jan    Feb    Mar     Apr       May    Jun    Jul      Aug     Sep    Oct      Nov    Dec"),file='clipar.dat',append=TRUE)
cat("\n", file="clipar.dat", append=TRUE)
write.table(station_monthly, file = "clipar.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()





for ( k in 1:(nc)){
precipitation[,k]=dss[,4*k-2]
tmaxtmintdew=cbind(tmaxtmintdew,dss[,4*k-1],dss[,4*k],dewT[,k])
}




precip=data.frame(precipitation)
temper=data.frame(tmaxtmintdew[,-1])

precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})

precip=data.frame(precip,dss[,1],hr)
temper=data.frame(temper,dss[,1],hr)


##writing rain.dat file

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



plot(rowMeans(dewT),rowMeans(min_temp))
plot(rowMeans(dewT1),rowMeans(min_temp))
plot(rowMeans(a))


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



##getting wind data
##Now wind data is extracted using RNCEP pakage but NLDAS data should be used
##Need to work further about wind data

dates_w=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day")
strDates_w= as.character(dates_w)
gh_w=gsub("-","", strDates_w, fixed=TRUE)
dss_w=data.frame(time=gh_w)
hr_w=rep.int(240000, nrow(dss_w))

##Please change lat lon and get data
#bbox=c(34.60,-119.90,34.70,-119.7)
#bbox=c(33.1,-83.75,33.25,-83.65)
#bbox=c(29.18,-97.83,29.4,-97.4)
#bbox=c(36.33,-120.63,36.44,-120.43)
#bbox=c(28.3,-97.95,28.75,-97.21)
#bbox=Bi third -103.83333, 45.75000 by -101.33333, 46.66667
#bbox=c(45.75,-99.25,46.5,-98.25)
#bbox=c(41.48,-117.6,41.80,-117.25)
#bbox=-115.85,-114.13, 46, 46.75
#bbox=c(42.52,-75.94,42.87,-75.67)
#bbox=c(42.67,-124.16,42.91,-123.82)
#bbox=c(30.13,-84.67,30.35,-84.45)
u_wind <- NCEP.gather(variable='uwnd.sig995',level='surface',months.minmax=c(1,12), years.minmax=c(1980,2012),lat.southnorth=c(41.9,41.4), lon.westeast=c(360-117.6,360-117.2),reanalysis2 = FALSE, return.units = TRUE)
v_wind <- NCEP.gather(variable='vwnd.sig995',level='surface',months.minmax=c(1,12), years.minmax=c(1980,2012),lat.southnorth=c(41.9,41.4), lon.westeast=c(360-117.6,360-117.2),reanalysis2 = FALSE, return.units = TRUE)
wx.df <- NCEP.array2df(wx.data=list(u_wind , v_wind),var.names=c('Uwind', 'Vwind'))
wind_six_hours=data.frame(time=wx.df$datetime,wind=(sqrt(wx.df$Uwind^2+wx.df$Vwind^2)))
wind_data_h=t(matrix(wind_six_hours$wind,nrow=24,ncol=289296/24))

wind_data_Daily <- rollapply(wind_data_h,4,(mean),by=4,by.column=TRUE,align='right')
wind=data.frame(rowMeans(wind_data_h) )
wind <- lapply(wind, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 

wind_speed=data.frame(wind,dss_w,hr_w) ##Need to change dss
wind_speed=data.frame(wind,dss_w,hr_w) ##Need to change dss
#wx.interp <- NCEP.interp(variable=c('uwnd.sig995','vwnd.sig995'), level='surface', dt=c(time_wind[12600],time_wind[12612],time_wind[12615]),lat=55.1,lon=11.3, interp='linear')

##writing wind data

##Comments: Number of Stream flow station data and wind station data should be similar ( I experienced that it does not work if the number of stations are different)

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




##Getting streamflow data

#install.packages(c("zoo","XML","RCurl"))
#install.packages("dataRetrieval", repos="http://usgs-r.github.com", type="source")
library(zoo)
library(XML)
library(RCurl)
library(dataRetrieval)

#Get daily data

siteNumber=c("07014500") 

streamflow_Daily = getDVData(siteNumber,"00060","1980-01-01","2012-12-31") 

streamflow=data.frame(Q=streamflow_Daily$Q)

##there are some missing data 
match_index=match(dates,streamflow_Daily$Date)
match_flow=data.frame(streamflow$Q[match_index])
match_flow[] <- lapply(match_flow, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
streamflow=data.matrix(match_flow)
streamflow[is.na(streamflow)] <- -999
observed_flow=data.frame(streamflow,dss[,1],hr)



plot(streamflow_Daily$Date,streamflow_Daily$Q,type="l",xlab="Date",ylab="Discharge m3/s")
#title('Logan River Daily Discharge')


##writing streamflow data

sink('streamflow_calibration.dat')
cat(sprintf("This file provides mean daily values of streamflow"),file='streamflow_calibration.dat',append=TRUE)
cat("\n", file="streamflow_calibration.dat", append=TRUE)
cat(sprintf("Flow values are provided in m3/sec"),file='streamflow_calibration.dat',append=TRUE)
cat("\n", file="streamflow_calibration.dat", append=TRUE)


##Need to add one line be carefule *1202020 etc

sites=seq(1,length(siteNumber),1)
cat(sprintf("%s %d ", "ver2",length(siteNumber)),file='streamflow_calibration.dat',append=TRUE) 
cat(sprintf( "%d", sites),(sprintf( "%s", "Date Hour","\n")),file='streamflow_calibration.dat',append=TRUE) 
cat("\n", file="streamflow_calibration.dat", append=TRUE)
write.table(observed_flow, file = "streamflow_calibration.dat",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
sink()


###writing boundy flow 
bndry_flow=data.frame(x=rep(-999,nrow(dss)))
bndry_flow[] <- lapply(bndry_flow, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
bndry_flow=data.matrix(bndry_flow)
bndry_flow=data.frame(bndry_flow,dss[,1],hr)

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




##Generating rainweights.txt

# input:
#(1)annual rainfall grid
#(2)shape file (rain gauge) (Using ArcGIS)
#(3)model element

#Output:
#(1)

arus=raster('PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil') # annual rainfall for whole united states downloaded from PRISM
ar_proj=projection(arus)
newproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#newproj= "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
arus_projected=projectRaster(arus, crs=newproj)
watershed=raster("demC22me.tif") # my model element
arus_tif=writeRaster(arus_projected,"arus.tif") # write raster to 
arwatershed=crop(arus_tif,watershed,"arwatershed.tif",overwrite=TRUE) # crop for the watershed area but got some pixel value out side boundary
arwat_samcell= resample(arwatershed, watershed, method='bilinear') # make same cell size as watershed
sum_ar_watrshed= overlay(arwat_samcell, watershed, fun=function(x,y){return(x+y)}) # sumof watershed and croped rainfall 
annual_rain_watershed=overlay(sum_ar_watrshed, watershed, fun=function(x,y){return(x-y)}) # subtract (sum of watershed and croped rainfall) to watershed for getting annural rainfall for watrshed
annrain_watershed=writeRaster(annual_rain_watershed,"annrain.tif",datatype='FLT4S',options="COMPRESS=NONE") ## change datatype which will change 64 bit to 32 bit

system ("rainweight  -w demC21me.tif     -rg rg_C21_dm.shp -ar annrain.tif  -tri triout.tif -wt weights.txt")   


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
dd=lat_lon_rg1[,c(1,2)]
df=lat_lon_rg[,1:2]
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
    
    
    









