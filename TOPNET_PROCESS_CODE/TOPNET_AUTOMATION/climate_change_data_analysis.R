##Future climate change dta processing########
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\B1_Watershed_final\\B1_CC_data\\year_2056_2065\\Pr_Tmax_Tmin\\RCp85")

##Run matlab script pr_tmax_tmin for getting text file, i am using matlab script as its flexible for using matlab for opeing and
#reading netcdf file

ptfile=t(matrix(scan("prtmaxtmin_B1_rcp85211.txt"),nrow=2,ncol=3653*3)) ## change row number according to number of gauge; A1=9
rain=ptfile[1:3653,]
tmax=data.frame(ptfile[3654:7306,])
tmin=data.frame(ptfile[7307:10959,])

##based on the previous data(1980-2012) try to regression between tmin and tdew, but using daymet data for regression. Based on the data determine tdew.

##Please change the nrow different for watershed

nc=50
temp_pre=data.frame(t(matrix(scan("tmaxtmintdew_1980_2012.dat",skip=3),nrow=nc*3+2,ncol=12054)))## change row number tmaxmintdew for 1980-2012 from daymet data

tmin_pre=matrix(ncol =1, nrow=12054)
tdew_pre=matrix(ncol =1, nrow=12054)

##please cahgne nc value based on number of gauge



for ( k in 1:nc){
 
  tmin_pre=cbind(tmin_pre,temp_pre[,3*k-1])
  tdew_pre=cbind(tdew_pre,temp_pre[,3*k])
  
}

tmin_pre=(rowMeans(tmin_pre[,-1]))
tdew_pre=(rowMeans(tdew_pre[,-1]))

tmin_tdew=lm(tdew_pre~tmin_pre)
intercept=tmin_tdew$coefficients[1]
coeff=tmin_tdew$coefficients[2]
tdew=data.frame(tmin*coeff+intercept)

dates=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")
strDates= as.character(dates)
gh=gsub("-","", strDates, fixed=TRUE)
dss=data.frame(time=gh)
hr=rep.int(240000, nrow(dss))
tmaxtmintdew=data.frame(matrix(ncol = 1, nrow = nrow(dss)))
nc=2
for ( k in 1:(nc)){
  tmaxtmintdew=cbind(tmaxtmintdew,tmax[,k],tmin[,k],tdew[,k])
}


precip=data.frame(rain)
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

######rain weights for rain gauges###########
rg_ll=matrix(scan("latlon_B1.txt"))
d1 <- expand.grid(x = rg_ll[2:3,1]-360, y = rg_ll[1,1]) ##change row accorfing to lat, lon

library(proj4)
library(rgdal)
library(raster)
library(shapefiles)
#writing rain gauge shape file

##Have to given rain

dd=data.matrix(cbind(d1$x,d1$y))

##changeing projections systems either albers or UTM zone 

#df=project(dd,"+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df=project(dd,"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
df2=data.frame(X=df[,1],Y=df[,2])
lots <- SpatialPointsDataFrame( coords = cbind(df2$X,df2$Y), data = df2 ) 
writeOGR( lots, dsn = 'tstShapefile', layer = 'tstShapefile', driver='ESRI Shapefile',overwrite=TRUE) 
##after then you need to add field ID in arcGIS and Export it as a layer then you can run the rain weight 

system ("rainweight  -w demA22me.tif     -rg rg_A2_CC.shp -ar annrain.tif  -tri triout.tif -wt weights.txt")   


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
###Wind data processing #########
#after running matlab script for extracting wind data 
setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_final\\A2_CC_data\\year_2056_2065\\wind')
dates_wind_all=matrix(seq(as.Date("2055/1/1"), as.Date("2069/12/31"), "day"))
dates_wind=dates_wind_all[-425]## 425,1886,3347,4808 will be deleted for not counting leap year
dates_wind=dates_wind[-1885] #delete 1885 as order is changed
dates_wind=dates_wind[-3345]
dates_wind=dates_wind[-4805]
data_matching=match(dates_wind_all,dates_wind)

wind=matrix(nrow=length(dates_wind_all),ncol=1)
wind_data_all=scan('wind_A2_5665_rcp85211.txt')
wind=matrix(wind_data_all[data_matching])


dates_wind_data=matrix(seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day"))
data_overlap=match(dates_wind_data,dates_wind_all)
wind_2056_2065=wind[data_overlap]
##filling missing data with mean value
wind_2056_2065[which(is.na(wind_2056_2065))]=mean(wind_2056_2065,na.rm=TRUE)
wind=data.frame(wind_2056_2065)
wind <- lapply(wind, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 

dates=seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day")
strDates= as.character(dates)
gh=gsub("-","", strDates, fixed=TRUE)
dss=data.frame(time=gh)
hr=rep.int(240000, nrow(dss))
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




#############run for future time period#########
setwd('E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A2_watershed_final\\A2_CC_RUN')
system(paste("topnet_modified"))
ff=scan("FlowAtStreamNodes_cms.txt", what="")

#l=basin_number+2; for A1 l=29, A2 l=113, B1 l= , B21 l=, B22 l=, C1 l= , C21 l=  , C22 l=
l=113+2
ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
time_all= seq(as.Date("2056/1/1"), as.Date("2065/12/31"), "day") #create time series
time_cali= seq(as.Date("2056/10/1"), as.Date("2065/9/30"), "day") # create time series based on start and end date
date_overlap=match(time_cali,time_all,) # get overlap time interval
simulated_flow=matrix(simu_flow[date_overlap])

plot(time_cali,simulated_flow,type="o", pch=22, lty=2, col="blue", xlab=" Time in days",ylab=" Stream flow (cms)")

dff=lm(sf~simu_flow*0.2)
sf=scan("RUNOFF_A1_VIC.txt")
plot(sf,simu_flow*0.2)
plot(simu_flow*0.2, type="o", col="blue", ylim=c(0,12))

# Graph trucks with red dashed line and square points
lines(sf, type="o", pch=22, lty=2, col="red")






