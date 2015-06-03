
#This script downloads rainfall data from NLDAS  

#Input: 

#(1)Bounday box
#(2)Start time 
#(3)End time in yy-month-day hour format 

#Output:

#(1) Hourly data
library(zoo)
library(plyr)
setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed/A2_CD/A2_CD_present/A2_NLDAS") # set working directory
bbox=c(42.64,-124.2,42.95,-123.8) # boundary box of the study area
start_time="2001-01-01 00:00" # start time
end_time="2005-12-31 23:00"   #end time


lat=seq(bbox[1],bbox[3],0.125) 
lon=seq(bbox[2],bbox[4],0.125)
Lat_Index = floor((lat - (25.062500))/0.125) # lat index in NLDAS grid
Lon_Index = floor((lon - (-124.9375))/0.125) #lon index in NLDAS grid

time_index_all= seq(from = as.POSIXct("1979-01-01 13:00"),to = as.POSIXct("2013-10-18 13:00"), by = "hour") #create time series
time_index_need= seq(from = as.POSIXct(start_time),to = as.POSIXct(end_time), by = "hour") # create time series based on start and end date
date_overlap=match(time_index_need,time_index_all,) # get overlap time interval
t1=date_overlap[1] # start time in  NLDAS time series
t2=date_overlap[length(date_overlap)] # end time index in NLDAS time series
len_timestep=t2-t1+1
num_lat=length(Lat_Index)
num_lon=length(Lon_Index)
a1=as.character(Lat_Index[1]) # start lat index as character
a2=as.character(Lat_Index[num_lat]) #end lat index as character
b1=as.character(Lon_Index[1]) ## start lon index as character
b2=as.character(Lon_Index[num_lon]) # end lon index as character
t1=
t2=as.character(t1+100*24)
n=round(len_timestep/8760)

dss=matrix(nrow=1,ncol=15)
for (i in 1:20) {
  tm1=as.character(t1+2*8760)
  tm2=as.character(t1+3*8760)
  
  x=c(tm1,tm2,b1,b2,a1,a2)
  den=paste("http://hydro1.sci.gsfc.nasa.gov/dods/NLDAS_FORA0125_H.002.ascii?apcpsfc","[",x[1],":",x[2],"]","[",x[3],":",x[4],"]","[",x[5],":",x[6],"]",sep="")
  
  data1=download.file(den,paste("output",2000,".txt",sep=""))
  
}

  f=paste(as.character(x), collapse=",")
  write(f, file = paste("input",i,".txt",sep=""))
  system(paste('python test.py -i',paste("input",i,".txt",sep=""),'-o',paste("output",i,".txt",sep="")))
  d=readLines(paste("output",i,".txt",sep=""))
  d=d[2:(8760*(num_lon+1))]
  df <-d[which(!d == "" )]
  s1 = unlist(strsplit(df, split=',', fixed=TRUE))
  sss=as.numeric(t(matrix(s1[-seq(1,,6,length(s1))],nrow=15,ncol=8760)))
  data=matrix(sss,nrow=8760,ncol=15)
  output <- rollapply(data,24,(sum),by=24,by.column=TRUE,align='right')

  dss=rbind(dss,output)

  
  return(dss)
  }

y=data.frame(xe=c(1,2,3,4,5))
tf=ddply(y,'xe',rain)

###########processigng NLDAS data for rainfall ####################

