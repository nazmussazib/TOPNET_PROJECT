###determine monthly rainfall  changes########
library(RNCEP)
library(zoo)
library(hydroTSM)
library(ggplot2)

##historical file reading
rainfall_his=function(file){
  rain=read.table(file,skip=3)
  dates_all=seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day")
  dates_bl=seq(as.Date("1986/1/1"), as.Date("2005/12/31"), "day")
  dates_match=match(dates_bl,dates_all)
  rainfall=rain[dates_match,(1:(ncol(rain)-2))]
  rain_mon=matrix(colMeans(data.frame(seasonalfunction(rainfall, FUN=sum, na.rm=TRUE,dates=dates_bl))/20))
  rain_ann=matrix(rowMeans(data.frame(annualfunction(rainfall, FUN=sum, na.rm=TRUE,dates=dates_bl))/20))
  rain_all=rbind(rain_ann,rain_mon)
}

rainfallhis=matrix(NA,nrow=5,ncol=1)
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
for ( i in 1:length(watsed)){
  setwd(paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""))
  rain_his=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*rain_his")
  mrhis=matrix(sapply(rain_his,rainfall_his),nrow=5,ncol=5)
  rainfallhis=cbind(rainfallhis,mrhis)
 
}

rain_monthly_his=rainfallhis[,-1]


rainfall_proj=function(file){
md=unlist(strsplit(file,""))[13]

if(length(grep("M",md)==1)){
  
rain=read.table(file,skip=3)
dates_all=seq(as.Date("2040/1/1"), as.Date("2099/12/31"), "day")
dates_46=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
dates_76=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")

dates_match46=match(dates_46,dates_all)
dates_match76=match(dates_76,dates_all)

rain46=rain[dates_match46,(1:(ncol(rain)-2))]
rain76=rain[dates_match76,(1:(ncol(rain)-2))]
rainmon46=matrix(colMeans(data.frame(seasonalfunction(rain46, FUN=sum, na.rm=TRUE,dates=dates_46))/20))
rainann46=matrix(rowMeans(data.frame(annualfunction(rain46, FUN=sum, na.rm=TRUE,dates=dates_46))/20))
rainmon76=matrix(colMeans(data.frame(seasonalfunction(rain76, FUN=sum, na.rm=TRUE,dates=dates_76))/20))
rainann76=matrix(rowMeans(data.frame(annualfunction(rain76, FUN=sum, na.rm=TRUE,dates=dates_76))/20))

rainall46=rbind(rainann46,rainmon46)
rainall76=rbind(rainann76,rainmon76)
rainall=cbind(rainall46,rainall76)

} else {

  rain=read.table(file,skip=3)
  rain46=rain[1:7305,(1:(ncol(rain)-2))]
  rain76=rain[7306:14610,(1:(ncol(rain)-2))]
  dates_46=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
  dates_76=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")
  rainmon46=matrix(colMeans(data.frame(seasonalfunction(rain46, FUN=sum, na.rm=TRUE,dates=dates_46))/20))
  rainann46=matrix(rowMeans(data.frame(annualfunction(rain46, FUN=sum, na.rm=TRUE,dates=dates_46))/20))
  rainmon76=matrix(colMeans(data.frame(seasonalfunction(rain76, FUN=sum, na.rm=TRUE,dates=dates_76))/20))
  rainann76=matrix(rowMeans(data.frame(annualfunction(rain76, FUN=sum, na.rm=TRUE,dates=dates_76))/20))
  
  rainall46=rbind(rainann46,rainmon46)
  rainall76=rbind(rainann76,rainmon76)
  rainall=cbind(rainall46,rainall76)

  
}
  
return(rainall)
} 

rain_monthly_2.6=matrix(NA,nrow=5,ncol=1)
rain_monthly_8.5=matrix(NA,nrow=5,ncol=1)

for ( i in 1:length(watsed)){

  setwd(paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""))
  rain_proj2.6=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*rain_rcp2.6")
  rain_proj8.5=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*rain_rcp8.5")
  
  mr26=matrix(sapply(rain_proj2.6,rainfall_proj),nrow=5,ncol=10)
  mr85=matrix(sapply(rain_proj8.5,rainfall_proj),nrow=5,ncol=10)
  
  rain_monthly_2.6=cbind(rain_monthly_2.6,mr26)
  rain_monthly_8.5=cbind(rain_monthly_8.5,mr85)
}


rain_monthly_2.6=rain_monthly_2.6[,-1]
rain_monthly_8.5=rain_monthly_8.5[,-1]

rainmonthlyhis=rain_monthly_his
rainmonthly2646=rain_monthly_2.6[,seq(1,80,2)];rainmonthly2676=rain_monthly_2.6[,seq(2,80,2)]
rainmonthly8546=rain_monthly_8.5[,seq(1,80,2)];rainmonthly8576=rain_monthly_8.5[,seq(2,80,2)]


###monthly temperature changes ############

##historical file reading
temperature_his=function(file){
  temp=read.table(file,skip=3)
  dates_all=seq(as.Date("1980/1/1"), as.Date("2005/12/31"), "day")
  dates_bl=seq(as.Date("1986/1/1"), as.Date("2005/12/31"), "day")
  dates_match=match(dates_bl,dates_all)
  temperature=temp[dates_match,(1:(ncol(temp)-2))]
  tmax=(temperature[1:length(dates_bl),seq(1,dim(temperature)[2],3)])
  tmin=(temperature[1:length(dates_bl),seq(2,dim(temperature)[2],3)])
  avgT=0.5*(tmax+tmin)
  temp_mon=matrix(colMeans(data.frame(seasonalfunction(avgT, FUN=mean, na.rm=TRUE,dates=dates_bl))))
  temp_ann=matrix(rowMeans(data.frame(annualfunction(avgT, FUN=mean, na.rm=TRUE,dates=dates_bl))))
  temp_all_his=rbind(temp_ann,temp_mon)
  return(temp_all_his)
}



temperaturehis=matrix(NA,nrow=5,ncol=1)
for ( i in 1:length(watsed)){
  
  setwd(paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""))
  temp_his=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*tdew_his")
  trhis=matrix(sapply(temp_his,temperature_his),nrow=5,ncol=5)
  temperaturehis=cbind(temperaturehis,trhis)
  
}
temperaturehis=temperaturehis[,-1]

temperature_proj=function(file){
  md=unlist(strsplit(file,""))[13]
  
  if(length(grep("M",md)==1)){
    
    temp=read.table(file,skip=3)
    dates_all=seq(as.Date("2040/1/1"), as.Date("2099/12/31"), "day")
    dates_46=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
    dates_76=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")
    temp=temp[,(1:(ncol(temp)-2))]
    tmax=temp[,seq(1,dim(temp)[2],3)]
    tmin=temp[,seq(2,dim(temp)[2],3)]
    tavg=0.5*(tmax+tmin)
    
    dates_match46=match(dates_46,dates_all)
    dates_match76=match(dates_76,dates_all)
    
    avgT46= tavg[dates_match46,]
    avgT76= tavg[dates_match76,]
    tempmon46=matrix(colMeans(data.frame(seasonalfunction(avgT46, FUN=mean, na.rm=TRUE,dates=dates_46))))
    tempann46=matrix(rowMeans(data.frame(annualfunction(avgT46, FUN=mean, na.rm=TRUE,dates=dates_46))))
    tempmon76=matrix(colMeans(data.frame(seasonalfunction(avgT76, FUN=mean, na.rm=TRUE,dates=dates_76))))
    tempann76=matrix(rowMeans(data.frame(annualfunction(avgT76, FUN=mean, na.rm=TRUE,dates=dates_76))))
    
    tempall46=rbind(tempann46,tempmon46)
    tempall76=rbind(tempann76,tempmon76)
    tempall=cbind(tempall46,tempall76)
    
  } else {
    
    temp=read.table(file,skip=3)
    dates_46=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
    dates_76=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")
    temp=temp[,(1:(ncol(temp)-2))]
    tmax=temp[,seq(1,dim(temp)[2],3)]
    tmin=temp[,seq(2,dim(temp)[2],3)]
    tavg=0.5*(tmax+tmin)
    
    avgT46= tavg[1:7305,]
    avgT76= tavg[7306:14610,]
    tempmon46=matrix(colMeans(data.frame(seasonalfunction(avgT46, FUN=mean, na.rm=TRUE,dates=dates_46))))
    tempann46=matrix(rowMeans(data.frame(annualfunction(avgT46, FUN=mean, na.rm=TRUE,dates=dates_46))))
    tempmon76=matrix(colMeans(data.frame(seasonalfunction(avgT76, FUN=mean, na.rm=TRUE,dates=dates_76))))
    tempann76=matrix(rowMeans(data.frame(annualfunction(avgT76, FUN=mean, na.rm=TRUE,dates=dates_76))))
    
    tempall46=rbind(tempann46,tempmon46)
    tempall76=rbind(tempann76,tempmon76)
    tempall=cbind(tempall46,tempall76)
    
    
  }
  
  return(tempall)
} 

temp_monthly_2.6=matrix(NA,nrow=5,ncol=1)
temp_monthly_8.5=matrix(NA,nrow=5,ncol=1)



for ( i in 1:length(watsed)){
  
  setwd(paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""))
  temp_proj2.6=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*tdew_rcp2.6")
  temp_proj8.5=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\precp_temp_changes\\",watsed[i],sep=""),pattern ="*tdew_rcp8.5")
  
  tr26=matrix(sapply(temp_proj2.6,temperature_proj),nrow=5,ncol=10)
  tr85=matrix(sapply(temp_proj8.5,temperature_proj),nrow=5,ncol=10)
  
  temp_monthly_2.6=cbind(temp_monthly_2.6,tr26)
  temp_monthly_8.5=cbind(temp_monthly_8.5,tr85)
}



temp_monthly_2.6=temp_monthly_2.6[,-1]
temp_monthly_8.5=temp_monthly_8.5[,-1]

tempmonthlyhis=temperaturehis
tempmonthly2646=temp_monthly_2.6[,seq(1,80,2)];tempmonthly2676=temp_monthly_2.6[,seq(2,80,2)]
tempmonthly8546=temp_monthly_8.5[,seq(1,80,2)];tempmonthly8576=temp_monthly_8.5[,seq(2,80,2)]


#####stream flow files 


streamflow_all=function(stream_file,bn) {
  
  
  time_his=seq(as.Date("1985/1/1"), as.Date("2005/12/31"), "day")
  time_his1=seq(as.Date("1986/10/1"), as.Date("2005/9/30"), "day")
  time_proj1=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
  time_proj11=seq(as.Date("2046/10/1"), as.Date("2065/9/30"), "day")
  time_proj2=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")
  time_proj22=seq(as.Date("2076/09/30"), as.Date("2095/9/30"), "day")
  date_op_his=match(time_his1,time_his) # get overlap time interval
  date_op_proj1=match(time_proj11,time_proj1) # get overlap time interval
  date_op_proj2=match(time_proj22,time_proj2) # get overlap time interval
  
  
  
  flow_all=matrix(NA,nrow=6940,ncol=27)
  
  for ( k in 1:length(stream_file)) {
    
    
    d1=strtoi(unlist(strsplit(stream_file[k],NULL)))
    d2=strtoi(d1[length(d1)-5])
    if(d2==4){date=time_proj11;date_lap=date_op_proj1}else if(d2==7){date=time_proj22;date_lap=date_op_proj2}
    else {date=time_his1;date_lap=date_op_his}
    
    stano=stream_file[k]
    
    if (length(grep("calibrated",stano))==1) {
      ff=scan(stano, what="")
      l=bn+2#for A2
      ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
      simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
      flow=data.frame(date=date,q=simu_flow[date_lap])
      flow[flow<0.001]=0.001
      flow_all[,k]=simu_flow[date_lap]
      
    }else if (length(grep("observed",stano))==1)   {
      
      sf=scan(stano, what="")
      sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
      obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
      
      time_all_observ= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
      
      date_overlap_obs=match(date,time_all_observ) # get overlap time interval
      flow=data.frame(date=date,q=obs_flow[date_overlap_obs])
      flow[flow<0.001]=0.001
      flow_all[,k]=obs_flow[date_overlap_obs]
      
      
    }else {
      
      ff=scan(stano, what="")
      
      l=bn+2#for A2
      ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
      simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
      flow=data.frame(date=date,q=simu_flow[date_lap])
      flow[flow<0.001]=0.001
      flow_all[,k]=simu_flow[date_lap]
      
    }
    
  }
  
  seasonal_streamflow1=matrix(seasonalfunction(flow_all, FUN=mean, na.rm=TRUE,dates=time_his1))
  seasonal_streamflow2=matrix(mean(flow_all,na.rm=TRUE)) ## to keep same with sulochon matrix, as it gives differesnt annlual if i am using annual function
  seasonal_streamflow=rbind(seasonal_streamflow2,seasonal_streamflow1)
  
  return(seasonal_streamflow)
}


watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
srall=matrix(NA,nrow=1,ncol=27)
for ( i in 1:length(watsed)){
  
  setwd(paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\streamflow_changes\\",watsed[i],sep=""))
  stream_all=list.files(path =paste("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\streamflow_changes\\",watsed[i],sep=""),pattern ="*.txt")
  
  banum=c(29,113,25,135,43,131,59,43)
  srhis=mapply(streamflow_all,stream_all,banum[i])
  sr=matrix(na.omit(srhis),nrow=5,ncol=27)
  srall=rbind(srall,sr)
}

srall=srall[-1,]
streammonthlyhis=srall[,seq(5,dim(srall)[2],5)]
streammonthly2646=srall[,seq(1,dim(srall)[2]-2,5)];streammonthly2676=srall[,seq(2,dim(srall)[2]-2,5)]
streammonthly8546=srall[,seq(3,dim(srall)[2]-2,5)];streammonthly8576=srall[,seq(4,dim(srall)[2]-2,5)]


####################plotting##########

tempchange2646=tempmonthly2646-tempmonthlyhis;
tempchange8546=tempmonthly8546-tempmonthlyhis
tempchange2676=tempmonthly2676-tempmonthlyhis
tempchange8576=tempmonthly8576-tempmonthlyhis


rainchange2646=((rainmonthly2646/rainmonthlyhis)-1)*100;
rainchange8546=((rainmonthly8546/rainmonthlyhis)-1)*100
rainchange2676=((rainmonthly2676/rainmonthlyhis)-1)*100
rainchange8576=((rainmonthly8576/rainmonthlyhis)-1)*100




annualstream=function(change){
  rh=matrix(NA,nrow=5,ncol=1) 
  for ( j in 1:8){
    
    dh=(change[((5*j-4):(5*j)),])
    rh=cbind(rh,dh)
  }
  rh=rh[,-1]
  return(rh)
}

stream_all_his=(annualstream(streammonthlyhis));streamchange2646=apply(annualstream(streamchange2646),2,mean);
streamchange2646=((streammonthly2646/streammonthlyhis)-1)*100;
streamchange8546=((streammonthly8546/streammonthlyhis)-1)*100;
streamchange2676=((streammonthly2676/streammonthlyhis)-1)*100;
streamchange8576=((streammonthly8576/streammonthlyhis)-1)*100;

############plotting just change the matrix########

#for temperature and precipitation
v1=rainchange2676; 
v2=rainchange8576

#for streamflow
v1=t(streamchange2676); 
v2=t(streamchange8576)

r1=matrix(v1,nrow=200,ncol=1)
r2=matrix(v2,nrow=200,ncol=1)
r=rbind(r1,r2)


watershed1=matrix((rep(watsed,each=25)))
watershed=rbind(watershed1,watershed1)

##for temperature and precipitation
season1=matrix(rep(c('Ann','s1', 's2', 's3', 's4'),40))
season=rbind(season1,season1)
##for streamflow
season1=matrix(rep(matrix(rep(c('Ann','s1', 's2', 's3', 's4'),each=5)),8))
season=rbind(season1,season1)

scena1=matrix(rep(c('RCP2.6'),each=200))
scena2=matrix(rep(c("RCP8.5"),each=200))
scena=rbind(scena1,scena2)


x <- data.frame(
  values = r,
  scenario= scena,
  time = season,
  place = watershed
)

# compare different sample populations across various temperatures
ggplot(x, aes(x = time, y = values, fill = scenario)) +
  geom_boxplot() +geom_hline(yintercept = 0.0,colour="black",size=1)+ #stat_summary(aes(group=scenario), fun.y=mean, geom="line")+
  facet_wrap(~ place,scales = "free")+theme_bw()+
theme(legend.position="none")+
scale_fill_manual(name = "Scenario", values = c("white", "grey"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  xlab("Season") + ylab("Rainfall Change (%)")+ggtitle(" Year 2076-2095 compare to 1986-2005 ")

  xlab("Season") + ylab(expression(paste(bold("Temperature Change(")^bold("0"),bold("C)"),sep="")))+ggtitle(" Year 2076-2095 compare to 1986-2005 ")

scale_x_discrete(breaks=c("Ann", "s1", "s2", "s3", "s4"), labels=c("Ann", 'Winter', 'Spring', 'Summer', 'Fall'))+


  
####writing mean change results#################


#########for AGU poster only annual changes #########

#for temperature and precipitation
v1=tempchange2676[1,]; 
v2=tempchange8576[1,]



r1=matrix(v1,nrow=40,ncol=1)
r2=matrix(v2,nrow=40,ncol=1)
r=rbind(r1,r2)


watershed1=matrix((rep(watsed,each=5)))
watershed=rbind(watershed1,watershed1)

##for temperature and precipitation
season1=matrix(rep(c('Annual'),40))
season=rbind(season1,season1)


scena1=matrix(rep(c('RCP2.6'),each=40))
scena2=matrix(rep(c("RCP8.5"),each=40))
scena=rbind(scena1,scena2)


x <- data.frame(
  values = r,
  scenario= scena,
  time = season,
  place = watershed
)

setwd("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\AGU_TOPNET")
# compare different sample populations across various temperatures
print(ggplot(x, aes(x = time, y = values, fill = scenario)) +
  geom_boxplot() +geom_hline(yintercept = 0.0,colour="black",size=1)+ #stat_summary(aes(group=scenario), fun.y=mean, geom="line")+
  facet_wrap(~ place,scales = "free",ncol=8)+theme_bw()+
  #theme(legend.position="none")+
  scale_fill_manual(name = "Scenario", values = c("green1","blue3"))+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="bold"),
        axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  #xlab("Emission Scenario") + ylab("Rainfall Change (%)"))

xlab("Emission Scenario") + ylab(expression(paste(bold("Temperature Change(")^bold("0"),bold("C)"),sep=""))))
#+ggtitle(" Year 2076-2095 compare to 1986-2005 ")

#scale_x_discrete(breaks=c("Ann", "s1", "s2", "s3", "s4"), labels=c("Ann", 'Winter', 'Spring', 'Summer', 'Fall'))+
  
ggsave("temp_change_76_95.tiff", width=14, height=2.5, dpi=500)
dev.off()









##rainfall and temperatyure change functionchange

change_mean=function(a1){
s=aggregate(a1$values, by=list(a1$time), FUN=mean)[2]
return(s)
}



##for rainfall and temperature 
a=seq(1,400,25)
b=seq(25,400,25)

##for streamflow
a=seq(1,400,5)
b=seq(5,400,5)

rain_mean=matrix(NA,5,1)
for (i in 1:80){
 
ra_cg=change_mean(x[a[i]:b[i],])
rain_mean=cbind(rain_mean,ra_cg)
}
  
rain_mean=rain_mean[,-1]

##only for streamflow
rain_mean=data.matrix(rain_mean[1,])
rain_mean=matrix(rain_mean,nrow=5,ncol=16)

write.table(rain_mean,'stream_change_2076_95.txt')




##############plotting only historical to see pattern ##################


h1=tempmonthlyhis
h11=matrix(h1,nrow=200,ncol=1)
watershedh1=matrix((rep(watsed,each=25)))
seasonh1=matrix(rep(c('Ann','s1', 's2', 's3', 's4'),40))
scenah1=matrix(rep(c('his'),each=200))




xh <- data.frame(
  values =h11,
  scenario= scenah1,
  time = seasonh1,
  place = watershedh1
)

# compare different sample populations across various temperatures
ggplot(xh, aes(x = time, y = values, fill = scenario)) +
  geom_boxplot() +geom_hline(yintercept = 0.0,colour="black",size=1)+stat_summary(aes(group=scenario), fun.y=mean, geom="line")+
  facet_wrap(~ place,scales = "free")+theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(name = "Scenario", values = c("white", "grey50"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  #xlab("Season") + ylab("Rainfall Change (%)")
  
  xlab("Season") + ylab(expression(paste(bold("Temperature Change(")^bold("0"),bold("C)"),sep="")))

scale_x_discrete(breaks=c("Ann", "s1", "s2", "s3", "s4"), labels=c("Ann", 'Winter', 'Spring', 'Summer', 'Fall'))+








month=c('DJF',"MAM",'JJA',"SON")
month1=c("","","","")
#op <- par(mfrow = c(2,1),mar=c(1,5,1,1))
op <- par(mfrow = c(2,1),mar=c(0.1,5,0,0.5), oma=c(1.5,1,1,1))
#op <- par(mfrow = c(1,2),mar=c(0.1,5,0,0.5), oma=c(1.5,1,1,1))
plot(tempchange8576,lty=1,lwd=2,type="o",ylim=c(0,8),axes=FALSE,ann=FALSE)
#axis(side=1,at=seq(1,12,1), month,font=2)
axis(side=1,at=seq(1,4,1), month1,font=2,cex=1.2)
axis(2, las=1, at=2*0:8,font=2,cex.lab=2)
box()
lines(seq(1,4,1),tempchange2676,col='grey60',lty=1,lwd=2,type="o")
lines(seq(1,4,1),tempchange8546,col='black',lty=3,lwd=3,type="o")
lines(seq(1,4,1),tempchange2646,col='grey60',lty=3,lwd=3,type="o")

legend(1,8.5,bty="n",c("Rcp8.5: 76-95","Rcp2.6: 76-95"),col=c("black","grey"),text.font=2,lty=c(1,1),lwd=c(2,2))
legend(3,8.5,bty="n",c("Rcp8.5: 46-65","Rcp2.6: 46-65"),col=c("black","grey"),text.font=2,lty=c(3,3),lwd=c(3,3))

title(xlab="Month",font.lab=2,cex.lab=1)
title(ylab=expression(paste(bold("Temperature Change(")^bold("0"),bold("C)"),sep="")),cex.lab=0.75)

plot(rainchange8576,lty=1,lwd=2,type="o",ylim=c(-40,50),axes=FALSE,ann=FALSE)
#axis(side=1,at=seq(1,12,1), month,font=2)
axis(side=1,at=seq(1,4,1), month1,font=2)
axis(2, las=1, at=10*-50:50,font=2)
box()
lines(seq(1,4,1),rainchange2676,col='grey60',lty=1,lwd=2,type="o")
lines(seq(1,4,1),rainchange8546,col='black',lty=3,lwd=3,type="o")
lines(seq(1,4,1),rainchange2646,col='grey60',lty=3,lwd=3,type="o")
lines(seq(1,4,1),rep(0,4),col='black',lty=1,lwd=3,type="o")
#title(xlab="Month",font.lab=2,cex.lab=1)
title(ylab="Rainfall change(%)",font.lab=2,cex.lab=0.75)





















####monthly streamflwo changes##########







