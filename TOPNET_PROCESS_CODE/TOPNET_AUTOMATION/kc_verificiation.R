
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C21_Watershed_final\\C21_CD\\C1_CD_present")

#####this code estimate reference evpotranspiraition to verfy whether we choose correct crop coefficient###########

xx="file"
nc=31
for (i in 1:nc){
  xx[i]=paste("filename",i,".csv",sep="")
}
rainfall <- function(file ){
  mydata2 = read.csv(file,skip=6)
  my.data.frame <- mydata2[(mydata2$year%%4==0) & (mydata2$yday ==365), ]
  my.data.frame$yday=my.data.frame$yday+1
  total <- rbind( my.data.frame,mydata2)
  gh= total[with(total, order(year,yday)), ]
  rain=data.frame(rainmm=gh$prcp..mm.day., tmaxcel=gh$tmax..deg.c.,tmincel=gh$tmin..deg.c.,vppa= gh$vp..Pa.,sr=gh$srad..W.m.2.)
  
  return(rain)
}
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


tmax=dss[,seq(3,ncol(dss),5)]
tmin=dss[,seq(4,ncol(dss),5)]
vp=dss[,seq(5,ncol(dss),5)]
sr=dss[,seq(6,ncol(dss),5)]

wind=read.table('wind.dat',skip=3)
wind=wind[,1]


require(spsurvey)
lat_lon_rg=read.dbf('rg_C21.dbf')
lat_lon_rg=do.call(cbind,lat_lon_rg)



lat_lon_rg1=albersgeod(lat_lon_rg[,1], lat_lon_rg[,2], sph="GRS80", clon=-96, clat=23, sp1=29.5, sp2=45.5)

###extratraistial radiation calculation####
lat_unique=unique(lat_lon_rg1[,2])
phi=matrix(nrow =length(lat_unique), ncol= 1)
dr=matrix(ncol =length(lat_unique), nrow = 1)
delta1=matrix(ncol=366, nrow = 1)
ws=matrix(nrow =length(lat_unique), ncol = 366)
r=matrix(nrow =length(lat_unique), ncol = 366)

###step by step calculation
##based on that sites:http://edis.ifas.ufl.edu/ae459
##1
avgT=0.5*(tmax+tmin)
RS=sr*0.0864
u2=matrix(rep(wind,nc),nrow=length(dates),ncol=nc)
delta=4098*(0.6108*exp((17.27*avgT)/(avgT+237.3)))/(avgT+273.3)^2
aP1=101.3*((293-0.0065*Ele_station)/293)^5.26
ap=matrix(rep(aP1,length(dates)),nrow=nc,ncol=length(dates))
aP=t(ap)
gamma=0.000665*aP
DT=delta/(delta+gamma(1+0.34*u2))
PT=gamma/(delta+gamma(1+0.34*u2))
TT=900*u2/(avgT+273)
estmax=0.6108*exp(17.27*tmax/(tmax+237.3))
estmin=0.6108*exp(17.27*tmin/(tmin+237.3))
es=0.5*(estmax+estmin)
ea=vp/1000


for ( i in 1:length(lat_unique)){
  for ( j in 1:366){
    
 phi[i]=pi*lat_unique[i]/180

 dr=1+0.033*cos(2*pi*j/366)
 delta1[j]=0.409*sin(2*pi*j/366-1.39)
 ws[i,j]=acos(-tan(phi[i,1])*tan(delta1[1,j]))
 r[i,j]=0.0820*dr*(ws[i,j]*sin(phi[i,1])*sin(delta1[1,j])+cos(phi[i])*cos(delta1[j])*sin(ws[i,j]))*24*60/pi
 }

}

rr=(t(r))
rr1=apply(rr, 2, rep,33)
rm=rr1[1:12054,]
ele=matrix(Ele_station,nrow=1,ncol=31)
ele=apply(ele, 2, rep,length(dates))
RSo=(0.75+2*10^(-5)*ele)*rm
Rns=(1-0.23)*RS

Rnl=(4.903*10^(-9))*(0.5*((tmax+273.4)^4+(tmin+273.4)^4))*(0.34-0.14*sqrt(ea))*((1.35*RS/RSo)-0.35)

RN=Rns-Rnl
RNg=0.408*RN
ETrad=DT*RNg
ETwind=PT*TT*(es-ea)
ETo=ETrad+ETwind
dates_2004=seq(as.Date("2004/1/1"), as.Date("2004/12/31"), "day")
dates_match=match(dates_2004,dates)
ETo_2004=ETo[dates_match,]
ETo_monthly=data.frame(monthlyfunction(ETo_2004, FUN=sum, na.rm=TRUE,dates=dates_2004))
ETo_monthly_mean=colMeans(ETo_monthly)
plot(ETo_monthly_mean)
#pet_C1=c(200,350,840,1220,1520,1900,2100,1800,1275,680,300,200) ## from PET map in mm/month
pet_C21=c(171,346,662,1120,1500,1650,1600,1450,1100,600,360,200)
#pet_C1=0.1*pet_C1/(25.4) ## in inches/month
pet_C21=0.1*pet_C21 ## in inches/month
plot(pet_C21)
#kc_C1=pet_C1/ETo_monthly_mean
kc_C21=pet_C21/ETo_monthly_mean









 vv v                                                                                                                                                                         







#sr=dss[,seq(5,311,5)]*0.0864
kt=0.00185*(tmax[dates_match,]-tmin[dates_match,])^2-0.0433*(tmax[dates_match,]-tmin[dates_match,])+0.423
eto=0.0135*kt*rr*(tmax[dates_match,]-tmin[dates_match,])^0.5*(0.5*tmax[dates_match,]+0.5*tmin[dates_match,])+17.8

library(RNCEP)
library(zoo)
library(hydroTSM)

eto_monthly<- (as.matrix(daily2monthly(eto, FUN=sum, na.rm=TRUE,dates=dates_2004)))
fg=(as.matrix(daily2monthly(tmax[dates_match,]-tmin[dates_match,], FUN=mean, na.rm=TRUE,dates=dates_2012)))
fg_mon_mean=rowMeans(fg)
eto_monthly_mean=rowMeans(eto_monthly)
plot(eto_monthly_mean)
# for A1 watershed
# g1=c(66,66,103,164,224,243,268,266,221,147,87,36)
# g2=c(67,67,108,168,228,248,276,271,225,149,89,37)
# g3=c(69,69,114,170,230,254,284,277,230,152,91,38)
# g4=c(71,71,120,172,233,260,291,283,235,155,92,39)
# kc_g1=signif(g1/eto_monthly_mean,2)
# kc_g2=signif(g2/eto_monthly_mean,2)
# kc_g3=signif(g3/eto_monthly_mean,2)
# kc_g4=signif(g4/eto_monthly_mean,2)
#for A2 Wateshed

ET_ac_C1=c(18,26,54,76,95,110,115,90,74,45,23,17)



kc_C1=ET_ac_C1/ETo_monthly_mean


g2=c(60,69,140,160,185,136,170,190,175,115,67,60)
g3=c(33,38,75,103,126,106,130,130,115,84,48,31)

kc=g3/g2
g1=c(200,350,840,1220,1520,1900,2100,1800,1275,680,300,200)
g1=c(171,346,662,1120,1500,1650,1600,1450,1100,600,360,200)

kc=g1/g2
kc=signif(g1*0.1/eto_monthly_mean,2)

plot(kc_C1)
lines(kc_g3,col='blue')
lines(kc_g2,col='green')
lines(kc_g1,col='orange')


