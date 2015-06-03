#install.packages('raster', dep=TRUE)
#install.packages('plyr', dep=TRUE)
#install.packages('Hmisc', dep=TRUE)
#install.packages('soilDB', dep=TRUE) # stable version from CRAN + dependencies
#install.packages("soilDB", repos="http://R-Forge.R-project.org") # most recent copy from r-forge
#install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source") # SSOAP and XMLSchema



#Input: 

#(1)SSURGO grid for interested area
#(2)Landcover raster data  
#(3)nodelinks.txt
#(4)Pit removed DEM
#(5)parameter specification file
#(6)lookup table for lulc and kc,cc,cr,albedo

#Output:

#(1)basinpars.txt


library(raster)
library(plyr)
library(Hmisc)
library(soilDB)
library(SSOAP)
library(foreign)
library(shapefiles)
library(rgdal)
setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\A1_Watershed_final\\A1_SD")
setwd("C:\\B22_watershed_final\\B22_SD")
##Need to check projection system 
# r1 <- raster('soil_all_B1.tif')
# r1_proj=projection(r1)
# newproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# r1_projected=projectRaster(r1, crs=newproj)
#  #if the projection is same dont need to writ
# r1_tif=writeRaster(r1_projected,"soilB1.tif") # write raster to 
# 

r <- raster('soilC212.tif') # get grid info from gssurgo data (download soil spatial and tabular data as tif format is big size)
r <- ratify(r,count=TRUE)
rat <- levels(r)[[1]]
tr <- read.dbf('soilC212.tif.vat.dbf') # extract mukey value 
mu=data.frame(tr)
names(mu)[1] <- 'ID'
names(mu)[2] <- 'Count'
names(mu)[3] <- 'MUKEY'
mu=mu[,-4]
in.statement <- format_SQL_in_statement(mu$MUKEY)


##make query for each variable

q1 <- paste("SELECT component.mukey, component.cokey, compname, comppct_r, hzdept_r, hzdepb_r, hzname,awc_r, ksat_r,wthirdbar_r,wfifteenbar_r,dbthirdbar_r,
sandtotal_r,claytotal_r,om_r
FROM component JOIN chorizon ON component.cokey = chorizon.cokey
AND mukey IN ", in.statement, "ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")

# now get component and horizon-level data for these map unit keys
res <- SDA_query(q1)



##query for soil depth
q2 <- paste("SELECT mukey,brockdepmin
 FROM muaggatt
 WHERE  mukey IN ", in.statement, "
 ", sep="")

res2 <- SDA_query(q2)

#####Model specific sensitivity parameter f in 1/m
##Need to more work 

# res_sort=res[order(res$cokey),]
# ktheta=data.frame(ktheta=res_sort$ksat) ##  ksat in different depth
# depth= data.frame(depth=res_sort$hzdepb_r) ## calculating depth 
# museq=unique(res_sort$mukey)
# 
# fvalue=rep(NA,length(museq))
# fvalue1=rep(NA,length(museq))
# for(i in 1:length(museq))
#   {
# muy=as.numeric(museq[1])
# muall=as.matrix(as.numeric(res_sort$mukey))
# ing=which(muall==muy)
# #yy=log(3600*10^(-6)*ktheta$ktheta[ing]) ## converting k into m/h and take log of that for linear regression
# yy=(3600*10^(-6)*ktheta$ktheta[ing])
# NonNAindex <- which(is.finite(yy)) ## excluding NAN and inf values
# yy=yy[NonNAindex]
# notfit=sum(abs(yy))
# xx=depth$depth[ing]
# #d=xx[1]
# xx=xx[NonNAindex]
# if (notfit>0){
# dat=data.frame(depth=xx[1:3],ksat=yy[1:3])
# f <- function(.pars,.dat,.fun) sum((.dat[,2]-.fun(.pars,.dat[,1]))^2)
# expFun <- function(b,x) b[1]*exp(b[2]*x)
# out <- optim(c(max(yy),0.01),f,.dat=dat,.fun=expFun)
# fvalue[i]=out$par[2]
# if( fvalue[i]>0 ) { fvalue[i]=NA}
#  else {fvalue[i]=abs(fvalue[i])}
# }
# else {fvalue[i]=NA }
# }
# #fvalue[fvalue<0.0001] =median(fvalue, na.rm=TRUE) #
# fvalue[is.na(fvalue)] =median(fvalue, na.rm=TRUE) #



#####check whether i am ri




# # function for copmuting weighted-mean  within a component key unit
co.mean.whc <- function(i) {
  wt <- i$comppct_r[1] # keep the first component pct (they are all the same)
  thick1 <- with(i, hzdepb_r - hzdept_r)# compute horizon thickness
  thick=thick1/100 ##in m
 #ksat <- thick/i$ksat_r #compute saturated hydraulic conductivity
  #ksat<-i$ksat_r[1]*3600*10^(-6)
  wcthirdbar <- thick * i$wthirdbar_r # compute water content at 1/3 bar with horizon
  wctfifteendbar  <- thick * i$wfifteenbar_r # compute water content at 15 bar with horizon
  dbthirdbar  <- thick * i$dbthirdbar_r # compute density at 1/3 bar with horizon
  sand <- thick * i$sandtotal_r# compute percentage of sand by weight with horizon
  clay<- thick * i$claytotal_r # compute percentage of clay  by weight with horizon
  om <- thick * i$om_r # compute percentage of organic matter by weight  with horizon
  awc <-  i$awc_r # available water capacity 
   
  thick.total=sum(thick, na.rm=TRUE)
  awc.total=sum(awc, na.rm=TRUE)
  awc.depth=( sum((thick *((!is.na(awc))*1)),na.rm=TRUE))
  #ksat.total <- sum(thick, na.rm=TRUE)/(sum(ksat, na.rm=TRUE)) # Harmonic mean
  #ksat.total<-ksat
 
  wcthirdbar.total <- (sum( wcthirdbar , na.rm=TRUE))/ (sum((thick *((!is.na( wcthirdbar))*1)),na.rm=TRUE))# depth weighted average of water content at 1/3 bar for each component  key
  wctfifteendbar.total <- (sum(wctfifteendbar, na.rm=TRUE))/(sum((thick *((!is.na( wctfifteendbar))*1)),na.rm=TRUE))# depth weighted average of water content at 15 bar for each component  key
  dbthirdbar.total <- (sum(dbthirdbar, na.rm=TRUE))/ (sum((thick *((!is.na( dbthirdbar))*1)),na.rm=TRUE))# depth weighted average of bulk density  at 1/3 bar for each component  key
  sand.total <- (sum(sand, na.rm=TRUE))/ (sum((thick *((!is.na( sand))*1)),na.rm=TRUE))# depth weighted average of sand   for each component  key
  clay.total <- (sum(clay, na.rm=TRUE))/ (sum((thick *((!is.na(clay))*1)),na.rm=TRUE)) # depth weighted average of clay  for each component  key
  om.total <- (sum(om, na.rm=TRUE))/ (sum((thick *((!is.na( om))*1)),na.rm=TRUE)) # depth weighted average of organic matter  for each component  key
  
  yy=log(3600*10^(-6)*i$ksat_r)
  xx=i$hzdept_r/100
  len=length(xx)
  if(len>1){
 
   hh=lm(yy~xx)
  fval=hh$coefficients[2]
  ko=exp(hh$coefficients[1])
  }else{
  fval=NA
  ko=NA}


  fval[fval>=0]=-1/thick.total ##positive value replace by inverse of depth
 #fval[fval>=0]=-0.001 ##from TOPNET paper
  fval=abs(fval)
 fval[fval<0.001]=1/thick.total ###constant k gives very low value of f , so replace those by inverse of depth
# fval[fval<0.001]=0.001
#fval[fval>5]=5
 ksat.total=ko
 ksat.total[is.na(ksat.total)]=i$ksat_r[1]*3600*10^(-6)
 trans=ksat.total/fval
 #trans=ksat.total*thick.total ##testing only for A1 has problem with base flow??
 #yy1=(3600*10^(-6)*i$ksat_r)
 #xx1=thick
 #tri=sum(yy1*xx1,na.rm=TRUE)/sum(xx1,na.rm=TRUE)

 #trans=tri*thick.total
 
  data.frame(ksat=ksat.total,wcthird=wcthirdbar.total,wctfifteendbar=wctfifteendbar.total,  dbthirdbar=dbthirdbar.total,
             sand=sand.total,clay=clay.total,om=om.total,awc=awc.total,awcdepth=awc.depth, thick=1/fval,wt=wt,f=fval,tr=trans) # return profile water storage and component pct
  
  #,f=fval
}

# function for copmuting weighted-mean whc within a map unit
mu.mean.whc <- function(i) {
  thick <- wtd.mean(i$ thick, weights=i$wt) # safely compute wt. mean ksat for each map unit key
  ksat <- wtd.mean(i$ ksat, weights=i$wt) # safely compute wt. mean ksat for each map unit key
  
  wcthird<- wtd.mean(i$wcthird, weights=i$wt) # safely compute wt. mean water content at 1/3 bar for each map unit key
  wctfifteendbar <- wtd.mean(i$wctfifteendbar, weights=i$wt) # safely compute wt. mean water content at 15 bar for each map unit key
  dbthirdbar <- wtd.mean(i$dbthirdba, weights=i$wt) # safely compute wt. mean bulk density at 1/3 bar for each map unit key
  sand <- wtd.mean(i$sand, weights=i$wt) # safely compute wt. mean sand for each map unit key
  clay<- wtd.mean(i$ clay, weights=i$wt) # safely compute wt. mean clay for each map unit key
  om<- wtd.mean(i$om, weights=i$wt) # safely compute wt. mean organic matter for each map unit key
  fvalue= wtd.mean(i$f, weights=i$wt,na.rm=TRUE)
  
  ts= wtd.mean(i$tr, weights=i$wt,na.rm=TRUE)
  data.frame(depth=thick,ksat=ksat,wcthird=wcthird,wctfifteendbar=wctfifteendbar, dbthirdbar= dbthirdbar,sand=sand,clay=clay,om=om,fval=fvalue,tr=ts) # return wt. mean water storage
  #,fval=fvalue*-1
}

# aggregate by component  unit
co.whc <- ddply(res, c('mukey', 'cokey'), co.mean.whc)
# aggregate by map unit
mu.whc <- ddply(co.whc, 'mukey', mu.mean.whc)


# ##fvalue calculation is different for A2 watershed as it mukey and cokey is same##########
# f=matrix(NA,nrow=16,ncol=1)
# museq=unique(res$mukey)
# for(i in 1:length(museq))
#     {
#    muy=as.numeric(museq[i])
#    muall=as.matrix(as.numeric(res$mukey))
#    ing=which(muall==muy)
#    yy=log(3600*10^(-6)*res$ksat_r[ing])
#    xx=res$hzdepb_r[ing]/100
#    if(length(xx)>2){
#    hh=lm(yy~xx)
#    fval=hh$coefficients[2]
#    }else {
#      fval=0}
#    fval[fval>=0]=-1/max(xx)
#    fval[is.na(fval)]=-1/max(xx)
#    f[i]=fval*-1
# }

 
# saturated hydraulic conductivity
ko=mu.whc$ksat 

# drainable moisture content
porosity=1-mu.whc$dbthirdbar/2.65
dth1=porosity-mu.whc$wcthird/100   

# plant available moisture content
dth2=(mu.whc$wcthird-mu.whc$wctfifteendbar)/100 


#soil depth 
#soildepth=res2$brockdepmin

# pore disconnectedness index
b=(log(1500)-log(33))/(log(mu.whc$wcthird)-log(mu.whc$wctfifteendbar)) ## from Rawsl 1992 et al
c1=1; ## TOPNET use c=1
c=2*b+3 # from Dingman
b=(c-3)/2

##Green-Ampt wetting front suction parameter in m
##equations are taking from Rawls et at 1992
suctiont1=-21.67*mu.whc$sand/100-27.93*mu.whc$clay/100-81.97*dth1+71.12*(mu.whc$sand*dth1/100) +8.29*(mu.whc$clay*dth1/100)+14.05*(mu.whc$sand*mu.whc$clay/10000)+27.16
suction=suctiont1+(0.02*suctiont1*suctiont1-0.113*suctiont1-0.70) #unit kpa 
suction_meter=suction*0.102 # convert kpa=m of water
psif=((2*b+3)/(2*b+6))*abs(suction_meter) #equation from Dingman


soil_data=data.frame(mukey=mu.whc$mukey, depth=mu.whc$depth,fvalue=mu.whc$fval,ksat=ko,dth1=dth1,dth2=dth2,c=c1,psif=psif,tr=mu.whc$tr)
#soil_data=data.frame(mukey=mu.whc$mukey, depth=mu.whc$depth,fvalue=f,ksat=ko,dth1=dth1,dth2=dth2,c=c,psif=psif)


## need to think about soil depth

names(soil_data)[1]='ID'
names(mu)[1]='ID1'
names(mu)[3]='ID'
soildata_join=join( mu,soil_data, by='ID')
names( soildata_join)[3]='Mukey'
names( soildata_join)[1]='ID'
rat.new=join(rat,soildata_join,type='left')

rat.new <- rat.new[,c("ID", "COUNT", "Mukey","fvalue","ksat","dth1","dth2","psif","depth","tr")]
#rat.new$depth <- rat.new$depth/100 ## change unit cm to m
#rat.new$ksat <- rat.new$ksat*(3600*10^(-6)) ## change unit micro meter/sec to m/h
#rat.new["Trans"] <- NA # That creates the new column named "MY_NEW_COLUMN" filled with "NA"
#rat.new$Trans <- rat.new$ksat*rat.new$depth

##Need to discuss and think



levels(r)=rat.new

q=c("f","ko","dth1","dth2","psif","sd","Trans")

for(i in 1:length(q)){
  r.new=deratify(r,att=names( rat.new)[i+3]) 
  r.new[is.na(r.new[])] <- cellStats(r.new,mean) ## fill missing data with mean value
  writeRaster(r.new,(paste(q[i],".tif",sep="")),overwrite=TRUE,datatype='FLT4S',format="GTiff",options="COMPRESS=NONE")
}





###Getting Land cover data for the study area

uslulc=raster('nlcd2006_landcover_4-20-11_se5.img')
#watershed=raster("lrdemprjmme1.tif") # my model element

lulcwatershed=crop(uslulc,r,"lulcwatershed1.tif",overwrite=TRUE)
sum_lulc_watrshed1= overlay(lulcwatershed, r, fun=function(x,y){return(x+y)}) # sumof watershed and croped lulc
sum_lulc_watershed2=overlay(sum_lulc_watrshed1,r, fun=function(x,y){return(x-y)}) # subtract (sum of watershed and croped rainfall) to watershed for getting annural rainfall for watrshed
my=setExtent(sum_lulc_watershed2, r, keepres=FALSE, snap=FALSE)
lulc_intersted_area=writeRaster(my,"lulcmmef.tif",overwrite=TRUE,datatype='INT4S',options="COMPRESS=NONE") ## change datatype which will change 64 bit to 32 bit

##need to change extent for fel file

fel=raster('demB22fel.tif')
extent_fel=setExtent(fel, r, keepres=FALSE, snap=FALSE) 
elev_me=writeRaster(extent_fel,"demB22fel1.tif",overwrite=TRUE,datatype='FLT4S',options="COMPRESS=NONE") ## change datatype which will change 64 bit to 32 bit


input="demA1" 
##need to change extent for fel file
system(paste("mepsetup -me", (paste(input,"me.tif",sep="")),"-parspec", (paste(input,"parspc.txt",sep="")) ,"-node", "nodelinks.txt","-mpar", "basinpars_test.txt"))






