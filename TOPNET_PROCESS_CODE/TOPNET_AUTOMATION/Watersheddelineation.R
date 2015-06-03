#install.packages("rgdal")
#install.packages("raster")
#install.packages("shapefiles")
library(raster)
library(shapefiles)


#INPUT: 

#(1) DEM for interested area
#(2) Outlet gauge shape file


#OUTPUT:

#(1)Delineated watershed
#(2)nodelinks.txt
#(3)rchlink.txt
#(4)distribution.txt
#(5)rchareas.txt
#(6)rchproperties.txt




# Set working directory to your location
#dsn=setwd("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/LittleBearRiverDEMO/DEMO_test_1/WD")#change directory according to Input DEM file location
dsn=setwd("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\C22\\C22_WD")  #change directory according to Input DEM file location

input1="dem_C22" # change input file name
input2="outlet_C22"  # Outlet shapefile without .shp extension

z1=raster(paste(input1,".tif",sep=""))
plot(z1)
newproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
newproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#newproj= "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
z=projectRaster(z1, crs=newproj,res=30.00)
plot(z)
writeRaster(z,"demC22.tif",options="COMPRESS=NONE",datatype='FLT4S',overwrite=TRUE) # Need this if it is not projected

input="demC22" 
outlet=readOGR(dsn="outlet_C22.shp",layer=input2)
proj_outlet=spTransform(outlet,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#proj_outlet=spTransform(outlet,CRS("+proj=utm +zone=18+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
writeOGR(proj_outlet, dsn="outletC22.shp", layer="outletC22",driver="ESRI Shapefile") ## shape file is Outlets

# Pitremove
system(paste("mpiexec -n 8 pitremove -z ",(paste(input,".tif",sep="")), "-fel",(paste(input,"fel.tif",sep=""))))
fel=raster(paste(input,"fel.tif",sep=""))
plot(fel)

# D8 flow directions
cmf=paste("mpiexec -n 8 D8Flowdir -p",(paste(input,"p.tif",sep="")),"-sd8", (paste(input,"sd8.tif",sep="")),"-fel", (paste(input,"fel.tif",sep="")))
 
system(cmf,show.output.on.console=F,invisible=F)
p=raster(paste(input,"p.tif",sep=""))
plot(p)
sd8=raster(paste(input,"sd8.tif",sep=""))
#plot(sd8)


# Contributing area
system(paste("mpiexec -n 8 AreaD8 -p",(paste(input,"p.tif",sep="")), "-ad8",(paste(input,"ad8.tif",sep=""))))
ad8=raster(paste(input,"ad8.tif",sep=""))
plot(log(ad8))
#zoom(log(ad8))

# DInf flow directions
inffow=paste("mpiexec -n 8 DinfFlowdir -ang",(paste(input,"ang.tif",sep="")),"-slp", (paste(input,"slp.tif",sep="")),"-fel", (paste(input,"fel.tif",sep="")))

system(inffow,show.output.on.console=F,invisible=F)

ang=raster(paste(input,"ang.tif",sep=""))
plot(ang)
slp=raster(paste(input,"slp.tif",sep=""))
plot(slp)


# Dinf contributing area
system(paste("mpiexec -n 8 AreaDinf -ang",(paste(input,"ang.tif",sep="")),"-sca", (paste(input,"sca.tif",sep=""))))
sca=raster(paste(input,"sca.tif",sep=""))
plot(log(sca))
#zoom(log(sca))

# Threshold
system(paste("mpiexec -n 8 Threshold -ssa", (paste(input,"ad8.tif",sep="")), "-src", (paste(input,"src.tif",sep="")), "-thresh 100"))
src=raster(paste(input,"src.tif",sep=""))
plot(src)
#zoom(src)

##Need to work on that
# Move Outlets
#change gauge shape file name
system(paste("mpiexec -n 8 moveoutletstostreams -p", (paste(input,"p.tif",sep="")),"-src", (paste(input,"src.tif",sep="")), "-o outletC22.shp -om Outlets_moved.shp"))
# 
# 
# outpt=read.shp("Outlets_moved.shp")
# write.shapefile(outpt, "temp", T)
# outlet2=readOGR(dsn="Outlets_moved.shp",layer="Outlets_moved")
# proj4string(outlet2) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
# proj_outlet2=spTransform(outlet2,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# writeOGR(proj_outlet2, dsn=dsn,"Outlets2", driver="ESRI Shapefile") ## shape file is Outlets
# write.shp(proj_outlet2, "temp", T)
# points(outpt$shp[2],outpt$shp[3],pch=19,col=2)


# Peuker Douglas stream definition
system(paste("mpiexec -n 8 PeukerDouglas -fel", (paste(input,"fel.tif",sep="")), "-ss", (paste(input,"ss.tif",sep=""))))
ss=raster(paste(input,"ss.tif",sep=""))
plot(ss)
#zoom(ss)

#  Accumulating candidate stream source cells
system(paste("mpiexec -n 8 Aread8 -p",(paste(input,"p.tif",sep="")), "-o Outlets_moved.shp -ad8",(paste(input,"ssa.tif",sep="")),"-wg", (paste(input,"ss.tif",sep=""))))
ssa=raster(paste(input,"ssa.tif",sep=""))
plot(ssa)

#  Drop Analysis
system(paste("mpiexec -n 8 Dropanalysis -p", (paste(input,"p.tif",sep="")), "-fel", (paste(input,"fel.tif",sep="")),"-ad8", (paste(input,"ad8.tif",sep="")),"-ssa", (paste(input,"ssa.tif",sep="")), "-drp", (paste(input,"drp.txt",sep="")),"-o Outlets_moved.shp -par 50 5000 25 0"))

## TODO automate the retrieval of threshold from drp file



drop_file = readLines ((paste(input,"drp.txt",sep="")))
len_text=length(drop_file)
end_line=drop_file[len_text]
threshold =(as.numeric((unlist(strsplit(end_line, split=':', fixed=TRUE))[2]))) ## to get rid of smaller sub-watershed 


##watershed delineation
system(paste("mpiexec -n 8 Threshold -ssa ",(paste(input,"ssa.tif",sep="")),"-src", (paste(input,"src2.tif",sep="")),"-thresh", threshold))
#plot(raster(paste(input,"src2.tif",sep="")))

system(paste("mpiexec -n 8 Streamnet -fel", (paste(input,"fel.tif",sep="")), "-p", (paste(input,"p.tif",sep="")),"-ad8", (paste(input,"ad8.tif",sep="")),"-src", (paste(input,"src2.tif",sep="")), "-ord",(paste(input,"ord2.tif",sep="")),"-tree", (paste(input,"tree.txt",sep="")),"-coord",(paste(input,"coord.txt",sep="")),"-net", (paste(input,"net2.shp",sep="")), "-w",(paste(input,"w.tif",sep="")), "-o Outlets_moved.shp"),show.output.on.console=F,invisible=F)
watershed=raster(paste(input,"w.tif",sep=""))
modelel_tif=writeRaster(watershed,paste(input,"me.tif",sep=""),overwrite=TRUE,options="COMPRESS=NONE",datatype='INT4S') # write raster to 
plot(raster(paste(input,"me.tif",sep="")))

## as little bear river has missing data choose below as model element

me=raster('demA1me.tif')
# #plot(me)
# # me here is the soil map unit raster that may not cover the full domain.  The following two lines add, then subtract this from the watershed grid
# # so that the result is now the original watershed grid value or no data in places where the soil data does not exist
# watershed1= overlay(me, watershed,fun=function(x,y){return(x+y)}) # sumof watershed and croped rainfall 
# watershed2=overlay(watershed1,me, fun=function(x,y){return (x-y)}) # subtract (sum of watershed and croped rainfall) to watershed for getting annural rainfall for watrshed
# watershed_final=writeRaster(watershed2,paste(input,"mme1.tif",sep=""),overwrite=TRUE,datatype='INT4S',options="COMPRESS=NONE")
# #plot(watershed2)

# Wetness Index
system(paste("mpiexec -n 8 SlopeAreaRatio -slp", (paste(input,"slp.tif",sep="")),"-sca", (paste(input,"sca.tif",sep="")),"-sar", (paste(input,"sar.tif",sep=""))), show.output.on.console=F, invisible=F)
sar=(raster(paste(input,"sar.tif",sep="")))

# extent_sar=setExtent(sar, me, keepres=FALSE, snap=FALSE) 
# 
extent_p=setExtent(p, me, keepres=FALSE, snap=FALSE) 
p_tif=writeRaster(extent_p,paste(input,"p1.tif",sep=""),options="COMPRESS=NONE",datatype='INT4S',overwrite=TRUE)
# sar_tif=writeRaster(extent_sar,paste(input,"slara.tif",sep=""),options="COMPRESS=NONE",datatype='FLT4S',overwrite=TRUE,format="GTiff")
# wi=sar
# wi[,]=-log(sar[,])
# #plot(wi)


# Distance Down
system(paste("mpiexec -n 8 D8HDistToStrm -p ",(paste(input,"p.tif",sep="")), "-src", (paste(input,"src.tif",sep="")),"-dist",(paste(input,"dist.tif",sep=""))),show.output.on.console=F, invisible=F)
dist=(raster(paste(input,"dist.tif",sep="")))

# extent_dist=setExtent(dist, me, keepres=FALSE, snap=FALSE) 
# dist_tif=writeRaster(extent_dist,paste(input,"dista.tif",sep=""),options="COMPRESS=NONE",datatype='FLT4S',format="GTiff",overwrite=TRUE)
# plot(raster(paste(input,"dist.tif",sep="")))






###comments:

##(1) need to check again
##(2) change thershold
##(3) need to change outlet shape file information for the stream gaueg moved function
##(4) make confirm whether we  choose D8 distance to stream or Din distance to down

##Get reachlink,nodelink, and other properties
system(paste("reachlink -me", (paste(input,"me.tif",sep="")),"-p",(paste(input,"p.tif",sep="")),"-tree", (paste(input,"tree.txt",sep="")),"-coord",(paste(input,"coord.txt",sep="")),"-reachlink","rchlink.txt","-nodelink", "nodelinks.txt","-nc",(paste(input,"nc.tif",sep="")),"-dc", (paste(input,"dc.tif",sep="")),"-rca","rchareas.txt", "-rcp", "rchproperties.txt"))

#Comment
#if the avove is not work, then change the extent for the p file, it will work


##get wetness index and distance to stream distribution
system(paste("twidistsetup -me",  (paste(input,"me.tif",sep="")),"-twi", (paste(input,"sar.tif",sep="")),"-dis",(paste(input,"dist.tif",sep="")),"-dists","distribution.txt"))

##Need data, NO DATA found