
##copy water managemenet file


# origindir <- c("E:/USU_Research_work/TOPNET PROJECT/LittleBearRiver/FINAL/WaterManagementData")
# targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/COPYFILES")
# x=list.files(path = "E:/USU_Research_work/TOPNET PROJECT/LittleBearRiver/FINAL/WaterManagementData", pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,
# ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# 
# copyfiles=function(filestocopy){
# file.copy(paste (origindir, filestocopy , sep = "/"), paste(targetdir, filestocopy , sep = "/"),
# recursive=FALSE,copy.mode = TRUE)}
# lapply(x,copyfiles)


##getting rainweight for each raingauge

setwd("C:/Users/shams/Desktop/daymet")
arus=raster('PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil') # annual rainfall for whole united states downloaded from PRISM
ar_proj=projection(arus)
newproj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
arus_projected=projectRaster(arus, crs=newproj)
watershed=raster("dema11w.tif") # my model element
arus_tif=writeRaster(arus_projected,"arus.tif") # write raster to 
arwatershed=crop(arus_tif,watershed,"arwatershed.tif",overwrite=TRUE) # crop for the watershed area but got some pixel value out side boundary
arwat_samcell= resample(arwatershed, watershed, method='bilinear') # make same cell size as watershed
sum_ar_watrshed= overlay(arwat_samcell, watershed, fun=function(x,y){return(x+y)}) # sumof watershed and croped rainfall 
annual_rain_watershed=overlay(sum_ar_watrshed, watershed, fun=function(x,y){return(x-y)}) # subtract (sum of watershed and croped rainfall) to watershed for getting annural rainfall for watrshed
annrain_watershed=writeRaster(annual_rain_watershed,"annrain2.tif",datatype='FLT4S',options="COMPRESS=NONE") ## change datatype which will change 64 bit to 32 bit
system ("rainweight  -w dema11w.tif     -rg Export_Output_2.shp -ar annrain2.tif  -tri triout.tif -wt weights.txt")   


##need to check raingauge shape file





###
###automate modelspecification file

basin=data.matrix(getValues(watershed))
basin[!rowSums(!is.finite(basin)),]
num_basin=unique(basin)
Nbasin=length(num_basin[!rowSums(!is.finite(num_basin)),])

gauge=(read.dbf('Export_Output_2.dbf', header=FALSE))
Ngauge=length(gauge$dbf$ID)





res <- readLines("modelspc.dat")

cat(sprintf("%d %d %d %f", 10,11,12,3,5,"\n")) 


b=res[16]
c=b[1]
print res
sink("modelspc.dat") 
cat(sprintf("Ver2 - Automagically generated TOPMODEL input file.  Rain Weights Adjusted by factor 1.4 in East","\n"))
cat(sprintf("Ngage, nbasins   nreaches","\n"))

cat(sprintf("%d %d %d %f", 10,11,12,3,5,"\n")) 

cat(sprintf("The number of basin properties, Number of model parameters","\n"))

cat(sprintf("%d %d ", 10,11,"\n")) 
cat(sprintf("Units for subcatchment properties","\n"))
cat(sprintf("0.001","\n"))
cat(sprintf("Table to control properties used as calibration parameters","\n"))
cat(sprintf("Type (COL 1) designates subbasin property (1), reach property (2), or initial condition (3).","\n"))
cat(sprintf("SP (COL2) designates property position in respecive array (SP, SI, or RP)","\n"))
cat(sprintf("Hslp# (COL3) designates the model element this applies to (or <= 0 for all elements)","\n"))
cat(sprintf("Par (COL4). Entry in calibration parameter that multiplies value from this file.","\n"))
cat(sprintf("Type    SP(i,.) Hslp#   Par             Parameter map","\n"))

for i=1:npars; fprintf(fid,'%d %d %d %d %s\n',[pp(i,:) parnames(i,:)]);end


##automate to get latlonfromxy.dat











##automate for estimating lapse rate


















