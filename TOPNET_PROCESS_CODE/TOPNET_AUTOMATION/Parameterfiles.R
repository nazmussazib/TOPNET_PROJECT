
##copy water managemenet file


# origindir <- c("E:/USU_Research_work/TOPNET PROJECT/LittleBearRiver/FINAL/WaterManagementData")
# targetdir <- c("E:/USU_Research_work/TOPNET PROJECT/INPUT_AUTOMATION/COPYFILES")
# x=list.files(path = "E:/USU_Research_work/TOPNET PROJECT/LittleBearRiver/FINAL/WaterManagementData", pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,
#              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# 
# copyfiles=function(filestocopy){
#   file.copy(paste (origindir, filestocopy , sep = "/"), paste(targetdir, filestocopy , sep = "/"),
#             recursive=FALSE,copy.mode = TRUE)}
# lapply(x,copyfiles)


##getting rainweight for each raingauge









###
###automate modelspecification file

# basin=data.matrix(getValues(watershed))
# basin[!rowSums(!is.finite(basin)),]
# num_basin=unique(basin)
# Nbasin=length(num_basin[!rowSums(!is.finite(num_basin)),])
# 
# gauge=(read.dbf('Export_Output_2.dbf', header=FALSE))
# Ngauge=length(gauge$dbf$ID)
# 
# 
# 
# 
# 
# res <- readLines("modelspc.dat")
# 
# cat(sprintf("%d %d %d %f", 10,11,12,3,5,"\n")) 
# 
# 
# b=res[16]
# c=b[1]
# print res
# sink("modelspc.dat") 
# cat(sprintf("Ver2 - Automagically generated TOPMODEL input file.  Rain Weights Adjusted by factor 1.4 in East","\n"))
# cat(sprintf("Ngage, nbasins   nreaches","\n"))
# 
# cat(sprintf("%d %d %d %f", 10,11,12,3,5,"\n")) 
# 
# cat(sprintf("The number of basin properties, Number of model parameters","\n"))
# 
# cat(sprintf("%d %d ", 10,11,"\n")) 
# cat(sprintf("Units for subcatchment properties","\n"))
# cat(sprintf("0.001","\n"))
# cat(sprintf("Table to control properties used as calibration parameters","\n"))
# cat(sprintf("Type (COL 1) designates subbasin property (1), reach property (2), or initial condition (3).","\n"))
# cat(sprintf("SP (COL2) designates property position in respecive array (SP, SI, or RP)","\n"))
# cat(sprintf("Hslp# (COL3) designates the model element this applies to (or <= 0 for all elements)","\n"))
# cat(sprintf("Par (COL4). Entry in calibration parameter that multiplies value from this file.","\n"))
# cat(sprintf("Type    SP(i,.) Hslp#   Par             Parameter map","\n"))
# 
# for i=1:npars; fprintf(fid,'%d %d %d %d %s\n',[pp(i,:) parnames(i,:)]);end
# 

##automate to get latlonfromxy.dat











##automate for estimating lapse rate

















