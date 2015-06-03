setwd("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/A2_Watershed/A2_CD/A2_CD_present/A2_NLDAS/A2_N_Rainfall")
rain_nldas=readLines('rain.dat')
sf=unlist(strsplit(rain_nldas, split=" "))
sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
df=sf1[which(!sf1 == "" )]
obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later