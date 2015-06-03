
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/getData.r")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/WY_conv.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/PCM.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/Q7MaxMin.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/timings.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/TPTH.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/flowrev.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/BFI_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/COVyr.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/MEANyr.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/zeroday.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/q167.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/q167_lp3.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/COV.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/qmean_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/qmean_whole.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/SI.R")
source("E:/USU_Research_work/TOPNET PROJECT/MODEL COMPARISON/From_Sulochan/Sixteen_variables/Functions for R/flw_puls_evnt_5_95.R")

library(hydroGOF)
library(zoo)
library(plyr)
library(nsga2R)
library(hydroTSM)




##trying to do some sensttivity analysis:

##precipitation change:


regime_change=function(delP,delT,a,b,c){
  
  #regime_change=function(delP,delT) 
  
  dir=("E:\\USU_Research_work\\SENSITIVITY_PROJECT\\A1_TOPNETRUN")
  setwd(dir)
  rainfall=read.table('rain_org.dat',skip=3)
  rain1=rainfall[,(1:(ncol(rainfall)-2))]
  rain2=rain1
  
  
  #   s1=12,1,2 : DJF
  #   s2=3,4,5 : MAM
  #   s3=6,7,8 : JJA
  #   s4= 9,10,11 : SON
  dates=seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day")
  date_df <- data.frame(date = dates,
                        year = as.numeric(format(dates, format = "%Y")),
                        month = as.numeric(format(dates, format = "%m")),
                        day = as.numeric(format(dates, format = "%d")))
  
  season=matrix(which(date_df$month==a |date_df$month==b | date_df$month==c ))
  #season=matrix(which(date_df$month==month))
  s_Rain=rain2[season,]*delP
  rain2[season,]=s_Rain
  date=rainfall[,(ncol(rainfall)-1)]
  hour=rainfall[,(ncol(rainfall))]
  
  temp=read.table('tmaxtmintdew_org.dat',skip=3)
  max_temp=temp[,seq(1,(ncol(temp)-2),3)]
  min_temp=temp[,seq(2,(ncol(temp)-2),3)]
  dew_temp=temp[,seq(3,(ncol(temp)-2),3)]
  s_maxT=max_temp[season,]+delT
  max_temp[season,]=s_maxT
  
  s_minT=min_temp[season,]+delT
  min_temp[season,]=s_minT
  
  s_dewT=dew_temp[season,]+delT
  dew_temp[season,]=s_dewT
  
  
  nc=(ncol(rainfall))-2
  tmaxtmintdew=matrix(ncol = 1, nrow = nrow(max_temp))
  for ( k in 1:(nc)){
    
    tmaxtmintdew=cbind(tmaxtmintdew,max_temp[,k],min_temp[,k],dew_temp[,k])
  }
  
  
  
  
  precip=data.frame(rain2)
  temper=data.frame(tmaxtmintdew[,-1])
  
  precip[] <- lapply(precip, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)}) 
  temper[] <- lapply(temper, function(.col){ if (is.numeric(.col)) return(sprintf("%8.2f",.col))else return(.col)})
  
  precip=data.frame(precip,date,hour)
  temper=data.frame(temper,date,hour)
  
  
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
  
  system(paste("topnet_modified"))
  
  
  
  filenames="FlowAtStreamNodes_cms.txt"
  time_his=seq(as.Date("1995/1/1"), as.Date("2005/12/31"), "day")
  time_his1=seq(as.Date("1996/10/1"), as.Date("2005/9/30"), "day")
  date_op_his=match(time_his1,time_his) # get overlap time interval
  
  
  #stns=read.table("StnList.txt")
  nsta=length(filenames)
  stnvar=matrix(NA,nrow=nsta,ncol=58)
  col_name=c("BFI","Slp_BFI","Pval_BFI",
             "CoV_whole",
             "CoV_yr","Slp_CoV","Pval_CoV",
             "Qmean_whole",
             "Qmean_yr","Slp_Qmean","Pval_Qmean",
             "Zero","Slp_Zero","Pval_Zero",
             "BFF_Ln",
             "FD_Ln","Slp_FD_Ln","Pval_FD_Ln",
             "BFF_Lp3",
             "FD_Lp3","Slp_FD_Lp3","Pval_FD_Lp3",
             "P","C","M",
             "T25","Slp_T25","Pval_T25",
             "T50","Slp_T50","Pval_T50",
             "T75","Slp_T75","Pval_T75",
             "Pk_time","H1",
             "Q7min","Slp_Q7min","Pval_Q7min",
             "Q7max","Slp_Q7max","Pval_Q7max",
             "FR","Slp_FR","Pval_FR",
             "SI","Slp_SI","Pval_SI",
             "HFE","Slp_HFE","Pval_HFE",
             "LFE","Slp_LFE","Pval_LFE",
             "ZFE","Slp_ZFE","Pval_ZFE",
             "Stn_No")
  colnames(stnvar)=col_name
  
  for (stn_ctr in 1:nsta){
    #A1=29,A2=113,B1=25,B21=135,B22=43,C1=131,C21=59,C22=43
    bn=29
    
    stano=filenames[stn_ctr]
    #stano=filenames
    stnvar[stn_ctr,58]=stano
    date=time_his1;date_lap=date_op_his
    ff=scan(stano, what="")
    l=bn+2#for A2
    ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
    simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
    flow=data.frame(date=date,q=simu_flow[date_lap])
    flow[flow<0.001]=0.001
    WYmat=WY_conv.R(flow) # Convert the data to Water year
    
    
    
    timing=t25t50t75(WYmat)
    t25=as.numeric(timing[1])
    t50=as.numeric(timing[4])
    t75=as.numeric(timing[7])
    slope_t25=as.numeric(timing[2])
    slope_t50=as.numeric(timing[5])
    slope_t75=as.numeric(timing[8])
    pval_t25=as.numeric(timing[3])
    pval_t50=as.numeric(timing[6])
    pval_t75=as.numeric(timing[9])
    # if matrix of T25, T50 and T75 is needed:
    # matT25=unlist(timing[10])
    # matT50=unlist(timing[11])
    # matT75=unlist(timing[12])
    stnvar[stn_ctr,26]=t25
    stnvar[stn_ctr,27]=slope_t25
    stnvar[stn_ctr,28]=pval_t25
    stnvar[stn_ctr,29]=t50
    stnvar[stn_ctr,30]=slope_t50
    stnvar[stn_ctr,31]=pval_t50
    stnvar[stn_ctr,32]=t75
    stnvar[stn_ctr,33]=slope_t75
    stnvar[stn_ctr,34]=pval_t75
    
    pkhar1=tpth(WYmat)
    pktime=as.numeric(pkhar1[1])
    har1=as.numeric(pkhar1[2])
    stnvar[stn_ctr,35]=pktime
    stnvar[stn_ctr,36]=har1
    
    max7min7=Q7MaxMin.R(WYmat)
    Q7min=as.numeric(max7min7[1])
    slope_Q7min=as.numeric(max7min7[2])
    pval_Q7min=as.numeric(max7min7[3])
    
    Q7max=as.numeric(max7min7[5])
    slope_Q7max=as.numeric(max7min7[6])
    pval_Q7max=as.numeric(max7min7[7])
    stnvar[stn_ctr,37]=Q7min
    stnvar[stn_ctr,38]=slope_Q7min
    stnvar[stn_ctr,39]=pval_Q7min
    stnvar[stn_ctr,40]=Q7max
    stnvar[stn_ctr,41]=slope_Q7max
    stnvar[stn_ctr,42]=pval_Q7max
    
    fr_list= flowrev(WYmat)
    fr=as.numeric(fr_list[1])
    slope_fr=as.numeric(fr_list[2])
    pval_fr=as.numeric(fr_list[3])
    # if matrix of yearly Flow Reversal is needed:
    # mat=unlist(fr_list[4])
    stnvar[stn_ctr,43]=fr
    stnvar[stn_ctr,44]=slope_fr
    stnvar[stn_ctr,45]=pval_fr
    
    bfi_list=BFI_whole.R(WYmat)
    BFI=as.numeric((bfi_list)[1])
    slope_BFI=as.numeric((bfi_list)[2])
    pval_BFI=as.numeric((bfi_list)[3])
    stnvar[stn_ctr,1]=BFI
    stnvar[stn_ctr,2]=slope_BFI
    stnvar[stn_ctr,3]=pval_BFI
    
    # if matrix of yearly BFI is needed:
    # mat=unlist(bfi_list[4])
    
    cov=COV.R(WYmat)
    stnvar[stn_ctr,4]=cov  
    
    cov_list=COVyr.R(WYmat)
    cov_yr=as.numeric(cov_list[1])
    slope_cov=as.numeric(cov_list[2])
    pval_cov=as.numeric(cov_list[3])
    # if matrix of yearly CoV is needed:
    # mat=unlist(cov_list[4])
    stnvar[stn_ctr,5]=cov_yr
    stnvar[stn_ctr,6]=slope_cov
    stnvar[stn_ctr,7]=pval_cov
    
    qmean1=qmean_whole.R(WYmat)
    stnvar[stn_ctr,8]=qmean1
    
    qmean_list=MEANyr.R(WYmat)
    qmean_yr=as.numeric(qmean_list[1])
    slope_qmean=as.numeric(qmean_list[2])
    pval_qmean=as.numeric(qmean_list[3])
    # if matrix of yearly Qmean is needed:
    # mat=unlist(qmean_list[4])
    stnvar[stn_ctr,9]=qmean_yr
    stnvar[stn_ctr,10]=slope_qmean
    stnvar[stn_ctr,11]=pval_qmean
    
    zero_list=zeroday.R(WYmat)
    zero_yr=as.numeric(zero_list[1])
    slope_zero=as.numeric(zero_list[2])
    pval_zero=as.numeric(zero_list[3])
    # if matrix of yearly Qmean is needed:
    # mat=unlist(zero_list[4])
    stnvar[stn_ctr,12]=zero_yr
    stnvar[stn_ctr,13]=slope_zero
    stnvar[stn_ctr,14]=pval_zero
    
    bff_ln_list=q167.R(WYmat) ## bank Full Flow = Q1.67 using log-Normal
    bff_ln=as.numeric(bff_ln_list[1])
    flddur_ln=as.numeric(bff_ln_list[2]) ## Flood Duration = Q1.67 using Log-Normal
    slope_flddur=as.numeric(bff_ln_list[3])
    pval_flddur=as.numeric(bff_ln_list[4])
    # if matrix of yearly Flood Duration is needed:
    # mat=unlist(bff_ln_list[5])
    stnvar[stn_ctr,15]=bff_ln
    stnvar[stn_ctr,16]=flddur_ln
    stnvar[stn_ctr,17]=slope_flddur
    stnvar[stn_ctr,18]=pval_flddur
    
    bff_lp3_list=q167_lp3.R(WYmat) ## bank Full Flow = Q1.67 using log-Pearson III
    bff_lp3=as.numeric(bff_lp3_list[1])
    flddur_lp3=as.numeric(bff_lp3_list[2]) ## Flood Duration = Q1.67 using Log-Pearson III
    slope_flddur_lp3=as.numeric(bff_lp3_list[3])
    pval_flddur_lp3=as.numeric(bff_lp3_list[4])
    # if matrix of yearly Flood Duration is needed:
    # mat=unlist(bff_lp3_list[5])
    stnvar[stn_ctr,19]=bff_lp3
    stnvar[stn_ctr,20]=flddur_lp3
    stnvar[stn_ctr,21]=slope_flddur_lp3
    stnvar[stn_ctr,22]=pval_flddur_lp3
    
    pcm_list=PCM(flow)
    p=as.numeric(pcm_list[1])
    c=as.numeric(pcm_list[2])
    m=as.numeric(pcm_list[3])
    # if contingency table (bin matrix) is needed:
    # vec = unlist(pcm_list[4])
    # mat = matrix(vec,nrow=7,ncol=12)
    stnvar[stn_ctr,23]=p
    stnvar[stn_ctr,24]=c
    stnvar[stn_ctr,25]=m
    
    SI_list=SI.R(WYmat)
    SI=as.numeric(SI_list[1])
    slope_SI=as.numeric(SI_list[2])
    pval_SI=as.numeric(SI_list[3])
    # if matrix of yearly Qmean is needed:
    # mat=unlist(SI_list[4])
    stnvar[stn_ctr,46]=SI
    stnvar[stn_ctr,47]=slope_SI
    stnvar[stn_ctr,48]=pval_SI
    
    FE_list=flw_puls_evnt_5_95.R(WYmat)
    LFE=as.numeric(FE_list[1])
    HFE=as.numeric(FE_list[2])
    ZFE=as.numeric(FE_list[3])
    
    slope_LFE=as.numeric(FE_list[7])
    pval_LFE=as.numeric(FE_list[8])
    slope_HFE=as.numeric(FE_list[9])
    pval_HFE=as.numeric(FE_list[10])
    slope_ZFE=as.numeric(FE_list[11])
    pval_ZFE=as.numeric(FE_list[12])
    
    # if matrix of yearly HFE,LFE and ZFE is needed:
    # matLFE=unlist(FE_list[4])
    # matHFE=unlist(FE_list[5])
    # matZFE=unlist(FE_list[6])
    
    stnvar[stn_ctr,49]=HFE
    stnvar[stn_ctr,50]=slope_HFE
    stnvar[stn_ctr,51]=pval_HFE
    
    stnvar[stn_ctr,52]=LFE
    stnvar[stn_ctr,53]=slope_HFE
    stnvar[stn_ctr,54]=pval_HFE
    
    stnvar[stn_ctr,55]=ZFE
    stnvar[stn_ctr,56]=slope_ZFE
    stnvar[stn_ctr,57]=pval_ZFE
    
    
  }
  
  
  rv_proj1=matrix(as.numeric(stnvar[,1:57]),nrow=1,ncol=57)
  var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
  resul_GCM=rv_proj1[,var_order]
  regime_results=resul_GCM
}

a=seq(0.6,1.4,0.1)
b=seq(-2,6,1)
a=1.1
b=1
df=expand.grid(x=a,y=b)
#rg=mapply(regime_change,df$x,df$y)
mon=matrix(c(12,1,2,3,4,5,6,7,8,9,10,11),nrow=3,ncol=4)
GD=matrix(NA,nrow=16,4)
for (i in 1:4){
GD[,i]=regime_change(a,b,mon[1,i],mon[2,i],mon[3,i])

}

REAL=regime_change(1,0,1,1,1)
weight_A1_ses=(GD/REAL)-1
write.table(rg,"B21_precp_temp_s4.txt")
















col_name=c("BFI","Slp_BFI","Pval_BFI",
           "CoV_whole",
           "CoV_yr","Slp_CoV","Pval_CoV",
           "Qmean_whole",
           "Qmean_yr","Slp_Qmean","Pval_Qmean",
           "Zero","Slp_Zero","Pval_Zero",
           "BFF",
           "FLDD","Slp_FD_Ln","Pval_FD_Ln",
           "BFF_Lp3",
           "FD_Lp3","Slp_FD_Lp3","Pval_FD_Lp3",
           "P","C","M",
           "T25","Slp_T25","Pval_T25",
           "T50","Slp_T50","Pval_T50",
           "T75","Slp_T75","Pval_T75",
           "Pk_time","H1",
           "Q7min","Slp_Q7min","Pval_Q7min",
           "Q7max","Slp_Q7max","Pval_Q7max",
           "FR","Slp_FR","Pval_FR",
           "SI","Slp_SI","Pval_SI",
           "HFE","Slp_HFE","Pval_HFE",
           "LFE","Slp_LFE","Pval_LFE",
           "ZFE","Slp_ZFE","Pval_ZFE",
           "Stn_No")
col_names=matrix(col_name)
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
names=col_names[var_order]

r=matrix(ra,nrow=576,ncol=1)
variables=matrix(rep(names,each=36))
scena1=data.frame(x=rep(df$x,16))
scena2=data.frame(y=rep(df$y,16))

library(ggplot2)

# create fake dataset with additional attributes - sex, sample, and temperature
x <- data.frame(
  
  x =scena1,
  y=scena2,
  values=r,
  sample = variables
)

write.table(x,"res.xls")
p1 <- ggplot(subset(x, sample=="FLDD"), aes(x,y))+
  geom_raster(aes(fill=values))+
  scale_fill_gradientn(colours=topo.colors(8),na.value = "transparent",
                       breaks=c(10,25,35),labels=c("Minimum",1.5,"Maximum"),
                       limits=c(10,35))
+ facet_grid(type ~ var) + 
  theme(legend.position="bottom", plot.margin = unit(c(1,-1,1,0.2), "line"))


d=ggplot(x, aes(x=x, y=y,)) +geom_raster(aes(fill=values))

d + facet_wrap(~ sample,scales = "free")

# compare different sample populations across various temperatures
d=ggplot(x, aes(x=scenario, y=values,fill = scenario,colour=scenario)) +
  geom_point(size=4)
d + facet_wrap(~ sample,scales = "free")


+geom_hline(yintercept = 0.0,colour="black",size=1)+
  theme_bw()+theme(legend.position="none")+scale_color_manual(values = c("green", "red"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  xlab("Watersheds") + ylab("Regime change(%)")+ggtitle(" Year 2076-2095 compare to 1986-2005 ")



# justification
df <- expand.grid(x =a, y =b)
ra=t(rg)
df$z <-ra
# default is compatible with geom_tile()
ggplot(df, aes(x, y, fill = z)) + geom_raster()






