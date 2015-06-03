
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

watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
w=2;bn=1
dir=(paste("E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\",watsed[w],"_Watershed_final\\",watsed[w],"_results\\","streamflow_files",sep=""))##for A1:
setwd(dir)

filenames =list.files(path=dir,pattern="*.txt")

time_his=seq(as.Date("1985/1/1"), as.Date("2005/12/31"), "day")
time_his1=seq(as.Date("1986/10/1"), as.Date("2005/9/30"), "day")
time_proj1=seq(as.Date("2046/1/1"), as.Date("2065/12/31"), "day")
time_proj11=seq(as.Date("2046/10/1"), as.Date("2065/9/30"), "day")
time_proj2=seq(as.Date("2076/1/1"), as.Date("2095/12/31"), "day")
time_proj22=seq(as.Date("2076/10/1"), as.Date("2095/9/30"), "day")
date_op_his=match(time_his1,time_his) # get overlap time interval
date_op_proj1=match(time_proj11,time_proj1) # get overlap time interval
date_op_proj2=match(time_proj22,time_proj2) # get overlap time interval


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
  

  stano=filenames[stn_ctr]
  stnvar[stn_ctr,58]=stano
  
  d1=strtoi(unlist(strsplit(filenames[stn_ctr],NULL)))
  d2=strtoi(d1[length(d1)-5])
  
  if(d2==4){date=time_proj11;date_lap=date_op_proj1}else if(d2==7){date=time_proj22;date_lap=date_op_proj2}
  else {date=time_his1;date_lap=date_op_his}
  
  if (length(grep("calibrated",stano))==1) {
    ff=scan(stano, what="")
    l=bn+2#for A2
    ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
    simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
    flow=data.frame(date=date,q=simu_flow[date_lap])
    flow[flow<0.001]=0.001
    WYmat=WY_conv.R(flow) # Convert the data to Water year
    
  }else if (length(grep("observed",stano))==1)   {
    
    sf=scan(stano, what="")
    sf1=sf[seq(21,length(sf),1)] ## need to change theis things later
    obs_flow=(as.numeric(sf1[seq(1,length(sf1),3)])) ##need to change this later
    
    
    time_all_observ= seq(as.Date("1980/1/1"), as.Date("2012/12/31"), "day") #create time series
    
    date_overlap_obs=match(date,time_all_observ) # get overlap time interval
    flow=data.frame(date=date,q=obs_flow[date_overlap_obs])
    WYmat=WY_conv.R(flow) 
    
  }else {
    
    ff=scan(stano, what="")
    l=bn+2#for A2
    ff1=ff[seq(l,length(ff),1)] ## need to change theis things later
    simu_flow=matrix(as.numeric(ff1[seq(2,length(ff1),l-1)])) ##need to change this later
    flow=data.frame(date=date,q=simu_flow[date_lap])
    flow[flow<0.001]=0.001
    WYmat=WY_conv.R(flow) # Convert the data to Water year
    
  }
   
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

#write.csv(stnvar,file="A1_climate_change_regime.xls")

rv_proj1=matrix(as.numeric(stnvar[,1:57]),nrow=27,ncol=57)
rv_26_46=seq(1,length(filenames)-2,5);rv_26_76=seq(2,length(filenames)-2,5);rv_85_46=seq(3,length(filenames)-2,5);rv_85_76=seq(4,length(filenames)-2,5)
his=seq(5,length(filenames)-2,5);cal=length(filenames)-1;obs=length(filenames)
rt_GCM=c(his,rv_26_46,rv_26_76,rv_85_46,rv_85_76)
resul_GCM=rv_proj1[rt_GCM,]
col_names=matrix(col_name)
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)

all_his=resul_GCM[1:5,var_order];
all_26_46=resul_GCM[6:10,var_order];all_26_76=resul_GCM[11:15,var_order]
all_85_46=resul_GCM[16:20,var_order];all_85_76=resul_GCM[21:25,var_order]

############first historical comparison ################################
obs_regime=rv_proj1[obs,var_order]
cal_regime=rv_proj1[cal,var_order]

#all_results=data.frame(obs=obs_regime,daymet=cal_regime,modhis=all_his,mod2646=all_26_46,mod2676=all_26_76,mod8546=all_85_46,mod8576=all_85_76)
all_results=rbind(obs_regime,cal_regime,all_his,all_26_46,all_26_76,all_85_46,all_85_76)

write.table(all_results,file="C22_climate_change_regime.txt")












#######verification o fresults###########








diff1=((median_his/obs_regime)-1)*100

plot(df$Qmean_whole[rv_56_4.5],main="Qmean",type="o",pch=21,ylim=c(-70,70),col='red',lty=1,xlab=" ",ylab="%change",axes=FALSE)
lines(df$Qmean_whole[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$Qmean_whole[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3, ylim=c(-70,70),col="red")
lines(df$Qmean_whole[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1,at=seq(1,4,1),x[1:4],labels=FALSE) #), at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)









##################Reading all regime files and compare resutls ###########
dir="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\Results_Analysis"
setwd(dir)
regime_files =list.files(path=dir,pattern="*.txt")
sg=read.table("A1_climate_change_regime.txt",skip=1)
dss=data.frame(matrix(NA,ncol=17,nrow=1))

for ( m in 1:length(regime_files)){
  hjk=read.table(regime_files[m],skip=1)
  dss=cbind(dss,hjk)
}

dss=dss[,-1]
dss=data.matrix(dss)

obs=(dss[,seq(1,dim(dss)[2],7)]);obs=apply(obs,2,as.numeric)

daymet=(dss[,seq(2,dim(dss)[2],7)]);daymet=apply(daymet,2,as.numeric)
model=(dss[,seq(3,dim(dss)[2],7)]);model=apply(model,2,as.numeric)
mod2646=(dss[,seq(4,dim(dss)[2],7)]);mod2646=apply(mod2646,2,as.numeric)
mod2676=(dss[,seq(5,dim(dss)[2],7)]);mod2676=apply(mod2676,2,as.numeric)
mod8546=(dss[,seq(6,dim(dss)[2],7)]);mod8546=apply(mod8546,2,as.numeric)
mod8576=(dss[,seq(7,dim(dss)[2],7)]);mod8576=apply(mod8576,2,as.numeric)

CE=matrix(NA,nrow=16,ncol=3)
for ( i in 1:16){

CE[i,1]=NSE(daymet[i,],obs[i,])
CE[i,2]=NSE(model[i,],daymet[i,])
CE[i,3]=NSE(model[i,],obs[i,])
}


###see how changes occur#########

change2646=((mod2646/model)-1)*100
change2676=((mod2676/model)-1)*100
change8546=((mod8546/model)-1)*100
change8576=((mod8576/model)-1)*100

m=8
op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(change2646[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,50))
lines(change8546[,m],col='grey',lty=1,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2676[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,50))
lines(change8576[,m],col='grey',lty=1,lwd=3,type='o')
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)

title(xlab="Month", col.lab=rgb(0,0,0))
title(ylab="Monthly Rainfall (mm)", col.lab=rgb(0,0,0))



plot(df$Qmean_whole[rv_56_4.5],main="Qmean",type="o",pch=21,ylim=c(-70,70),col='red',lty=1,xlab=" ",ylab="%change",axes=FALSE)
lines(df$Qmean_whole[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$Qmean_whole[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3, ylim=c(-70,70),col="red")
lines(df$Qmean_whole[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1,at=seq(1,4,1),x[1:4],labels=FALSE) #), at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)


##plotting q7max

plot(df$Q7max[rv_56_4.5],main=" Q7max",type="o",pch=21,ylim=c(-70,100),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$Q7max[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$Q7max[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$Q7max[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4],labels=FALSE)
axis(side=2)
box()



# ##plotting Q7min ########problem
# 
# plot(df$Q7min[rv_56_4.5],main="Change of Qmean",type="o",pch=21,ylim=c(-70,100),col='red',lty=1,xlab="GCM Model",axes=FALSE,ylab="%change")
# lines(df$Q7min[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
# lines(df$Q7min[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
# lines(df$Q7min[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
# lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
# #legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
# axis(side=1, at=seq(1,4,1),x[1:4])
# axis(side=2)
# box()


##plotting q167


plot(df$BFF_Ln[rv_56_4.5],main=" Q167",type="o",pch=21,ylim=c(-70,100),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$BFF_Ln[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$BFF_Ln[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$BFF_Ln[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)

##plotting T50


plot(df$T50[rv_56_4.5],main="T50",type="o",pch=21,ylim=c(-40,10),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$T50[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$T50[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$T50[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4])
axis(side=2)
box()



op <- par(mfrow = c(2,2),
          oma = c(5,4,0,0) + 0.15,
          mar = c(0,0,1,1) + 0.5)

##plotting Qmean
x=c('CanESm2','CCSM4','CNRM-CM5','MRI-CGCM3')
plot(df$Pk_time[rv_56_4.5],main="Peaktime",type="o",pch=21,ylim=c(-30,30),col='red',lty=1,xlab=" ",ylab="%change",axes=FALSE)
lines(df$Pk_time[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$Pk_time[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$Pk_time[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1,at=seq(1,4,1),x[1:4],labels=FALSE) #), at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)


##plotting q7max

plot(df$FD_Ln[rv_56_4.5],main="FLUDDUR",type="o",pch=21,ylim=c(-100,30),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$FD_Ln[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$FD_Ln[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$FD_Ln[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4],labels=FALSE)
axis(side=2)
box()



# ##plotting Q7min ########problem
# 
# plot(df$Q7min[rv_56_4.5],main="Change of Qmean",type="o",pch=21,ylim=c(-70,100),col='red',lty=1,xlab="GCM Model",axes=FALSE,ylab="%change")
# lines(df$Q7min[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
# lines(df$Q7min[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
# lines(df$Q7min[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
# lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
# #legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
# axis(side=1, at=seq(1,4,1),x[1:4])
# axis(side=2)
# box()


##plotting q167


plot(df$FR[rv_56_4.5],main="FLOW REVERSAL",type="o",pch=21,ylim=c(-10,10),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$FR[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$FR[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$FR[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4])
axis(side=2)
box()
mtext(side = 2, text = "% Change", line = 2.5)

##plotting T50


plot(df$HFE[rv_56_4.5],main="HFE",type="o",pch=21,ylim=c(20,200),col='red',lty=1,xlab="",axes=FALSE,ylab="")
lines(df$HFE[rv_56_8.5],main="56-65 8.5",type="o",pch=22, lty=1, col="blue")
lines(df$HFE[rv_90_4.5],main="56-65 8.5",type="o", pch=15, lty=3,col="red")
lines(df$HFE[rv_90_8.5],main="56-65 8.5",type="o", pch=16, lty=3, col="blue")
lines(seq(1,4,1),c(0,0,0,0),lty=6,pch=25,col="grey")
#legend('topright', c("56-65 rcp 4.5","56-65 rcp 8.5","90-99 rcp 4.5","90-99 rcp 8.5"), cex=0.8, col=c("red","blue","red","blue"), pch=c(21,22,15,16), lty=c(1,1,3,3));
axis(side=1, at=seq(1,4,1),x[1:4])
axis(side=2)
box()






























##plotting HFE
plot(df$HFE[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-20,200),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$HFE[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$HFE[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-20,200),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$HFE[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);


##plotting LFE
plot(df$LFE[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-50,50),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$LFE[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$LFE[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-50,70),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$LFE[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);







##plotting ZFE
plot(df$ZFE[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(0,150),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$ZFE[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$ZFE[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(0,170),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$ZFE[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);


##plotting SI###########problem

plot(df$SI[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(0,150),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$SI[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$SI[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-100,170),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$SI[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);





##plotting M

plot(df$M[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-75,75),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$M[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$M[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-100,170),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$M[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);


##plotting COV



plot(df$CoV_whole[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-75,75),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$CoV_whole[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$CoV_whole[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-100,170),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$CoV_whole[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);


##plotting p

plot(df$P[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-75,75),col='red')
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$P[rv_56_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);

plot(df$P[rv_90_4.5],main="56-65 8.5",type="o", pch=22, lty=2, ylim=c(-100,170),col="red")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
lines(df$P[rv_90_8.5],main="56-65 8.5",type="o", pch=22, lty=2, col="blue")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);



##plotting c

plot(df$C[rv_56_4.5],main="56-65 4.5",type="o",ylim=c(-75,75),col='red',lty="dotted")
lines(df$C[rv_56_8.5],main="56-65 8.5",type="o", pch=22, col="blue",lty="dotted")
lines(df$C[rv_90_4.5],main="56-65 8.5",type="o", pch=22,ylim=c(-100,170),col="red",lty="dotdash")
lines(df$C[rv_90_8.5],main="56-65 8.5",type="o", pch=22,col="blue",lty="dotdash")
lines(seq(1,4,1),c(0,0,0,0),lty="dashed",pch=25,col="grey")
legend('topleft', c("rcp 4.5","rcp 8.5"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2);



