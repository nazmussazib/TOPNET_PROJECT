dir="E:\\USU_Research_work\\Other_Work\\SENSITIVITY_PROJECT\\Results\\precp_sensitivity"
setwd(dir)

rain_files=list.files(path=dir,pattern="_precipitation_sensitivity.txt")
a=seq(0.6,1.4,0.1)
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
           "ELFI","Slp_SI","Pval_SI",
           "HFE","Slp_HFE","Pval_HFE",
           "LFE","Slp_LFE","Pval_LFE",
           "ZFE","Slp_ZFE","Pval_ZFE",
           "Stn_No")
col_names=matrix(col_name)
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
names=col_names[var_order]
#names=names[c(2,3,9,10,11,13)] ## seasonal change r somoi only peak those 5/6 varaibles
#variable=matrix(rep(names,72))

sdnames=c("a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8")
rg_names=data.frame(name=matrix(rep(sdnames,72)))

porder=c(2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16) #=(mean,cov,q7max,q7min,bankful,fr,T50,p,c,m,HFE,LFE,peak,SI,FD,zfe)
ranames=names[porder]
ranames[1]="Qmean";ranames[2]="CV";ranames[13]="Peaktime"
#rg_names=data.frame(name=matrix(rep(sdnames,)))

##############Precipitation Sensitivity################

delp_p=t(matrix(rep((a-1),16),9,16))
rain_read=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,5]-1)*100
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  sg=matrix(sg,nrow=16,ncol=9)

}


rain_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain_files)) {
  pe=rain_read(rain_files[i])
  rain_sens=cbind(rain_sens,pe)
}
rain_sens=rain_sens[,-1]
##change the order of rain sens so that all of the plots are same as others (e.g. comparsion of stream flow regiem variables)
rain_sens=rain_sens[porder,]

rain_sens[is.na(rain_sens)]=NA
ax=seq(-40,40,10)
var_rain=matrix(rep(ax,each=16))
deltaP=matrix(rep(var_rain,8))
#rain_sens_selec=rain_sens[c(2,3,9,10,11,13),]
var_change_rain=matrix(rain_sens,nrow=(dim(rain_sens)[1])*(dim(rain_sens)[2]),ncol=1)
#var_name=c("a1","a2","a3","a4","a5","a6")
variable2=matrix(rep(names,72))
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed1=matrix((rep(watsed,each=144)))

rain_all <- data.frame(
  PercentChange=deltaP,
  changeofPrecp=var_change_rain,
  var=rg_names,
  Watershed=watershed1 
)

rain_all2=rain_all
#levels(rain_all2$var)[levels(rain_all2$var)==variable2] <-variable
#DATA2all <- DATA_all
##levels(DATA2all$name)[levels(DATA2all$name)==sdnames] <- variable2



levels(rain_all2$name)[levels(rain_all2$name)==sdnames] <- ranames
# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(rain_all2, aes(x=PercentChange, y=changeofPrecp, colour=Watershed)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ name,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(-40,40)+

  xlab("Percent Change of Precipitation")+
ylab("Percent Change of Streamflow Regime")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))






############# only temperature sensitivity analysis#############

dir="E:\\USU_Research_work\\Other_Work\\SENSITIVITY_PROJECT\\Results\\Temp_sensitivity"
setwd(dir)
temp_files =list.files(path=dir,pattern="*temperature_sensitivity.txt")
b=seq(-2,6,1)
del_T=t(matrix(rep((b),16),9,16))
temp_read=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,3]-1)*100
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  sg=matrix(sg,nrow=16,ncol=9)
  
}

temp_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(temp_files)) {
  te=temp_read(temp_files[i])
  temp_sens=cbind(temp_sens,te)
}
temp_sens=temp_sens[,-1]
temp_sens=temp_sens[porder,]

temp_sens[is.na(temp_sens)]=0

var_temp=matrix(rep(b,each=16))
deltaT=matrix(rep(var_temp,8))
var_change_temp=matrix(temp_sens,nrow=(dim(temp_sens)[1])*(dim(temp_sens)[2]),ncol=1)


temp_all <- data.frame(
  PercentChange=deltaT,
  changeofTemperature=var_change_temp,
  var=rg_names,
  Watershed=watershed1 
)

# compare different sample populations across various temperatures
# library(ggplot2)
# p1 <- ggplot(temp_all, aes(x=PercentChange, y=changeofTemperature, colour=place, group=place)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
#   geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
#   scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+
#   
#   
#   ylab("Percent Change")+
#   theme(strip.text.x = element_text(size=14, face="bold"),
#         axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))

temp_all2=temp_all
levels(temp_all2$name)[levels(temp_all2$name)==sdnames] <- ranames
# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(temp_all2, aes(x=PercentChange, y=changeofTemperature, colour=Watershed)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ name,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(-2,6)+
  
  xlab("Percent Change of Temperature")+
  ylab("Percent Change of Streamflow Regime")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))














  