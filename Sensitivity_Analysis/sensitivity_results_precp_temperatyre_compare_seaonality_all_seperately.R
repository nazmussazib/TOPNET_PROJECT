dir="E:\\USU_Research_work\\SENSITIVITY_PROJECT\\Results\\precp_sensitivity"
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
           "SI","Slp_SI","Pval_SI",
           "HFE","Slp_HFE","Pval_HFE",
           "LFE","Slp_LFE","Pval_LFE",
           "ZFE","Slp_ZFE","Pval_ZFE",
           "Stn_No")
col_names=matrix(col_name)
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
names=col_names[var_order]
#names=names[c(2,3,9,10,11,13)] ## seasonal change r somoi only peak those 5/6 varaibles
#variable=matrix(rep(names,72))



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
rain_sens[is.na(rain_sens)]=NA
var_rain=matrix(rep(a,each=16))
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
  var=variable2,
  place=watershed1 
)

#rain_all2=rain_all
#levels(rain_all2$var)[levels(rain_all2$var)==variable2] <-variable

# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(rain_all, aes(x=PercentChange, y=changeofPrecp, colour=place, group=place)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(0.5,1.5)+


ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


####some urbitary test ####

##question1: which one is more sensitive among Q7max, BFF, Q7min, mean flow??
a1=rain_sens[2,] #mean
a2=rain_sens[3,] # BFF
a3=rain_sens[10,] # Q7 min
a4=rain_sens[11,] #Q7max
me_q7_bff=t(rbind(a1,a2,a3,a4))
me_q7_bff=matrix(me_q7_bff,nrow=(dim(me_q7_bff)[1])*(dim(me_q7_bff)[2]),ncol=1)
var_fr=c("Mean","BFF","Q7min","Q7max")
var2=matrix(rep(var_fr,each=72))
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed2=matrix((rep(watsed,each=9)))
wat2=matrix(rep(watershed2,4))
deltaP2=matrix(rep(a,32))
rg_fr <- data.frame(
  PercentChange2=deltaP2,
  changeofPrecp2=me_q7_bff,
  var2=var2,
  place2=watershed2 
)

library(ggplot2)
p2 <- ggplot(rg_fr, aes(x=PercentChange2, y=changeofPrecp2, colour=var2, group=var2)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ place2 ,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan"))+xlim(0.5,1.5)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))



#####for seasonal sensitiviy test##################

rain_season_files=list.files(path=dir,pattern="*_precp")

rain_season_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain_season_files)) {
  pe=rain_read(rain_season_files[i])
  rain_season_sens=cbind(rain_season_sens,pe)
}
rain_season_sens=rain_season_sens[,-1]
rain_season_sens[is.na(rain_season_sens)]=0

var_rain=matrix(rep(a,each=16))
deltaP=matrix(rep(var_rain,4))
deltaP_sen=matrix(rep(deltaP,8))
var_change_rain_sn=matrix(rain_season_sens,nrow=(dim(rain_season_sens)[1])*(dim(rain_season_sens)[2]),ncol=1)
ses=c("s1","s2","s3","s4")
ses11=matrix((rep(ses,each=144)))
ses_sn=matrix(rep(ses11,8))
names1=names
variable_s=matrix(rep(names1,36))
variable_s_sn=matrix(rep(variable_s,8))
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed_sn=matrix((rep(watsed,each=576)))


rains_all <- data.frame(
  PercentChange=deltaP_sen,
  changeofPrecp=var_change_rain_sn,
  var=variable_s_sn,
  season=ses_sn,
  place=watershed_sn
)




rs=grep ("C22", rains_all$place) 
chose_all=rains_all[rs,]
##for winter season

library(ggplot2)
pss <- ggplot(chose_all, aes(x=PercentChange, y=changeofPrecp, colour=season, group=season)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(0.5,1.5)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))

#####checking super position for all of the variables ##############
dff=matrix(chose_all[,2],16,36)
q1=matrix(rowSums(matrix(dff[2,],9,4)))
q2=matrix(rowSums(matrix(dff[3,],9,4)))
q3=matrix(rowSums(matrix(dff[10,],9,4)))
q4=matrix(rowSums(matrix(dff[11,],9,4)))






##question1: which one is more sensitive among Q7max, BFF, Q7min, mean flow??
a1s=chose_all[grep ("Qmean_whole", chose_all$var),] #mean
a2s=chose_all[grep ("BFF", chose_all$var),] # BFF
a3s=chose_all[grep ("Q7min", chose_all$var),]# Q7 min
a4s=chose_all[grep ("Q7max", chose_all$var),]#Q7max
me_q7_bffs=(rbind(a1s,a2s,a3s,a4s))

library(ggplot2)
p2s <- ggplot(me_q7_bffs, aes(x=PercentChange, y=changeofPrecp, colour=var, group=var)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~season ,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan"))+xlim(0.5,1.5)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))










############# only temperature sensitivity analysis#############

dir="E:\\USU_Research_work\\SENSITIVITY_PROJECT\\Results\\Temp_sensitivity"
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
temp_sens[is.na(temp_sens)]=0

watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed1=matrix((rep(watsed,each=144)))
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
variable=matrix(rep(names,72))
var_temp=matrix(rep(b,each=16))
deltaT=matrix(rep(var_temp,8))
var_change_temp=matrix(temp_sens,nrow=(dim(temp_sens)[1])*(dim(temp_sens)[2]),ncol=1)


temp_all <- data.frame(
  PercentChange=deltaT,
  changeofTemperature=var_change_temp,
  var=variable,
  place=watershed1 
)

# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(temp_all, aes(x=PercentChange, y=changeofTemperature, colour=place, group=place)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+
  
  
  ylab("Percent Change")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))

##question1: which one is more sensitive among Q7max, BFF, Q7min, mean flow??
a1t=temp_all[grep ("Qmean_whole", temp_all$var),] #mean
a2t=temp_all[grep ("BFF", temp_all$var),] # BFF
a3t=temp_all[grep ("Q7min", temp_all$var),]# Q7 min
a4t=temp_all[grep ("Q7max", temp_all$var),]#Q7max
a5t=temp_all[grep ("Pk_time", temp_all$var),]# Q7 min
a6t=temp_all[grep ("T50", temp_all$var),]#Q7max


me_q7_bfft=(rbind(a1t,a2t,a3t,a4t,a5t,a6t))

library(ggplot2)
p2t <- ggplot(me_q7_bfft, aes(x=PercentChange, y=(changeofTemperature), colour=place)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~var ,scales ="free")+xlab("") +
  #scale_color_manual(values = c("grey80", "black","blue","cyan"))+
  xlim(-2,6)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


#############temperature seasonal_sensitivty test ###############

temp_season_files=list.files(path=dir,pattern="*_temp_")

temp_season_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(temp_season_files)) {
  te=temp_read(temp_season_files[i])
  temp_season_sens=cbind(temp_season_sens,te)
}
temp_season_sens=temp_season_sens[,-1]

b=seq(-2,6,1)
var_temp=matrix(rep(b,each=16))
deltaT=matrix(rep(var_temp,4))
deltaT_s=matrix(rep(deltaT,8))
var_change_temp=matrix(temp_season_sens,nrow=(dim(temp_season_sens)[1])*(dim(temp_season_sens)[2]),ncol=1)
ses=c("s1","s2","s3","s4")
ses11=matrix((rep(ses,each=144)))
ses11_sn=matrix(rep(ses11,8))
variable_s=matrix(rep(names,36))
var_sn=matrix(rep(variable_s,8))
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed_sn=matrix((rep(watsed,each=576)))
temp_s_all <- data.frame(
  PercentChange=deltaT_s,
  changeofPrecp=var_change_temp,
  var=var_sn,
  season=ses11_sn,
  place=watershed_sn
)

# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(temp_s_all, aes(x=PercentChange, y=changeofPrecp, colour=season, group=season)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan"))+xlim(0.5,1.5)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))



###By watershed########

rt=grep ("A1",temp_s_all$place) 
chose_all_t=temp_s_all[rt,]
##for winter season

# library(ggplot2)
# pss <- ggplot(chose_all, aes(x=PercentChange, y=changeofPrecp, colour=season, group=season)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
#   geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
#   scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(0.5,1.5)+
#   
#   
#   ylab("Percent Change of LFE")+
#   theme(strip.text.x = element_text(size=14, face="bold"),
#         axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))




##question1: which one is more sensitive among Q7max, BFF, Q7min, mean flow??
a1s_t=chose_all_t[grep ("Qmean_whole", chose_all_t$var),] #mean
a2s_t=chose_all_t[grep ("BFF", chose_all_t$var),] # BFF
a3s_t=chose_all_t[grep ("Q7min", chose_all_t$var),]# Q7 min
a4s_t=chose_all_t[grep ("Q7max", chose_all_t$var),]#Q7max
a5s_t=chose_all_t[grep ("Pk_time", chose_all_t$var),]# Q7 min
a6s_t=chose_all_t[grep ("T50", chose_all_t$var),]#Q7max
me_q7_bffs_t=(rbind(a1s_t,a2s_t,a3s_t,a4s_t,a5s_t,a6s_t))

library(ggplot2)
p2s <- ggplot(me_q7_bffs_t, aes(x=PercentChange, y=(changeofPrecp), colour=season, group=season)) +geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~var,scales ="free")+xlab("") +
  #scale_color_manual(values = c("grey80", "black","blue","cyan"))+
  xlim(-2,6)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))














  