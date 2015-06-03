######climate change result analysis######

dir="E:\\USU_Research_work\\TOPNET PROJECT\\MODEL COMPARISON\\Results_Analysis"
#dir="E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\Results and Discussion\\streamflow_regimes_changes"
setwd(dir)
regime_files =list.files(path=dir,pattern="*.txt")
sg=read.table("A1_climate_change_regime.txt",skip=1)
dss=data.frame(var=nrow(sg))

for ( m in 1:length(regime_files)){
  hjk=read.table(regime_files[m],skip=1)
  dss=cbind(dss,hjk)
}

dss=data.matrix(dss[,-1])


obs=(dss[1,]);
obs=matrix(obs,17,8);
obs=obs[-1,];
daymet=(dss[2,]);
daymet=matrix(daymet,17,8);daymet=daymet[-1,]
model=matrix(colMeans(dss[3:7,]),nrow=17,ncol=8);model=model[-1,]
model_26_46=matrix(colMeans(dss[8:12,]),nrow=17,ncol=8);model_26_46=model_26_46[-1,]
model_26_76=matrix(colMeans(dss[13:17,]),nrow=17,ncol=8);model_26_76=model_26_76[-1,]
model_85_46=matrix(colMeans(dss[18:22,]),nrow=17,ncol=8);model_85_46=model_85_46[-1,]
model_85_76=matrix(colMeans(dss[23:27,]),nrow=17,ncol=8);model_85_76=model_85_76[-1,]


######serial based on performance:
#order=2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16=(mean,cov,q7max,q7min,bankful,fr,T50,p,c,m,HFE,LFE,peak,SI,FD,zfe)
porder=c(2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16)
model1=model[porder,];obs1=obs[porder,];daymet1=daymet[porder,]
model_26_46f=model_26_46[porder,];model_26_76f=model_26_76[porder,]
model_85_46f=model_85_46[porder,];model_85_76f=model_85_76[porder,]


#model_his=dss[3:7,]
#model_26_46=dss[8:12,];model_26_76=dss[13:17,];model_85_46=dss[18:22,];model_85_76=dss[23:27,]

CE=matrix(NA,nrow=3,ncol=16)
for ( i in 1:16){
  
  CE[1,i]=NSE(daymet1[i,],obs1[i,])
  CE[2,i]=NSE(model1[i,],daymet1[i,])
  CE[3,i]=NSE(model1[i,],obs1[i,])
}


#########scatter plot to see how it looks like plot#########
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


######serial based on performance:
#order=2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16=(mean,cov,q7max,q7min,bankful,fr,T50,p,c,m,HFE,LFE,peak,SI,FD,zfe)
sdnames=c("a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8")
ranames=names[porder]
ranames[1]="Qmean";ranames[2]="CV";ranames[13]="Peaktime"
rg_names=data.frame(name=matrix(rep(sdnames,8)))
obs_data=data.frame(obs=matrix(obs1,128,1))
daymet_data=data.frame(daymet=matrix(daymet1,128,1))
model_data=data.frame(model=matrix(model1,128,1))

model_data_26_46=data.frame(model_26_46=matrix(model_26_46f,128,1))
model_data_26_76=data.frame(model_26_76=matrix(model_26_76f,128,1))
model_data_85_46=data.frame(model_85_46=matrix(model_85_46f,128,1))
model_data_85_76=data.frame(model_85_76=matrix(model_85_76f,128,1))

watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed1=matrix((rep(watsed,each=16)))
a1=rep(0,16)
b1=c(100,8,450,3,550,150,250,1,1,0.3,15,6,300,0.5,15,1)
DATA=cbind(obs_data,daymet_data,model_data,rg_names,watershed1)
DATA_all=cbind(model_data,model_data_26_46,model_data_26_76,model_data_85_46,model_data_85_76,rg_names,watershed1,ymin = rep(a1, each=8), xmin = rep(a1,8),xmax = rep(b1,8),ymax = rep(b1, 8))


X1=data.frame(X=matrix(rbind(matrix(model1,128,1),matrix(model1,128,1)),nrow=256,ncol=1))
Y1=data.frame(Y=matrix(rbind(matrix(model_85_46f,128,1),matrix(model_85_76f,128,1)),nrow=256,ncol=1))
name_agu=data.frame(name=matrix(rbind(matrix(rep(sdnames,8)),matrix(rep(sdnames,8))),nrow=256,ncol=1))
ymin_ag=data.frame(ymin=matrix(rbind(rep(a1, each=8),rep(a1, each=8)),nrow=256,ncol=1))
ymax_ag=data.frame(ymax=matrix(rbind(rep(b1, each=8),rep(a1, each=8)),nrow=256,ncol=1))
xmin_ag=data.frame(xmin=matrix(rbind(rep(a1, each=8),rep(a1, each=8)),nrow=256,ncol=1))
xmax_ag=data.frame(xmax=matrix(rbind(rep(b1, each=8),rep(a1, each=8)),nrow=256,ncol=1))
watsed3=c("A1_2.6","A2_2.6","B1_2.6","B21_2.6","B22_2.6","C1_2.6","C21_2.6","C22_2.6","A1_8.5","A2_8.5","B1_8.5","B21_8.5","B22_8.5","C1_8.5","C21_8.5","C22_8.5")                                            
watershed3=matrix((rep(watsed3,each=16)))
DATA_all_agu=cbind(X1,Y1,watershed3,name_agu,ymin_ag,ymax_ag,xmin_ag,xmax_ag)









###########stream flow regime variables calibration and validation#########

lm_eqn = function(df){
  m = lm(daymet~ obs, data=df)
  a=df$daymet;b=df$obs
  d=NSE(a,b)
  eq <- list(f = format(summary(m)$r.squared, digits = 2),t=format(d,digits=2))
  
  
  c(eq = paste("NSE=",as.character(as.expression(eq$t)),",R2=",as.character(as.expression(eq$f)),sep=""))               
}


#order=2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16
DATA2 <- DATA
labeldata2 <- ddply(DATA2,.(name),lm_eqn )
##label position for daymet vs observed

x=c(20,4,150,3.5,200,55,170,0.45,0.4,0.15,6,1,150,-0.1,1.8,1)
y=c(74,7.55,350,8,470,135,240,0.74,0.73,0.35,20,4,350,0.37,19,0.75)

#label posiiton for the daymet vs GCM
#x=c(30,3,120,2.5,180,70,150,0.45,0.4,0.1,5.5,2,180,0.1,5,0.0000)
#y=c(80,5.7,380,7.5,475,115,230,0.75,0.75,0.28,12.5,3.25,260,0.28,14,0.55)
#b=as.factor(rep(c(16:2),16))

p=c(24,21,24,22,21, 22,24,21)
s=c(6,6,6,6,6,6,6,6)


setwd("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\AGU_TOPNET")
labeldata=cbind(labeldata2,x,y)
levels(DATA2$name)[levels(DATA2$name)==sdnames] <- ranames
cbPalette <- c("blue", "blue", "gray60", "gray60", "gray60", "red", "red", "red")
print(ggplot(DATA2,aes(obs,daymet)) + geom_point(size=s,shape =p,colour=cbPalette,fill=cbPalette)+
  geom_smooth(method="lm",colour="black",linetype="dotted",size = 1.1) + 
  xlab("Observe") + ylab("Daymet") +
  #ggtitle("Stream flow Regime Variables") + 
  geom_text(data=labeldata, aes(x=x, y=y, label=eq,family="Times", fontface="bold"), parse=FALSE) +
  facet_wrap(~name,scales="free")+

theme_bw()+theme(legend.position="right")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="bold")))

ggsave("regime_estimate.tiff", width=12, height=13, dpi=500)
dev.off()

##############FOR AGU POSTER #########################
#####only change the shape and color ############











 
###Aikahne choto khato problem ase, prothome purata run dite hobe , terpor abra labeldata2 theke run dite hobe plot r jonno ; jani na ki problem hoisie??


########streamflow changes ########



library(ggplot2)
DATA2all <- DATA_all
levels(DATA2all$name)[levels(DATA2all$name)==sdnames] <- ranames
cor_p2 <- ggplot(DATA2all,aes(model,model_85_76)) + geom_point(size=s,shape =p,colour=cbPalette,fill=cbPalette)+geom_abline(intercept = 0)+
 
  xlab("Baseline(1986-2005) Streamflow Regime Varibles") + ylab("Future (2076-2095) Streamflow Regime Varibles under RCP8.5") +
  #ggtitle("Stream flow Regime Variables") + 
  #geom_text(data=labeldata, aes(x=x, y=y, label=eq,family="Times", fontface="bold"), parse=FALSE) +
  facet_wrap(~name,scales="free")+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="bold"))





##################for AGU POSTER#########################


DATA2all_agu <-DATA_all_agu
levels(DATA2all_agu$name)[levels(DATA2all_agu$name)==sdnames] <- ranames

p=c(21,22,21,25,24,21,25,24,21,22,21,25,24,21,25,24)
s=c(10,6,6,6,6,3,4,4,10,6,6,6,6,3,4,4)
cbPalette_mod=c("white","white","white","white","white","white","white","white","#9999CC", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


cor_p2 <- ggplot(DATA2all_agu,aes(X,Y)) + geom_point(size=s,shape =p,fill=cbPalette_mod)+geom_abline(intercept = 0)+
  
  xlab("Baseline(1986-2005) Streamflow Regime Variables") + ylab("Future (2076-2095) Streamflow Regime Variables") +
  #ggtitle("Stream flow Regime Variables") + 
  #geom_text(data=labeldata, aes(x=x, y=y, label=eq,family="Times", fontface="bold"), parse=FALSE) +
  facet_wrap(~name,scales="free")+
  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="bold"))





############try diffeent set of image #################
wats=c("A1","A2","B1","B21","B22","C1","C21","C22")
wats1=matrix((rep(watsed,each=16)))
wats2=matrix(rep(wats1,2))
XX=DATA2all_agu$X
YY=matrix(DATA2all_agu$Y)
XX1=matrix(XX[1:128])
value=matrix((rbind(((YY[1:128]/XX1)-1)*100,((YY[129:256]/XX1)-1)*100)))
scenario=c("RCP2.6","RCP8.5")
sce=matrix(rep(scenario,each=128))

dat=data.frame(Z=sce,name=DATA2all_agu$name,value=value,Watershed=wats2)
myData <- dat[-seq(16,256,16), ]
rhg_cols1<- c("green1","blue3" )

setwd("E:\\USU_Research_work\\PAPERS\\Paper_from_TOPNET\\AGU_TOPNET")
##change zero flow event 


print(ggplot(data=myData) +
  geom_bar(aes(x=Watershed, y=value, fill=Z), stat="identity") +scale_fill_manual(values = rhg_cols1)+
  xlab("Watershed") + ylab("Streamflow Regime Change (%) ")+
  facet_wrap(~name,scales="free")+

  
  theme_bw()+theme(legend.position="none")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.5,face="bold")))
ggsave("regime_changes.tiff", width=14, height=12, dpi=500)
dev.off()



########only for legend #############

p=c(24,21,24,22,21, 22,24,21)
s=c(6,6,6,6,6,6,6,6)
#cbPalette_mod=c("white","white","white","white","white","white","white","white","#9999CC", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbPalette <- c("blue", "blue", "gray60", "gray60", "gray60", "red", "red", "red")
df=data.frame(
  x=runif(10),
  y=runif(10),
  x1=runif(10),
  y1=runif(10),
  x3=runif(10),
  y4=runif(10),
  t4=runif(10),
  G4=runif(10),
  
  Watershed=rep(c("A1","A2","B1","B21","B22","C1","C21","C22"),5)
)
library(ggthemes)
print(ggplot(data=df,aes(x=x,y=y,x1=x1,y1=y1,x3=x3,x4=y4,h5=t4,v5=G4,c,fill=factor(Watershed),size=factor(Watershed)))+
  geom_point(aes(shape = factor(Watershed)))+guides(col = guide_legend(ncol = 1, byrow =FALSE))+
  scale_shape_manual(values=p[1:8])+scale_size_manual(values=s[1:8])+ scale_fill_manual(values =cbPalette[1:8])+
facet_wrap(~Watershed,ncol=1)+theme_economist())
ggsave("legned.tiff", width=12, height=12, dpi=500)


###########
p=c(21,22,21,25,24,21,25,24,21,22,21,25,24,21,25,24)
s=c(10,6,6,6,6,3,4,4,10,6,6,6,6,3,4,4)
cbPalette_mod=c("white","white","white","white","white","white","white","white","#9999CC", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbPalette <- c("#9999CC", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
df=data.frame(
  x=runif(10),
  y=runif(10),
  x1=runif(10),
  y1=runif(10),
  x3=runif(10),
  y4=runif(10),
  t4=runif(10),
  G4=runif(10),
  
  facet=rep(c("A1","A2","B1","B21","B22","C1","C21","C22"),5)
)




ggplot(data=df,aes(x=x,y=y,x1=x1,y1=y1,x3=x3,x4=y4,h5=t4,v5=G4,color=factor(facet),fill=factor(facet),size=factor(facet)))+
  geom_point(aes(shape = factor(facet)))+
  scale_shape_manual(values=p[1:8])+scale_size_manual(values=s[1:8])+ scale_fill_manual(values =cbPalette_mod[1:8])+
  facet_wrap(~facet,ncol=1)


cor_p2 <- ggplot(DATA2all,aes(model,model_26_46)) + geom_point(aes(shape=watershed1,colour = watershed1,size=7)) +
geom_abline(intercept = 0)+
  #geom_smooth(method="lm") + 
  xlab("Daymet") + ylab("GCM") +
  #ggtitle("Stream flow Regime Variables") + 
  #geom_text(data=labeldata, aes(x=x, y=y, label=eq,family="Times", fontface="bold"), parse=FALSE) +
  facet_wrap(~name,scales="free")+geom_blank(aes(y = ymin,x=xmin)) + geom_blank(aes(y = ymax,x=xmax))+scale_shape_manual(values = seq(1,8,1))

dfr <- data.frame(
  +     x = rep.int(1:10, 2),
  +     y = runif(20),
  +     g = factor(rep(letters[1:2], each = 10)),
  +     ymin = rep(a1, each = 8), xmin = rep(a1, each = 8),xmax = rep(b1 each = 8),
  +     ymax = rep(b1, each = 8)
  + )
> p <- ggplot(dfr, aes(x, y)) + 
  +     geom_point() +
  +     facet_wrap(~ g, scales = "free_y")+geom_blank(aes(y = ymin,x=xmin)) + geom_blank(aes(y = ymax,x=xmax))
> p
> 
  
  









###########change of stream flow regime variables old ###################

porder=c(2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16)
model_his=dss[3:7,]
model_26_46=dss[8:12,];model_26_76=dss[13:17,];model_85_46=dss[18:22,];model_85_76=dss[23:27,]
change_26_46=((model_26_46/model_his)-1)*100;change_26_461=apply(change_26_46[,-seq(1,136,17)],2,mean);change_26_462=matrix(change_26_461,nrow=16,ncol=8);
change_26_76=((model_26_76/model_his)-1)*100;change_26_761=apply(change_26_76[,-seq(1,136,17)],2,mean);change_26_762=matrix(change_26_761,nrow=16,ncol=8);
change_85_46=((model_85_46/model_his)-1)*100;change_85_461=apply(change_85_46[,-seq(1,136,17)],2,mean);change_85_462=matrix(change_85_461,nrow=16,ncol=8);
change_85_76=((model_85_76/model_his)-1)*100;change_85_761=apply(change_85_76[,-seq(1,136,17)],2,mean);change_85_762=matrix(change_85_761,nrow=16,ncol=8);


change_26_46f=change_26_462[porder,]
change_26_76f=change_26_762[porder,]
change_85_46f=change_85_462[porder,]
change_85_76f=change_85_762[porder,]



watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
r1=matrix(change_26_46f,nrow=128,ncol=1)
r2=matrix(change_85_46f,nrow=128,ncol=1)
r=rbind(r1,r2)


watershed2=matrix((rep(watsed,each=16)))
watershed=rbind(watershed2,watershed2)

variables2=matrix(rep(sdnames,8))
variables=rep(variables2,2)
scena1=matrix(rep(c('RCP2.6'),each=128))
scena2=matrix(rep(c("RCP8.5"),each=128))
scena=rbind(scena1,scena2)

library(ggplot2)

# create fake dataset with additional attributes - sex, sample, and temperature
x <- data.frame(
  values = r,
  scenario = scena,
  sample = variables,
  place = watershed
)

x2<- x
levels(x2$sample)[levels(x2$sample)==sdnames] <- ranames
# compare different sample populations across various temperatures
d=ggplot(x2, aes(x=place, y=values,fill = scenario,colour=scenario)) +
  geom_point(size=4)
d + facet_wrap(~ sample,scales = "free")+geom_hline(yintercept = 0.0,colour="black",size=1)+
 theme_bw()+theme(legend.position="none")+scale_color_manual(values = c("green", "red"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+

xlab("Watersheds") + ylab("Regime change(%)")+ggtitle(" Year 2076-2095 compare to 1986-2005 ")



##for my analyis and writng

a11=matrix(change_26_46f,nrow=16,ncol=8)
a22=matrix(change_85_46f,nrow=16,ncol=8)
a33=matrix(change_26_76f,nrow=16,ncol=8)
a44=matrix(change_85_76f,nrow=16,ncol=8)

all=rbind(cbind(a11,a22),cbind(a33,a44))

write.table(all,"result.xls")
#########seeing box plot##############
# compare different sample populations across various temperatures

change_26_462=(change_26_46[,-seq(1,136,17)])
change_26_762=(change_26_76[,-seq(1,136,17)])
change_85_462=(change_85_46[,-seq(1,136,17)])
change_85_762=(change_85_76[,-seq(1,136,17)])
v1=change_26_462; 
v2=change_85_462;

r1=matrix(v1,nrow=640,ncol=1)
r2=matrix(v2,nrow=640,ncol=1)
r=rbind(r1,r2)


watershed1=matrix((rep(watsed,each=80)))
watershed=rbind(watershed1,watershed1)


variables2=matrix(rep(names,each=5))
variables22=matrix(rep(variables2,8))
variables=rbind(variables22,variables22)

scena1=matrix(rep(c('RCP2.6'),each=640))
scena2=matrix(rep(c("RCP8.5"),each=640))
scena=rbind(scena1,scena2)


xx <- data.frame(
  values = r,
  scenario = scena,
  sample = variables,
  place = watershed
)



ggplot(xx, aes(x = sample, y = values, fill = scenario)) +
  geom_boxplot() +geom_hline(yintercept = 0.0,colour="black",size=1)+ #stat_summary(aes(group=scenario), fun.y=mean, geom="line")+
  facet_wrap(~ place,scales = "free")+theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(name = "Scenario", values = c("green", "red"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  xlab("Season") + ylab("Streamflow Regime Change (%)")+ggtitle(" Year 2046-2065 compare to 1986-2005 ")















####explanation of q7 min

porder=c(2,1,11,10,3,12,8,5,6,7,14,15,9,13,4,16)
model_his=dss[3:7,]
model_26_46=dss[8:12,];model_26_76=dss[13:17,];model_85_46=dss[18:22,];model_85_76=dss[23:27,]
change_26_46=((model_26_46/model_his)-1)*100;change_26_461=apply(change_26_46[,-seq(1,136,17)],2,mean);change_26_462=matrix(change_26_461,nrow=16,ncol=8);
change_26_76=((model_26_76/model_his)-1)*100;change_26_761=apply(change_26_76[,-seq(1,136,17)],2,mean);change_26_762=matrix(change_26_761,nrow=16,ncol=8);
change_85_46=((model_85_46/model_his)-1)*100;change_85_461=apply(change_85_46[,-seq(1,136,17)],2,mean);change_85_462=matrix(change_85_461,nrow=16,ncol=8);
change_85_76=((model_85_76/model_his)-1)*100;change_85_761=apply(change_85_76[,-seq(1,136,17)],2,mean);change_85_762=matrix(change_85_761,nrow=16,ncol=8);






#####plot daymet vs obs
op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(daymet[1,],obs[1,])

###see how changes occur#########

change2646=((mod2646/model)-1)*100
change2676=((mod2676/model)-1)*100
change8546=((mod8546/model)-1)*100
change8576=((mod8576/model)-1)*100


###for A1 and A2 watershed

m=1
op <- par(mfrow = c(2,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(change2646[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2646[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+1],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))



###for B1,B21 and B22 watershed

m=6
op <- par(mfrow = c(3,1),mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(change2646[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2646[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+1],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))




plot(change2646[,m+2],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-30,100))
lines(change8546[,m+2],col='grey',lty=1,lwd=3,type='o')
lines(change2676[,m+2],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+2],col='grey',lty=2,lwd=3,type='o')
lines(seq(1,16,1),rep(0,16),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


m=2
plot(change2646[m,],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=1,lwd=0.5,type="o",ylim=c(-50,100))
lines(change8546[m,],col='grey',lty=1,lwd=3,type='o')
lines(change2676[m,],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-50,100))
lines(change8576[m,],col='grey',lty=2,lwd=3,type='o')
lines(seq(-5,16,1),rep(0,22),lty=3,lwd=3)
legend('topleft',c("Daymet",paste(Model,"without bc",sep=" "),paste(Model,"with bc",sep=" ")),col=c("black","grey","grey"),lty=c(2,1,3),lwd=c(2,2,2))


plot(change2676[,m+1],xlab="Time (days)",ylab="Cumulative Rainfall(mm)",lty=2,lwd=0.5,type="o",ylim=c(-30,70))
lines(change8576[,m+1],col='grey',lty=1,lwd=3,type='o')
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
