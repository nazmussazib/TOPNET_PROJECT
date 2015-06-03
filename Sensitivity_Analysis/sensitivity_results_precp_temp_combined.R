dir="E:\\USU_Research_work\\SENSITIVITY_PROJECT\\Results\\precp_temp_sensitivity"
setwd(dir)

rain_temp_files=list.files(path=dir,pattern="*temperature_precipitation_sensitivity.txt")
watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed1=matrix((rep(watsed,each=144)))
col_name=c("BFI","Slp_BFI","Pval_BFI",
           "CV",
           "CoV_yr","Slp_CoV","Pval_CoV",
           "Qmean",
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
           "Peaktime","H1",
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
# variable=matrix(rep(names,72))
# var_temp=matrix(rep(b,each=16))
# deltaT=matrix(rep(var_temp,8))
# var_change_temp=matrix(temp_sens,nrow=(dim(temp_sens)[1])*(dim(temp_sens)[2]),ncol=1)


############Precpitation and Temperature Sensitivity test##############


temp_rain_read=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,23]-1)*100 ##as 23 is rain multification factor=1, temp change=0
  
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  sg=matrix(sg,nrow=16,ncol=81)
}


temp_rain_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain_temp_files)) {
  petm=temp_rain_read(rain_temp_files[i])
  temp_rain_sens=cbind(temp_rain_sens,petm)
}
temp_rain_sens=temp_rain_sens[,-1]


a=seq(0.6,1.4,0.1)
b=seq(-2,6,1)
df=expand.grid(x=a,y=b)
#####plotting qmean #####
var_order=c(4,8,15,16,23,24,25,29,35,37,40,43,46,49,52,55)
names=col_names[var_order]

aw=c(-40,-30,-20,-10,0,10,20,30,40)
a=seq(0.6,1.4,0.1)
b=seq(-2,6,1)
data=expand.grid(x=a,y=b)
data=rbind(data,data,data,data,data,data,data,data)
library(ggplot2)
v=3
data$zvar <- temp_rain_sens[v,1:dim(temp_rain_sens)[2]]
write.table(data,'BFF.txt',row.names=FALSE,col.names=FALSE)

##Need to change the varibale to scale so that plot are adjusted to get rid of out lier

##for v=1cv: var>50=50:v=2,qmean= var>150=150,BFF>150=150,P>30=30


fg=data$zvar
fg[fg>50]=50
fg[fg<(-50)]=-50
data$LFE=fg

data$wats=rep(watsed,each=81)
br <- c(-100,-75,-50,-25,0,25,50,75,100,125,150)
br= c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
br=seq(-150,25,25)
#br=seq(-25,250,20)
prtemp_plt=ggplot(data, aes(x, y, fill =LFE)) + geom_raster(hjust = 0, vjust = 0)+
  scale_fill_gradient2(low="blue", mid="white",high="red",limits=c(-50,50),midpoint=0,breaks=br)+
 # ,limits=c(-70,50), midpoint =0,breaks=br) +
  #scale_fill_gradientn(colours=jet.colors(7), legend_param=list(colorbar=T, colorbar_nbin=100)+
  geom_hline(yintercept=0)+geom_vline(xintercept=1)+theme_bw()+facet_wrap(~ wats,scales = "free")+  ylab("Temperature Change")+
  xlab("Percent Change in Precipitation")+theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+theme(legend.position=c(.85, .15),legend.title = element_text(colour="black", size=12, face="bold"),legend.text = element_text(colour="black", size = 12, face = "bold"))



# use custom gradient (here jet colormap)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
ggplot(melt(volcano), aes(x=X1, y=X2, fill=value)) + geom_tile() +
  scale_fill_gradientn(colours=jet.colors(7), legend_param=list(colorbar=T, colorbar_nbin=100))













x1=data$x
y1=data$y

dd=cbind(x1,y1)
dd=matrix(dd,nrow=648,ncol=2)
id=which(dd[,1]==20 & dd[,2]==5)
#66 147 228 309 390 471 552 633
vv=data$zvar
vf1=vv[id]

########Need some test for explanation#########
w=seq(1,648,81)
x=data[]
re=x[,3]
re=matrix(re,9,9)




####seasonality of temperature and precipitaion test#############
rain_temp_sn_files=list.files(path=dir,pattern="*A1_precp_temp_")

temp_rain_seasn_sen=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain_temp_sn_files)) {
  petm_sn=temp_rain_read(rain_temp_sn_files[i])
  temp_rain_seasn_sen=cbind(temp_rain_seasn_sen,petm_sn)
}
temp_rain_seasn_sen=temp_rain_seasn_sen[,-1]


aw=c(-40,-30,-20,-10,0,10,20,30,40)
a=seq(0.6,1.4,0.1)
b=seq(-2,6,1)
data=expand.grid(x=a,y=b)
data=rbind(data,data,data,data)
v=3
data$zvar <- temp_rain_seasn_sen[v,1:dim(temp_rain_seasn_sen)[2]]
write.table(data[,1:3],'A1_BFF_test.txt',col.names=FALSE,row.names=FALSE)
season=c("s1","s2","s3","s4")
data$season=rep(season,each=81)
p6=ggplot(data, aes(x, y, fill = zvar)) + geom_raster(hjust = 0, vjust = 0)+
  scale_fill_gradient2(low="blue", high="red", na.value="black", name="") +
  geom_hline(yintercept=0)+geom_vline(xintercept=1)+theme_bw()+facet_wrap(~ season,scales = "free")+  ylab("Temperature Change")+
  xlab("Percent Change in Precipitation")+theme(strip.text.x = element_text(size=14, face="bold"),
                                                axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
                                                axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
                                                axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
                                                axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))















































