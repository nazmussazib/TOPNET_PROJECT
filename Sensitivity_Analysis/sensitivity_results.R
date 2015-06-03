dir="E:\\USU_Research_work\\SENSITIVITY_PROJECT\\Results"
setwd(dir)

rain_files=list.files(path=dir,pattern="_precipitation_sensitivity.txt")
rain_temp_files=list.files(path=dir,pattern="*temperature_precipitation_sensitivity.txt")


############# only temperature sensitivity analysis#############

temp_files =list.files(path=dir,pattern="*temperature_sensitivity.txt")

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


watsed=c("A1","A2","B1","B21","B22","C1","C21","C22")
watershed1=matrix((rep(watsed,each=144)))

a=seq(0.6,1.4,0.1)
b=seq(-2,6,1)

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



#############temperature seasonal_sensitivty test ###############

temp_season_files=list.files(path=dir,pattern="*_temp_")

temp_season_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(temp_season_files[1:4])) {
  te=temp_read(temp_season_files[i])
  temp_season_sens=cbind(temp_season_sens,te)
}
temp_season_sens=temp_season_sens[,-1]

b=seq(-2,6,1)
var_temp=matrix(rep(b,each=16))
deltaT=matrix(rep(var_temp,4))
var_change_temp=matrix(temp_season_sens,nrow=(dim(temp_season_sens)[1])*(dim(temp_season_sens)[2]),ncol=1)
ses=c("s1","s2","s3","s4")
ses11=matrix((rep(ses,each=144)))
variable_s=matrix(rep(names,36))
temp_s_all <- data.frame(
  PercentChange=deltaT,
  changeofPrecp=var_change_temp,
  var=variable_s,
  season=ses11
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






##############Precipitation Sensitivity################


rain_read=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,5]-1)*100
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  sg=matrix(sg,nrow=16,ncol=9)
}

rain1_files=rain_files[seq(1,16,2)]
rain_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain1_files)) {
  pe=rain_read(rain1_files[i])
  rain_sens=cbind(rain_sens,pe)
}
rain_sens=rain_sens[,-1]

var_rain=matrix(rep(a,each=16))
deltaP=matrix(rep(var_rain,8))
var_change_rain=matrix(rain_sens,nrow=(dim(rain_sens)[1])*(dim(rain_sens)[2]),ncol=1)


rain_all <- data.frame(
  PercentChange=deltaP,
  changeofPrecp=var_change_rain,
  var=variable,
  place=watershed1 
)

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




#####for seasonal sensitiviy test##################

rain_season_files=list.files(path=dir,pattern="*_precp")

rain_season_sens=matrix(NA,nrow=16,ncol=1)
for (i in 1:length(rain_season_files)) {
  pe=rain_read(rain_season_files[i])
  rain_season_sens=cbind(rain_season_sens,pe)
}
rain_season_sens=rain_season_sens[,-1]

var_rain=matrix(rep(a,each=16))
deltaP=matrix(rep(var_rain,4))
var_change_rain=matrix(rain_season_sens,nrow=(dim(rain_season_sens)[1])*(dim(rain_season_sens)[2]),ncol=1)
ses=c("s1","s2","s3","s4")
ses11=matrix((rep(ses,each=144)))
variable_s=matrix(rep(names,36))
rains_all <- data.frame(
  PercentChange=deltaP,
  changeofPrecp=var_change_rain,
  var=variable_s,
  season=ses11
)

# compare different sample populations across various temperatures
library(ggplot2)
p1 <- ggplot(rains_all, aes(x=PercentChange, y=changeofPrecp, colour=season, group=season)) +geom_hline(yintercept=0)+geom_vline(xintercept=1)+
  geom_line(size=1.2) +theme_bw()+facet_wrap(~ var,scales ="free")+xlab("") +
  scale_color_manual(values = c("grey80", "black","blue","cyan","brown", "darkgreen","gold","red"))+xlim(0.5,1.5)+
  
  
  ylab("Percent Change of LFE")+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))






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
data=expand.grid(x=aw,y=b)
data=rbind(data,data,data,data,data,data,data,data)
v=2
data$zvar <- temp_rain_sens[v,1:dim(temp_rain_sens)[2]]
data$wats=rep(watsed,each=81)
p6=ggplot(data, aes(x, y, fill = zvar)) + geom_raster(hjust = 0, vjust = 0)+
  scale_fill_gradient2(low="blue", high="red", na.value="black", name="") +
  geom_hline(yintercept=0)+geom_vline(xintercept=1)+theme_bw()+facet_wrap(~ wats,scales = "free")+  ylab("Temperature Change")+
  xlab("Percent Change in Precipitation")+theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))




























multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






library(ggplot2)
library(grid)
library(gridExtra)
a <- qplot(a,rain_sens[v,1:9], main = "a")
b <- qplot(b,temp_sens[v,1:9], main = "b")
c <- qplot(df,temp_rain_sens[v,1:81], main = "c")





par(mar = c(3,5,2.5,2))
image(df$x,df$y, t(temp_rain_sens[v,1:81]), col=ColorRamp)
, xlab="",
      ylab="", axes=FALSE, zlim=c(min,max))
if( !is.null(title) ){
  title(main=title)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1:2))  # key is to define vplayout
print(b, vp = vplayout(2, 1))
print(c, vp = vplayout(2, 2))





rain_sens=sapply(rain_files,rain_file)
rain_sens=matrix(rain_sens,nrow=dim(rain_sens)[2]*dim(rain_sens)[1],ncol=1)
var_rain=matrix(seq(0.6,1.4,0.1))
deltaP=matrix(rep(matrix(rep(var_rain,each=16)),4))






















watsed=c("A1","A2","B1","C1")
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
variable=matrix(rep(names,36))
var_temp=matrix(seq(-2,6,1))

deltaT=matrix(rep(matrix(rep(var_temp,each=16)),4))

temp_all <- data.frame(
  values =temp_sens,
  scenario= deltaT,
  vari = variable,
  place = watershed1
)

# compare different sample populations across various temperatures
library(ggplot2)
ggplot(temp_all, aes(x =scenario, y = values,colour=place,group=place)) +
  geom_line()+geom_hline(yintercept = 0.0,colour="black",size=1)+
  facet_wrap(~vari,scales = "free")+theme_bw()
  
  




####precipitation sensitivity analysis#############

rain_file=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,5]-1)*100
  sg=matrix(sg,nrow=16*9,ncol=1)
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  return(sg)
}


rain_sens=sapply(rain_files,rain_file)
rain_sens=matrix(rain_sens,nrow=dim(rain_sens)[2]*dim(rain_sens)[1],ncol=1)
var_rain=matrix(seq(0.6,1.4,0.1))
deltaP=matrix(rep(matrix(rep(var_rain,each=16)),4))

rain_all <- data.frame(
  values =rain_sens,
  scenario= deltaP,
  vari = variable,
  place = watershed1
)

# compare different sample populations across various temperatures
library(ggplot2)
ggplot(rain_all, aes(x =scenario, y = values,colour=place,group=place)) +
  geom_line()+geom_hline(yintercept = 0.0,colour="black",size=1)+
  facet_wrap(~vari,scales = "free")+theme_bw()



#####temperatue and precipitaion sensitivity analysis######

rain_temp_file=function(file){
  sg=read.table(file)
  sg=data.matrix(sg)
  sg=(sg/sg[,5]-1)*100
  sg=matrix(sg,nrow=16*9,ncol=1)
  sg[is.na(sg)]=0
  sg[is.infinite(sg)]=0
  return(sg)
}





rain_sens=sapply(rain_files,rain_file)
rain_sens=matrix(rain_sens,nrow=dim(rain_sens)[2]*dim(rain_sens)[1],ncol=1)
var_rain=matrix(seq(0.6,1.4,0.1))
deltaP=matrix(rep(matrix(rep(var_rain,each=16)),4))

rain_all <- data.frame(
  values =rain_sens,
  scenario= deltaP,
  vari = variable,
  place = watershed1
)

# compare different sample populations across various temperatures
library(ggplot2)
ggplot(rain_all, aes(x =scenario, y = values,colour=place,group=place)) +
  geom_line()+geom_hline(yintercept = 0.0,colour="black",size=1)+
  facet_wrap(~vari,scales = "free")+theme_bw()







  theme(legend.position="none")+
  scale_fill_manual(name = "Scenario", values = c("green", "red"))+
  theme(strip.text.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  #xlab("Season") + ylab("Streamflow Change (%)")+ggtitle(" Year 2046-2065 compare to 1986-2005 ")
  
  xlab("Season") + ylab(expression(paste(bold("Temperature Change(")^bold("0"),bold("C)"),sep="")))+ggtitle(" Year 2046-2065 compare to 1986-2005 ")

scale_x_discrete(breaks=c("Ann", "s1", "s2", "s3", "s4"), labels=c("Ann", 'Winter', 'Spring', 'Summer', 'Fall'))+
  