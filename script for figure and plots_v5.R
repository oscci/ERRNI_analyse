# Script to produce information for Tables 1-3 and figs 2-3 (ERRNI data)
# Added to Github on 24 Jun 2020
###############################################################################
#Paul Thompson December2016
#Edited by Dorothy Bishop 21/12/16
#Added script to create Table 4 22/12/16
# Included scripts for sequential analysis of proportions 22/12/16

library(XLConnect)
library(reshape2)
library(doBy)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(car)
thisperson='dorothybishop' #change to pthompson if need be
mymac=1 #set to 0 if not using a mac

myfolder=paste("C:\\Users\\",thisperson,"\\Dropbox\\ERRNIanalysis2016\\ERRNI raw DATA\\final_041116",sep="")
mybackfile=paste("C:\\Users\\",thisperson,"\\Dropbox\\ERRNIanalysis2016\\ERRNI outputs\\ID_backgds.xlsx",sep="")
myoutputs=paste("C:\\Users\\",thisperson,"\\Dropbox\\ERRNIanalysis2016\\ERRNI outputs",sep="")
mysep="\\"
if (mymac==1){
  thisperson='dorothybishop'
  mysep="/"
  myfolder=paste("/Users/",thisperson,"/Dropbox/ERRNIanalysis2016/ERRNI raw DATA/final_041116",sep="")
  mybackfile=paste("/Users/",thisperson,"/Dropbox/ERRNIanalysis2016/ERRNI outputs/ID_backgds.xlsx",sep="")
  myoutputs=paste("/Users/",thisperson,"/Dropbox/ERRNIanalysis2016/ERRNI outputs",sep="")
}

location_final<-myfolder
myfinallist<-list.files(location_final) #NB the final raw data folder should ONLY contain included raw data files
Nsub=length(myfinallist)

setwd(myoutputs) #default is to write any newly created files to ERRNI outputs


#extract age bands
ageB <- substring(myfinallist,1,2)

#######################################

background_wb<-loadWorkbook(mybackfile)
backgroundy=data.frame(readWorksheet(background_wb,sheet=1))

#######################################
#check files match
match(backgroundy$ID,substring(toupper(myfinallist),1,7))


#Before running code below select which story to run for "Fish or "beach"
id_ly<-data.frame(IDold=myfinallist, ID=substring(toupper(myfinallist),1,7))

id_er<-merge(id_ly,backgroundy,by="ID", all.x=T)

id_er<-id_er[is.na(id_er$story)==FALSE,]

################################################################################
# Table 1
################################################################################

# create a blank table in R to populate with values
tab1<-matrix(NA,14,6)
tab1<-as.data.frame(tab1)

#add table names (these are updated later)
names(tab1)<-c("Age_Band", "N", "Beach_fish","Male_Female","TROG","No._recall")

#populate column for age bands and N.
tab1$Age_Band<-names(table(id_er$ageband))
tab1$N<-table(id_er$ageband)

#Run a loop to fill values for beach|fish counts, Male|Female counts, and TROG mean(SD). 
for(i in 1:14){
  
  tab1$Beach_fish[i]<-paste(round(table(id_er$story[id_er$ageband==names(table(id_er$ageband))[i]])/sum(table(id_er$story[id_er$ageband==names(table(id_er$ageband))[i]]))*100),collapse = "|")
  #
  tab1$Male_Female[i]<-paste(round(table(id_er$sex[id_er$ageband==names(table(id_er$ageband))[i]])[2:1]/sum(table(id_er$sex[id_er$ageband==names(table(id_er$ageband))[i]])[2:1])*100),collapse = "|")
  
  tab1$TROG[i]<-paste(round(mean(id_er$TROG[id_er$ageband==names(table(id_er$ageband))[i]],na.rm=T),1)," (",round(sd(id_er$TROG[id_er$ageband==names(table(id_er$ageband))[i]],na.rm=T),1),")",sep = "")
}
########################################

#THis function then creates a list of the participants that did not produce a narrative
recaller<-matrix(NA,ncol=2,nrow=Nsub)
recaller<-as.data.frame(recaller)
names(recaller)<-c("ID","N_recall")
for(i in 1:Nsub){
  myID=paste(location_final,myfinallist[i],sep=mysep)
  
  mytranscript=loadWorkbook(myID)
  mytrans=data.frame(readWorksheet(mytranscript,sheet=1))
  mytrans$N.word=as.numeric(mytrans$N.word)#needed because in some files this is non-numeric!!
  myagg=aggregate(mytrans$N.word~mytrans$TELL.RECALL, data=mytrans, FUN=function(x) c(mean=mean(x), count=length(x)))
  norecall=0
  aggrow=nrow(myagg)
  if (aggrow<2){norecall=1} #exclude cases with zero recall
  numbit=myagg[,2]#this is a matrix, not a number!
  if(sum(numbit[,2])<10) {norecall=1} #  exclude cases with fewer than total of 10 utterances
  if(mean(numbit[,1])<1.5){norecall=1} # exclude cases with MLU < 1.5 (only labelling)
  recaller[i,1] <-substring(myfinallist[i],1,7)
  recaller[i,2]<-norecall
}


#The ID columns are changed to upper case font so that they can be merged correctly.
id_er$ID<-as.vector(unlist(lapply(id_er$ID, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})))

recaller$ID<-as.vector(unlist(lapply(recaller$ID, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})))

#Merge data together, so that a summary column of percentage of "no narrative" participants by age can be added to Table 1.

id_er2<-merge(id_er,recaller,by="ID")

for(i in 1:14){
  
  tab1$No._recall[i]<-sum(id_er2$N_recall[id_er2$ageband==names(table(id_er2$ageband))[i]],na.rm=T)
}

#############################################
#Reorder table 1 so that younger age bands appear first.

tab1<-tab1[c(9:14,1:8),]
names(tab1)<-c("Age_Band", "N", "%Beach_fish","%Male_Female","TROG ss","N no narrative")
#Write csv to file with data for table.
write.csv(tab1,"table_1_errni.csv",row.names=F)

################################################################################
# Pirate plots Fig 1
# NB loaded current version of yarrr in development via Github
################################################################################

# Load data table

summary_dat_wb<-loadWorkbook("summary_allNov14_2016.xlsx")
summary_dat=data.frame(readWorksheet(summary_dat_wb,sheet=1))

#Load R package to produce pirate plots.
#install.packages("yarrr")
library("yarrr")




summary_dat$ageband<-factor(summary_dat$ageband,levels=c( "4A", "4B", "4C", "4D", "5A", "5B","06", "07", "08", "09", "10", "12", "14", "17"))

allexclude=which(summary_dat$allexclude==1)
summary_dat=summary_dat[-allexclude,]
tiff(file = "Figure 1_col.tiff", width = 3200, height = 3200, units = "px", res = 500)
#par(xpd=T) #turned this off to avoid having distribution lines extending beyond axes

pcol<-c("skyblue","dodgerblue","dodgerblue3","dodgerblue4","green","green4","pink","yellow","red","purple","orange","deeppink4","darkmagenta","brown")

pirateplot(formula = avgmlu ~ ageband,
           theme=0,
           data = summary_dat,
           xlab = "Age Band",
           ylab = "MLUw",
           main = "",
           point.col=pcol,point.bg = pcol,
           inf.f.o = .7,
           avg.line.o = 1,
           bar.f.o = .01,
           bean.b.o = .1,
           point.o = .2,
           point.pch = 16,
           back.col = gray(.99))
# NB commands for this version of yarrr needed changing:
#hdi.o to inf.f.o, line.o to avg.line.o , and bar.o to bar.f.o
dev.off()

################################################################################
# Pirate plots Fig 2 - clause density
################################################################################
tiff(file = "Figure 2_col.tiff", width = 3200, height = 3200, units = "px", res = 500)
#par(xpd=T) #turned this off to avoid having distribution lines extending beyond axes
pirateplot(formula = meandensity_all ~ ageband,
           data = summary_dat,
           xlab = "Age Band",
           ylab = "Clause Density",
           main = "",
           #pal = "black",
           inf.f.o = .7,
           avg.line.o = 1,
           bar.f.o = .01,
           bean.b.o = .1,
           point.o = .2,
           point.pch = 16,
           back.col = gray(.99))
dev.off()
################################################################################
# Regression on age with mluw and clause density as predictors.
################################################################################
library(stats)
#fit linear models
summary_dat$ageband <- factor(summary_dat$ageband, levels = c("4A","4B","4C","4D","5A","5B","06","07","08","09","10","12","14","17"))
summary_dat$agebandn<-as.numeric(summary_dat$ageband)


lm1<-lm(agebandn~ avgmlu ,data=summary_dat)
lm1a<-lm(agebandn~ avgmlu + meandensity_all,data=summary_dat)

# equivalent to lm1<-lm(agelevel~ avgmlu + meandensity_all,data=summary_dat)
summary(lm1)
summary(lm1a)
###################################

#plot fit statistics
windows()
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm1a)
#all looks good! see http://data.library.virginia.edu/diagnostic-plots/

#compare with model when actual age used
# lm2<-lm(ageyr~ avgmlu + meandensity_all,data=summary_dat)
# windows()
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
# plot(lm2)

#correlation between variables
cor(summary_dat[,c("avgmlu","meandensity_all","agebandn","ageyr")],use="na.or.complete")

###########################################################################
# mean clause density different for TELL_RECALL
###########################################################################

summary_dat_NA<-summary_dat[!is.na(summary_dat$meandensity_R),]

data.long <- melt(summary_dat_NA[,c("ID","agebandn","story","meandensity_T","meandensity_R")], id = c("ID","story","agebandn"), 
                  measure = c("meandensity_T","meandensity_R"), variable.name="Tell_recall")  


names(data.long)[5]<-"meandensity"


###REpeated measures ancova

data.aov2 <- aov(meandensity ~ agebandn * Tell_recall + Error(ID), data = data.long)
summary(data.aov2)



levels(data.long$Tell_recall)<-c("Tell", "Recall")

##########################################################################
pirateplot(formula = meandensity ~ Tell_recall,
           data = data.long,
           xlab = "Tell|Recall",
           ylab = "Mean Clause Density",
           main = "",
           pal = "black",
           inf.f.o = .7,
           avg.line.o = 1,
           bar.f.o = .01,
           bean.b.o = .1,
           point.o = .2,
           point.pch = 16,
           back.col = gray(.99))

###########################################################################
# mean clause density different for story narrated
###########################################################################


###Repeated measures ancova
library(car)
summary_dat_NA$story<-recode(summary_dat_NA$story,"0='Beach';1='Fish'")
summary_dat_NA$story <- as.factor(summary_dat_NA$story)
data.aov3 <- aov(meandensity_all ~ agebandn * story + Error(ID), data = summary_dat_NA)
summary(data.aov3)


##########################################################################
pirateplot(formula = meandensity_all ~ story,
           data = summary_dat_NA,
           xlab = "Story",
           ylab = "Mean Clause Density",
           main = "",
           pal = "black",
           inf.f.o = .7,
           avg.line.o = 1,
           bar.f.o = .01,
           bean.b.o = .1,
           point.o = .2,
           point.pch = 16,
           back.col = gray(.99))

##########################################################################
#          Appendix 1. Mean frequency of clause type usage at each age band
##########################################################################
attach(summary_dat)
Nnew=nrow(summary_dat)
clausedat=data.frame(matrix(rep(NA,Nnew*16),nrow=Nnew))
colnames(clausedat)=c('ID','ageband','x','m','m.','n','r','a','cf','cn','cr','t','d','a-','a+','total')
clausedat[,1:2]=summary_dat[,1:2]
clausedat[,3:13]<-c(T_x+R_x,T_m+R_m,T_m.+R_m.,T_n+R_n,T_r+R_r,T_a+R_a+T_a.+R_a.,T_cf+R_cf,T_cn+R_cn,T_cr+R_cr,T_t+R_t,T_d+R_d)
clausedat[,14]=T_a.+R_a.
clausedat[,15]=T_a+R_a
detach(summary_dat)
attach(clausedat)
clausedat[,16]=rowSums(clausedat[,3:13])
tab4a=summaryBy(x+m+m.+n+r+a+cf+cn+cr+t+d~ageband,data=clausedat,FUN=mean)
clausedat2=clausedat
clausedat2[,3:13]=100*clausedat2[,3:13]/clausedat2[,16] #compute percentages
tab4b=summaryBy(x+m+m.+n+r+a+cf+cn+cr+t+d~ageband,data=clausedat2,FUN=mean)
tabtemp=round(tab4a[,2:12],digits=1)
tabtemp2=round(tab4b[,2:12],digits=1)
tabtemp3=data.frame(matrix(rep(NA,28*12),nrow=28))
myrow=nrow(tabtemp2)
mycol=ncol(tabtemp2)
for (i in 1:myrow){
  thisrow=(i-1)*2+1
  tabtemp3[thisrow,1]=levels(ageband)[i]
  tabtemp3[i*2,1]=""
  for (j in 1:mycol){
    
    tabtemp3[thisrow,j+1]=tabtemp[i,j]
    tabtemp3[i*2,j+1]=paste('[',tabtemp2[i,j],"]",sep="")
    
  }
}
colnames(tabtemp3)=c("Age band",'x','m','m+','n','r','a','cf','cn','cr','t','d')
#Write csv to file with data for table.
#write.csv(tabtemp3,"Appendix1_errni.csv",row.names=F)
##########################################################
# % in each age band with at least one instance of clause type
##########################################################
includecols=c(2,5:13)
clauseuse=clausedat[,includecols]#ignore x and m for this analysis
for(i in 2:10){
  for (j in 1:nrow(clauseuse)){
    if(clauseuse[j,i]>0){clauseuse[j,i]=1}
  }
}
tab5=summaryBy(m.+n+r+a+cf+cn+cr+t+d~ageband,data=clauseuse,FUN=mean)
rownames(tab5)=levels(ageband)
rownames(tab5)[14]='17+'
colnames(tab5)=c('Age band','coord','n','r','a','cf','cn','cr','t','d')
tab5[,2:10]=round(tab5[,2:10],digits=2)
#Write csv to file with data for table.
#write.csv(tab5,"newtab4_errni.csv",row.names=F)
##########################################################
#  simplified table with just nonclause/main/coord/subord
selectcols=c(2,3,4,5,6,16)
shortclause=clausedat[,selectcols]
colnames(shortclause)=c('ageband','none','main','coord','subord','total')
allsub=clausedat[,6]+clausedat[,7]+clausedat[,8]+clausedat[,9]+clausedat[,10]+clausedat[11]+clausedat[,12]+clausedat[,13]
shortclause[,5]=allsub
shortclause2=shortclause
attach(shortclause2)
shortclause2[,2:5]=100*shortclause[,2:5]/shortclause[,6] #compute percentages
tab4c=summaryBy(none+main+coord+subord~ageband,data=shortclause,FUN=mean)
tab4d=summaryBy(none+main+coord+subord~ageband,data=shortclause2,FUN=mean)
tabtemp=round(tab4c[,2:5],digits=1)
tabtemp2=round(tab4d[,2:5],digits=1)
tabtemp4=data.frame(matrix(rep(NA,28*5),nrow=28))
myrow=nrow(tabtemp2)
mycol=ncol(tabtemp2)
for (i in 1:myrow){
  thisrow=(i-1)*2+1
  tabtemp4[thisrow,1]=levels(ageband)[i]
  tabtemp4[i*2,1]=""
  for (j in 1:mycol){
    
    tabtemp4[thisrow,j+1]=tabtemp[i,j]
    tabtemp4[i*2,j+1]=paste('[',tabtemp2[i,j],"]",sep="")
    
  }
}
colnames(tabtemp4)=c("Age band",'No verb','Main','Coordinate','Subordinate')
detach(shortclause2)
###################################################################
#stacked bar chart with basic clause types
###################################################################
tiff(file = "Figure 3.tiff", width = 5000, height = 2200, units = "px", res = 500)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
col <- c(rep(c("blue","skyblue","red","red3"),14))
mybardata=t(tabtemp2) #transpose
colnames(mybardata)=levels(ageband)
colnames(mybardata)[14]='17+'
barplot(as.matrix(mybardata),xlab="Age band", ylab="Mean percent",legend=colnames(tabtemp4)[2:5],args.legend = list(x="bottomright",inset=c(-0.2,0)))

dev.off()

###############################################################################
# function to do proportions
###############################################################################
myproportions <- function(mynumer,mydenom,mytype,mydescrip,mydf,wb1){
  px=mynumer/mydenom
  myvalid=which(mydenom>0) #used to get the N for this contrast
  myinclude=rep(0,length(mydenom))
  myinclude[myvalid]=1 #1 means have data in denominator
  tabcount=aggregate( myinclude~ agebandn, data=mydf, FUN=sum,na.rm=TRUE)#counts how many cases have relevant data
  #(i.e. one or more utterance in denominator)
  
  tabpn=aggregate( mynumer ~ agebandn, data=mydf, FUN=mean,na.rm=TRUE) #mean N instances of clausetype in numerator
  tabpd=aggregate( mydenom ~ agebandn, data=mydf, FUN=mean,na.rm=TRUE) #mean all utterances counted in denominator
  
  tabpx=aggregate( px ~ agebandn, data=mydf, FUN=mean,na.rm=TRUE) #key table with proportions by age 
  
  tabpxsd=aggregate( px ~ agebandn, data=mydf, FUN=sd,na.rm=TRUE)
  tabpxmin=aggregate( px ~agebandn, data=mydf, FUN=min,na.rm=TRUE)
  tabpxmax=aggregate( px ~ agebandn, data=mydf, FUN=max,na.rm=TRUE)
  tableall=cbind(tabpn[1],tabcount[2],tabpn[2],tabpd[2],tabpx[2],tabpxsd[2],tabpxmin[2],tabpxmax[2])
  
  tableall=round(tableall,digits=3)
  colnames(tableall)=c("agebandn", "Count","Meanraw","Uttraw","Mean_p","SD_p","Min_p","Max_p")
  mydescrip=paste('Column E: ',mydescrip)
  createSheet(wb1,mytype)
  writeWorksheet(wb1,tableall,mytype)
  writeWorksheet(wb1, mydescrip, sheet = mytype, startRow = 17, startCol = 1,rownames=FALSE)
  # mydescrip explains what the comparison is: I cannot get it to write this except as wrapped text, so needs reformattng in xls
  
  saveWorkbook(wb1)
  return(tabpx)
}


myagereg <- function(mynumer,mydenom,mytype,mydf,wb1,agebandn){
  
  #NB regression of proportion on ageband (rather than ageyr) 
  #Because of how agebands are constructed, this gives more linear relation and accounts for more variance
  #than if ageyr is used
  
  myforlog=mydenom-mynumer
  elog=log((mynumer+0.5)/(myforlog+0.5))
  #mywts= 1/(mynumer+.5)+1/(myforlog+.5)#old weights
  
  lm1 = lm(elog ~ agebandn ,data=mydf) 
  
  #print(length(lm1$residuals))
  #print(length(agebandn))
  
  lm1A=lm(abs(lm1$residuals)~agebandn,data=mydf) #regression using residuals from initial model
  mywts<-1/(lm1A$fitted.values^2) #New model weights for final regression using inverse of squared fitted values
  
  lm1new<-lm(elog~agebandn,data=mydf,weights=mywts)
  #plot(lm1new)
  
  regsum=summary(lm1new)
  myterm=c('Intercept','Ageband')
  myconf=as.data.frame(confint(lm1new,method="boot"))
  
  myconf=cbind(myterm,myconf)
  #convert reg outputs to data frame
  mycoef <- as.data.frame(summary(lm1new)$coef)
  mycoef=cbind(myterm,mycoef)
  mymodelinfo= c(r.squared = summary(lm1new)$r.squared, F = summary(lm1new)$fstatistic[1], df.res = summary(lm1new)$df[2])
  mymodelinfo=as.data.frame(t(mymodelinfo))
  writeWorksheet(wb1,  mycoef, sheet = mytype, startRow = 20, startCol = 1)
  writeWorksheet(wb1,  mymodelinfo, sheet = mytype, startRow = 23, startCol = 1)
  mylabel="Confidence intervals"
  writeWorksheet(wb1, mylabel,sheet = mytype, startRow = 25, startCol = 1)
  writeWorksheet(wb1, myconf, sheet = mytype, startRow = 26, startCol = 1)
  
  saveWorkbook(wb1)
  plot(lm1new)
  plot(agebandn,elog,ylab="e-log(y)", xlab="Age band",pch=20)
  abline(lm1new,col="red",lwd=2)
  return(mycoef)
}
#######################################
# Main analysis of proportions starts here
#######################################
detach()
myxl=paste("proportions5.xlsx",sep="") #output to be written to excel workbook
wb1 = loadWorkbook(myxl,create=T) #create the workbook to write to (in working directory)
#to see where workbook is, use getwd()
attach(clausedat) #data frame with relevant numbers, summed for Tell and Recall
clausedat$agebandn=as.numeric(ageband)
#######################################
# Preliminary analysis: check how many non-clauses, i.e. x
#Here we just specify the elements to go into the analysis, which is done via functions tabp and myagereg (defined above)
mynumer=x #numerator for proportion
mydenom=total #denominator for proportion
myforlog=mydenom-mynumer #difference is used in regression on proportions
mytype='x'
mydescrip='x as proportion of all utterances'
mydf=clausedat#dataframplot(tabp)#optional - to see proportion by age
tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)
#---------------------------------------------------------------------
#First analysis: proportion of main clauses (m) 
#---------------------------------------------------------------------
mynumer=m
mydenom=total-x
mytype='m'
mydescrip='monoclausal(m+t) as proportion of all utterances'
tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)
#---------------------------------------------------------------------
#2nd analysis: proportion of complex that are coordinate
#---------------------------------------------------------------------
#NB R converts plus sign to dot
mydenom=total-m-x #compute by taking out m and x from totals
mynumer=m.
mytype='m+'
mydescrip='m+ as proportion of all complex (ignores m and x)'
tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)

#For all remaining analyses, do age regression on those age 6 upwards only
#Too few complex clauses in younger children to give accurate estimates
mycases=which(agebandn<6) #include age 6 and above only
clausedat$totcomplex=n+cn+r+a+cf+cr+d+t
mycases2=which(totcomplex<5) # also need to exclude those with less than 5 complex clauses
mycases3=unique(sort(c(mycases,mycases2)))
clauseolder=clausedat[-mycases3,]
detach(clausedat)
attach(clauseolder)

#---------------------------------------------------------------------
#3rd analysis: proportion of subordinate that are nonfinite
#---------------------------------------------------------------------
mydf=clauseolder
mynumer=n + cn+t# can include t here ('try' 'start' 'stop' etc)
              # currently treated with m, but could treat as a type of n
mydenom=totcomplex

mydescrip='n + cn +t as proportion of all subordinate: age band 6+ only in regression'
mytype='nonfin'
tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)
#---------------------------------------------------------------------
#4th analysis: proportion of finite clauses that are adverbials
#---------------------------------------------------------------------
# 
mydf=clauseolder
mydenom=totcomplex-n-cn #
mynumer=a
mytype='adverbials'
mydescrip='adverbials as proportion of all finite: age band 6+ only in regression'

tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)

#---------------------------------------------------------------------
#5th analysis: proportion of adverbial clauses that are a-
#---------------------------------------------------------------------

mydenom=a #
mynumer=clauseolder$'a-'
mytype='a-'
mydescrip='a- as proportion of all adverbials: age band 6+ only in regression'
tabp=myproportions(mynumer,mydenom,mytype,mydescrip,mydf,wb1)
plot(tabp)
myagereg(mynumer,mydenom,mytype,mydf,wb1,agebandn)
detach(clauseolder)

