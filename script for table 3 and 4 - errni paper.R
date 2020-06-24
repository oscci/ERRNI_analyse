# script for table 3 and 4 (ERRNI paper)

################################################################################

#First, load R package to allow loading of excel data, recoding data
library(XLConnect)
require(car)

#Gather filenames of all excel data files for individuals
location_final<-"C:\\Users\\pthompson\\Dropbox\\ERRNIanalysis2016\\ERRNI raw DATA\\final_041116\\"
myfinallist<-list.files(location_final)[1:354]

#extract age bands for each excel file
ageB <- substring(myfinallist,1,2)


#Gather all information about clauses from each excel file.
clauser<-matrix(NA,ncol=2)
clauser<-as.data.frame(clauser)
names(clauser)<-c("adding","sheet")
for(i in 1:length(myfinallist)){
  
  myID=paste(location_final,myfinallist[i],sep="")
  if(file.exists(myID)){
    mytranscript=loadWorkbook(myID)
    mytrans=data.frame(readWorksheet(mytranscript,sheet=1))
    
    mytrans<-mytrans[!mytrans$clause %in% c("a+","cr+","cn+","n+","r+","cf+","t+"),]
    
    mytrans$clause<-recode(mytrans$clause,"'a-'='a';'cr-'='cr';'cf-'='cf';'u'=NA;'d-'='d';'n-'='n'")
    
    adding <- levels(factor(mytrans$clause))
    sheet <- rep(myfinallist[i],length(adding))
    clause1<-cbind(adding, as.character(sheet))
    colnames(clause1)<-c("adding","sheet")
    clauser<-rbind(clauser,clause1)
  }
  
}
#clauser1<-as.data.frame(clauser)
table(clauser$adding)

#Determine names of all available clauses in data
clauses<-names(table(clauser$adding))

#reorder names of clauses
clauses<-clauses[c(11,6,7,8,9,1,2,3,4,10,5)]

#set number of clauses
N.clause<-dim(table(clauser$adding))


#######################################
#Next function creates a count of all clauses from each participan by Tell and Recall.
#Function also counts an instance of each clause used by each participant.

clausecount<-matrix(NA,ncol=N.clause+2,nrow=length(myfinallist))
clausecount_T<-clausecount_R<-clausecount_all<-as.data.frame(clausecount)
clauseinst_T<-clauseinst_R<-clauseinst_all<-as.data.frame(clausecount)
for(i in 1:length(myfinallist)){
  
  myID=paste(location_final,myfinallist[i],sep="")
  if(file.exists(myID)){
    mytranscript=loadWorkbook(myID)
    mytrans=data.frame(readWorksheet(mytranscript,sheet=1))
    
    #mytrans$clause <- factor(mytrans$clause)
    clausecount_T[i,1]<-clausecount_R[i,1]<-clausecount_all[i,1]<-substring(myfinallist[i],1,7)
    clausecount_T[i,2]<-clausecount_R[i,2]<-clausecount_all[i,2]<-substring(myfinallist[i],1,2)
    #
    clauseinst_T[i,1]<-clauseinst_R[i,1]<-clauseinst_all[i,1]<-substring(myfinallist[i],1,7)
    clauseinst_T[i,2]<-clauseinst_R[i,2]<-clauseinst_all[i,2]<-substring(myfinallist[i],1,2)
      
    mytrans<-mytrans[!mytrans$clause %in% c("a+","cr+","cn+","n+","r+","cf+","t+"),]
    
    mytrans$clause<-recode(mytrans$clause,"'a-'='a';'cr-'='cr';'cf-'='cf';'u'=NA;'d-'='d';'n-'='n'")
    
    for(j in 1:N.clause)
    {
      clauseinst_T[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause[mytrans$TELL.RECALL=="T"])),nomatch=0)>0,1,0)
      clauseinst_R[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause[mytrans$TELL.RECALL=="R"])),nomatch=0)>0,1,0)
      clauseinst_all[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause)),nomatch=0)>0,1,0)
      #
      clausecount_T[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause[mytrans$TELL.RECALL=="T"])),nomatch=0)>0,table(mytrans$clause[mytrans$TELL.RECALL=="T"])[names(table(mytrans$clause[mytrans$TELL.RECALL=="T"]))==clauses[j]][[1]],0)
      clausecount_R[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause[mytrans$TELL.RECALL=="R"])),nomatch=0)>0,table(mytrans$clause[mytrans$TELL.RECALL=="R"])[names(table(mytrans$clause[mytrans$TELL.RECALL=="R"]))==clauses[j]][[1]],0)
      clausecount_all[i,j+2]<-ifelse(match(clauses[j],names(table(mytrans$clause)),nomatch=0)>0,table(mytrans$clause)[names(table(mytrans$clause))==clauses[j]][[1]],0)
      
    }
    
  }
  
}

#Rename column headings for new summary tables of clauses counts and clause instances.
colnames(clausecount_T)<-c("ID","AGE_band",paste(clauses,"_T"))
colnames(clausecount_R)<-c("ID","AGE_band",paste(clauses,"_R"))
colnames(clausecount_all)<-c("ID","AGE_band",clauses)
#################################################################
colnames(clauseinst_T)<-c("ID","AGE_band",paste(clauses,"_T"))
colnames(clauseinst_R)<-c("ID","AGE_band",paste(clauses,"_R"))
colnames(clauseinst_all)<-c("ID","AGE_band",clauses)

#Function recodes the ID column so that all ID characters are upper case.

clausecount_T$AGE_band<-clauseinst_T$AGE_band<-as.vector(unlist(lapply(clausecount_T$AGE_band, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})))
########
clausecount_R$AGE_band<-clauseinst_R$AGE_band<-as.vector(unlist(lapply(clausecount_R$AGE_band, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})))
########
clausecount_all$AGE_band<-clauseinst_all$AGE_band<-as.vector(unlist(lapply(clausecount_all$AGE_band, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})))
########

#calculates the means of clauses for each participant on their TELL responses.

clausemean_T<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clausecount_T$AGE_band)))
clausemean_T<-as.data.frame(clausemean_T)
names(clausemean_T)<-c("AGE_band",clauses)

for(i in 1:length(unique(clausecount_T$AGE_band)))
{
  sub<-clausecount_T[clausecount_T$AGE_band==unique(clausecount_T$AGE_band)[i],]
  clausemean_T[i,2:(N.clause+1)]<-colMeans(sub[,-c(1,2)])
  clausemean_T[i,1]<-unique(clausecount_T$AGE_band)[i]
}

#calculates the means of clauses for each participant on their RECALL responses.

clausemean_R<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clausecount_R$AGE_band)))
clausemean_R<-as.data.frame(clausemean_R)
names(clausemean_R)<-c("AGE_band",clauses)

for(i in 1:length(unique(clausecount_R$AGE_band)))
{
  sub<-clausecount_R[clausecount_R$AGE_band==unique(clausecount_R$AGE_band)[i],]
  clausemean_R[i,2:(N.clause+1)]<-colMeans(sub[,-c(1,2)])
  clausemean_R[i,1]<-unique(clausecount_R$AGE_band)[i]
}


#calculates the means of clauses for each participant on their combined TELL and RECALL responses.

clausemean_all<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clausecount_all$AGE_band)))
clausemean_all<-as.data.frame(clausemean_all)
names(clausemean_all)<-c("AGE_band",clauses)

for(i in 1:length(unique(clausecount_all$AGE_band)))
{
  sub<-clausecount_all[clausecount_all$AGE_band==unique(clausecount_all$AGE_band)[i],]
  clausemean_all[i,2:(N.clause+1)]<-colMeans(sub[,-c(1,2)])
  clausemean_all[i,1]<-unique(clausecount_all$AGE_band)[i]
}

#Reorders the summary tables so that age bands are in descending order.

clausemean_R<-clausemean_R[c(9:14,1:8),]
clausemean_T<-clausemean_T[c(9:14,1:8),]
clausemean_all<-clausemean_all[c(9:14,1:8),]

################################################################################################

#Calculates percentage of participants producing at least one utterance (TELL)

clauseprop_T<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clauseinst_T$AGE_band)))
clauseprop_T<-as.data.frame(clauseprop_T)
names(clauseprop_T)<-c("AGE_band",clauses)

for(i in 1:length(unique(clauseinst_T$AGE_band)))
{
  sub<-clauseinst_T[clauseinst_T$AGE_band==unique(clauseinst_T$AGE_band)[i],]
  clauseprop_T[i,2:(N.clause+1)]<-round((colSums(sub[,-c(1,2)])/dim(sub)[1])*100,2)
  clauseprop_T[i,1]<-unique(clauseinst_T$AGE_band)[i]
}

#Calculates percentage of participants producing at least one utterance (RECALL)

clauseprop_R<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clauseinst_R$AGE_band)))
clauseprop_R<-as.data.frame(clauseprop_R)
names(clauseprop_R)<-c("AGE_band",clauses)

for(i in 1:length(unique(clauseinst_R$AGE_band)))
{
  sub<-clauseinst_R[clauseinst_R$AGE_band==unique(clauseinst_R$AGE_band)[i],]
  clauseprop_R[i,2:(N.clause+1)]<-round((colSums(sub[,-c(1,2)])/dim(sub)[1])*100,2)
  clauseprop_R[i,1]<-unique(clauseinst_R$AGE_band)[i]
}


#Calculates percentage of participant producing at least one utterance (ALL)

clauseprop_all<-matrix(NA,ncol=N.clause+1,nrow=length(unique(clauseinst_all$AGE_band)))
clauseprop_all<-as.data.frame(clauseprop_all)
names(clauseprop_all)<-c("AGE_band",clauses)

for(i in 1:length(unique(clauseinst_all$AGE_band)))
{
  sub<-clauseinst_all[clauseinst_all$AGE_band==unique(clauseinst_all$AGE_band)[i],]
  clauseprop_all[i,2:(N.clause+1)]<-round((colSums(sub[,-c(1,2)])/dim(sub)[1])*100,2)
  clauseprop_all[i,1]<-unique(clauseinst_all$AGE_band)[i]
}

#Reorders the summary tables so that age bands are in descending order.

clauseprop_R<-clauseprop_R[c(9:14,1:8),]
clauseprop_T<-clauseprop_T[c(9:14,1:8),]
clauseprop_all<-clauseprop_all[c(9:14,1:8),]


################################################################################################

#### Write data tables to .csv file

#Clause means

write.csv(clausemean_T,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clausemean_tell_table.csv",row.names = FALSE)
write.csv(clausemean_R,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clausemean_recall_table.csv",row.names = FALSE)
write.csv(clausemean_all,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clausemean_table.csv",row.names = FALSE)

################################################################################

# Clause proportions

write.csv(clauseprop_T,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clauseinstance_tell_table.csv",row.names = FALSE)
write.csv(clauseprop_R,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clauseinstance_recall_table.csv",row.names = FALSE)
write.csv(clauseprop_all,"C:/Users/pthompson/Dropbox/ERRNIanalysis2016/ERRNI outputs/tables/clauseinstance_table.csv",row.names = FALSE)

################################################################################

