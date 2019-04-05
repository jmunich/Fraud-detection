###Create corpus
library(caret)
library(readtext)
library(quanteda)
library(spacyr)
library(igraph)
library(text2vec)
library(reshape2)
library(gtools)
library(lexicon)
library(gdata)
##Read the text files (insert correct file location)
gen_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/Genuine/*.txt"
gen_data<-readtext(gen_loc)
fraud_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/Fraud/*.txt"
fraud_data<-readtext(fraud_loc)

##Extract id and section type from document names for matching
gen_identifier<-strsplit(gen_data[,1], split=c("_"))
gen_id<-data.frame()
for(i in 1:length(gen_identifier)){
  gen_id[i,1]<-as.numeric(unlist(strsplit(gen_identifier[[i]][2],split=".txt")))
  gen_id[i,2]<-gen_identifier[[i]][1]
}

fraud_identifier<-strsplit(fraud_data[,1], split=c("_"))
fraud_id<-data.frame()
for(i in 1:length(fraud_identifier)){
  fraud_id[i,1]<-1000*as.numeric(unlist(strsplit(fraud_identifier[[i]][2],split=".txt")))
  fraud_id[i,2]<-fraud_identifier[[i]][1]
}

##Add genuine/fraudulent value
gen_id<-cbind(gen_id, rep(0,length(gen_id[,1])))
colnames(gen_id)<-NULL
fraud_id<-cbind(fraud_id, rep(1,length(fraud_id[,1])))
colnames(fraud_id)<-NULL

##Merge with text data and create corpus
gen_data<-cbind(gen_id, gen_data)
fraud_data<-cbind(fraud_id, fraud_data)

##Collapse sections into full articles
gencor<-matrix(0,nrow=length(unique(as.numeric(gen_data[,1]))), ncol=2)
gencor[,1]<-unique(as.numeric(gen_data[,1]))
for(i in unique(as.numeric(gen_data[,1]))){
  text<-gen_data[which(gen_data==i),5]
  gencor[which(gencor[,1]==i),2]<-paste(text, collapse = ' ')
}

fracor<-matrix(0,nrow=length(unique(as.numeric(fraud_data[,1]))), ncol=2)
fracor[,1]<-unique(as.numeric(fraud_data[,1]))
for(i in unique(as.numeric(fraud_data[,1]))){
  text<-fraud_data[which(fraud_data==i),5]
  fracor[which(fracor[,1]==i),2]<-paste(text, collapse = ' ')
}
colnames(gencor)<-c("doc_id","text")
colnames(fracor)<-c("doc_id","text")

glabel<-matrix(0,nrow=length(gencor[,1]),ncol=1)
flabel<-matrix(1,nrow=length(fracor[,1]),ncol=1)
glabel_dat<-matrix(1,nrow=length(gencor[,1]),ncol=1)
flabel_dat<-matrix(1,nrow=length(fracor[,1]),ncol=1)

##Jeff and Dave's data

JDgen_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/JLSP_Fraud_data/Fraud_data/Unretracted_Full/*.txt"
JDgen_data<-readtext(JDgen_loc)
JDfraud_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/JLSP_Fraud_data/Fraud_data/Fraudulent_Full/*.txt"
JDfraud_data<-readtext(JDfraud_loc)

JDglabel<-matrix(0,nrow=length(JDgen_data[,1]),ncol=1)
JDflabel<-matrix(1,nrow=length(JDfraud_data[,1]),ncol=1)
JDglabel_dat<-matrix(0,nrow=length(JDgen_data[,1]),ncol=1)
JDflabel_dat<-matrix(0,nrow=length(JDfraud_data[,1]),ncol=1)

##Bind data together

all_text<-rbind(gencor,JDgen_data, fracor, JDfraud_data)
all_label<-rbind(glabel,JDglabel, flabel, JDflabel)
all_label_dat<-rbind(glabel_dat,JDglabel_dat, flabel_dat, JDflabel_dat)

write.table(all_text, "all_text_files.txt")
###

tot_corpus<-corpus(as.character(all_text[,2]))


all_id<-cbind(all_text[,1], all_label)
names(all_id)<-c("id","fraud")