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
gen_loc<-"E:/New Folder/Dropbox/Research Master/Internship/Data/Txt/Genuine/*.txt"
gen_data<-readtext(gen_loc)
fraud_loc<-"E:/New Folder/Dropbox/Research Master/Internship/Data/Txt/Fraud/*.txt"
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
gen_corpus<-corpus(gen_data)

fraud_data<-cbind(fraud_id, fraud_data)
fraud_corpus<-corpus(fraud_data)

tot_corpus<-gen_corpus + fraud_corpus

colnames(gen_id)<-c("id","section","fraud")
colnames(fraud_id)<-c("id","section","fraud")
all_id<-rbind(gen_id, fraud_id)
