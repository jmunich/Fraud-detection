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

##Jeff and Dave's data

JDgen_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/JLSP_Fraud_data/Fraud_data/Unretracted_Full/*.txt"
JDgen_data<-readtext(JDgen_loc)
JDfraud_loc<-"D:/Dropbox/Research Master/Internship/Data/Retractions/Data_final/JLSP_Fraud_data/Fraud_data/Fraudulent_Full/*.txt"
JDfraud_data<-readtext(JDfraud_loc)

JDglabel<-matrix(0,nrow=length(JDgen_data[,1]),ncol=1)
JDflabel<-matrix(1,nrow=length(JDfraud_data[,1]),ncol=1)

##Bind data together

all_text<-rbind(JDgen_data, JDfraud_data)
all_label<-rbind(JDglabel, JDflabel)
###

tot_corpus<-corpus(as.character(all_text[,2]))

all_id<-cbind(all_text[,1], all_label)
names(all_id)<-c("id","fraud")