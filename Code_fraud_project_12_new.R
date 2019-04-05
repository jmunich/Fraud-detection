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
library(pROC)

source("Get_new_text.R")
###Clean data
##From British to American
toks<-tokens(tot_corpus)
vocabulary<-read.csv("vocab.txt")
toks<-tokens_replace(toks, as.character(vocabulary[,1]), as.character(vocabulary[,2])) 
toks<-tokens_remove(toks, pattern = " ")
toks<-lapply(toks, function(x) tolower(x))

ctoks<-unlist(lapply(toks, function(x) paste(x, sep=" ", collapse=" ")))
tot_corpus_clean<-corpus(ctoks)


###########
### Entities
spacy_initialize(model='en')

docvars(tot_corpus_clean, "text")<-all_id[,1]
identify<-docvars(tot_corpus_clean)
identify<-cbind(identify, rownames(identify))
parses<-spacy_parse(tot_corpus_clean, dependency=TRUE)
entities<-entity_extract(parses, type = "all")
entities<-cbind(entities, mne=rep(0,length(entities[,1])))
entities[,5]<-ifelse(entities$entity_type %in% c("ORG", "QUANTITY", "ORDINAL", "CARDINAL"),1,0)
encount <-aggregate(entities[,5], by=list(entities[,1]), FUN=sum, na.rm=TRUE)

colnames(identify)<-c("id","match")
colnames(encount)<-c("match","score")

endata<-merge(identify, encount, by="match", all.x=TRUE)
endata[which(is.na(endata[,3])==TRUE),3]<-0
endata[,3]<-endata[,3]/ntoken(tot_corpus_clean)

###Get lemma corpus

lemmatized<-c()
counter<-0
for(i in unique(parses[,1])){
  counter<-counter+1
  wordvec<-parses[which(parses[,1]==i),5]
  lemmatized[counter]<-paste(wordvec, sep=" ", collapse=" ")
}

tot_corpus_clean_lemma<-corpus(lemmatized)

### Dependencies

prop_root<-c()
prop_nsubj<-c()
prop_nobj<-c()
slope_root<-c()
slope_nsubj<-c()
slope_nobj<-c()
counteri<-0
for(i in unique(parses[,1])){
    counteri<-counteri+1
    root<-c()
    subject<-c()
    object<-c()
    counterj<-0
  for(j in unique(parses[which(parses[,1]==i),2])){
    counterj<-counterj+1
    set<-NULL
    set<-as.matrix(parses[which(parses[,1]==i & parses[,2]==j),])
    root_id<-set[which(set[,8]=="ROOT"),3]
    nsubj_id<-set[which(set[,8]=="nsubj"),3]
    pobj_id<-set[which(set[,8]=="pobj"),3]
    root[counterj]<-length(which(set[,7]==root_id))/length(set[,1])
    subject[counterj]<-length(which(set[,7]==nsubj_id))/length(set[,1])
    object[counterj]<-length(which(set[,7]==pobj_id))/length(set[,1])
  }
    prop_root[counteri]<-mean(root)
    prop_nsubj[counteri]<-mean(subject)
    prop_nobj[counteri]<-mean(object)
    slope_root[counteri]<-lm(root~c(1:length(root)))$coefficients[2]
    slope_nsubj[counteri]<-lm(subject~c(1:length(subject)))$coefficients[2]
    slope_nobj[counteri]<-lm(object~c(1:length(object)))$coefficients[2]
}

dependencies_data<-cbind(prop_root,prop_nsubj,prop_nobj, slope_root, slope_nsubj, slope_nobj)

###N-grams with stopwords

ngrams<-dfm(tot_corpus_clean_lemma, remove_punct=TRUE, remove_numbers=TRUE, ngrams=2:3)
select<-dfm_trim(ngrams, sparsity=.90)
select1<-dfm_tfidf(select)
ngram_data<-as.data.frame(select1)

###Ngrams nonstop
##NB Get better stopwords

source("get_sw.R")

nostop_tot_corp<-tokens(tot_corpus_clean_lemma)
nostop_tot_corp<-tokens_remove(nostop_tot_corp, pattern=c(sws, "et","al", "p","c"))

ns_ngrams<-dfm(nostop_tot_corp, remove_punct=TRUE, remove_numbers=TRUE, ngrams=1:3)
ns_select<-dfm_trim(ns_ngrams, sparsity=.90)
ns_select1<-dfm_tfidf(ns_select)
ns_ngram_data<-as.data.frame(ns_select1)

dup<-which(colnames(ns_ngram_data) %in% colnames(ngram_data))
ns_ngram_data<-ns_ngram_data[,-dup]

###Readability
toks<-tokens(tot_corpus)
vocabulary<-read.csv("vocab.txt")
toks<-tokens_replace(toks, as.character(vocabulary[,1]), as.character(vocabulary[,2])) 
toks<-tokens_remove(toks, pattern = " ")
ctoksup<-unlist(lapply(toks, function(x) paste(x, sep=" ", collapse=" ")))
tot_corpus_cleanup<-corpus(ctoksup)

readability<-textstat_readability(tot_corpus_cleanup, "Flesch")
names(readability)<-c("id","Flesch")

### Semantic network: here, I create a 
### boolean co-occurence network for every individual document. 
### The networks consist of the 30 most frequent tokens in all aggregated documents.


my_toks<-tokens(tot_corpus_clean_lemma, remove_punct = TRUE, remove_numbers = TRUE)
my_toks<-tokens_remove(my_toks, c(stopwords('en'),"et","al", "p", "c"))
my_dfm<-dfm(my_toks, tolower=TRUE)

# Get wordlist
source("words.R")

# Total probablities of occurrence
my_dfm_p<-dfm_weight(my_dfm, scheme="prop")
props<-dfm_select(my_dfm_p, allw)
tprops<-as.data.frame(props)[,-1]

# Quantites of word occurrences 
quants<-dfm_select(my_dfm, allw)
tquant<-as.data.frame(quants)[,-1]

# Prepare a dataframe for values
edgelist<-permutations(length(allw), r=2, allw, repeats.allowed = TRUE)
edgelist<-paste(edgelist[,1],edgelist[,2], sep = "_")
my_fcms<-data.frame(matrix(0,ncol=length(edgelist), nrow=length(my_toks)))
colnames(my_fcms)<-edgelist

for(h in 1:length(my_toks)){
  cmat<-fcm(paste(my_toks[h], sep=" ", collapse=" "), context="window", window=10, count="weighted")
  pcmat<-as.matrix(fcm_select(cmat, allw))
  namemat<-colnames(pcmat)
  
  if(length(pcmat)>0){
    for(i in 1:length(pcmat[,1])){
      for(j in 1:length(pcmat[,1])){
        pcmat[i,j]<-(pcmat[i,j]/tquant[h,namemat[j]])/tprops[h,namemat[i]]
      }
    }
    
    el<-melt(as.matrix(pcmat))
    edges<-paste(el[,1],el[,2], sep = "_")
    my_fcms[i, edges]<-el[,3]
  }
}

my_fcms<-my_fcms[,-which(colSums(my_fcms)==0)]


### Add sentiment

file_gn<-read.table("unretracted_new.txt", header=TRUE)
file_fn<-read.table("fraudulent_new.txt", header=TRUE)

file_gn<-file_gn[,c(4,7)]
file_fn<-file_fn[,c(4,7)]

LIWC<-rbind(file_gn,file_fn)

### Transform from long to wide
names(all_id)[2]<-"section_of_article"
final_data<-list(as.matrix(all_id[,3],ncol=1), ngram_data[,-1], ns_ngram_data, as.matrix(readability[,-1],ncol=1), my_fcms, LIWC, as.matrix(endata[,3],ncol=1), dependencies_data)
feature_names<-c("fraud", "ngrams", "ns_ngrams", "readability", "hedging", "liwc", "entities", "dependencies")
feat_namelist<-list()
final_list<-list()
for(i in 1:length(final_data)){
set<-cbind(all_id[,c(1,2)],final_data[[i]])
features<-reshape(set, idvar = "id", timevar = "section_of_article", direction = "wide")
feat_namelist[[i]]<-colnames(final_data[[i]])
final_list[[i]]<-features[,-1]
}

widid<-reshape(all_id[,c(1,2)], idvar = "id", timevar = "section_of_article", direction = "wide")

for(i in 1:length(widid[,1])){
  if(widid[i,1]>900){widid[i,1]<-widid[i,1]/1000}
}  




sortedmems<-rep(max(memberdata[,2])+1,length(matchids))
for(i in 1:length(matchids)){
  if(matchids[i]>900){matchids[i]<-matchids[i]/1000}
  if(length(memberdata[which(memberdata[,1]==matchids[i]),2])>0){
    sortedmems[i]<-memberdata[which(memberdata[,1]==matchids[i]),2]
  }
}



wide_feat_namelist<-list()
for(i in 1:length(feat_namelist)){
  namevec<-c()
  secs<-c(".d",".i",".m",".r")
  for(k in 1:4){
  l<-length(feat_namelist[[i]])*(k-1)
    for(j in 1:length(feat_namelist[[i]])){
    namevec[j+l]<-paste(feat_namelist[[i]][j],secs[k],sep="")
  }
  }
  wide_feat_namelist[[i]]<-namevec
}



final_list[[1]][is.na(final_list[[1]])]<-1000
score<-list()
for(i in 1:length(final_list[[1]][,1])){
  
  score[[i]]<-unique(unlist(final_list[[1]][i,]))
}

fraudvec<-c()
for(i in 1:length(score)){
  if(length(score[[i]])==1){
    fraudvec[i]<-score[[i]][1]
  }
  if(length(score[[i]])==2){
    fraudvec[i]<-min(score[[i]])
  }
  if(length(score[[i]])>2){
    print("Warning, something went teribly wrong!!!!")
  }
}
  
final_list[[1]]<-as.matrix(fraudvec, ncol=1)
names(final_list)<-feature_names
names(wide_feat_namelist)<-feature_names
for(i in 1:length(final_list)){
  names(final_list[[i]])<-wide_feat_namelist[[i]]
}

names(final_list[[1]])<-"fraud"
names(final_list[[4]])<-c("flesch.d","flesch.i","flesch.m","flesch.r")
names(final_list[[7]])<-c("entities.d","entities.i","entities.m","entities.r")

final_frame<-do.call(cbind, final_list)
exclude<-complete.cases(final_frame)

for(i in 1:length(final_list)){
  final_list[[i]]<-final_list[[i]][-which(exclude==FALSE),]
  if(i>1){
    if(length(which(colSums(final_list[[i]])==0)==TRUE)>0)
      final_list[[i]]<-final_list[[i]][,-which(colSums(final_list[[i]])==0)]
  }
}

widid<-widid[-which(exclude==FALSE),]

source("Get_membership.R")
matchids<-reshape(all_id[,-3], idvar = "id", timevar = "section_of_article", direction = "wide")
matchids<-matchids[,1]
usemember<-sortedmems[-which(exclude==FALSE)]

set.seed(1)
selects<-c()
a<-1
b<-1
while(a<.6 | a>.65 | b<.494 | b>.506){
  selects<-sample(unique(usemember), sample(1:(length(unique(usemember)))))
  intraining<-which(usemember%in%selects)
  a<-length(intraining)/length(usemember)
  b<-sum(as.numeric(unlist(final_list[1])[intraining]))/length(unlist(final_list[1])[intraining])
}
inTraining_sep<-intraining
