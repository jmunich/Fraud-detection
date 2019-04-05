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

###
source("Get_old_text.R")

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

ndoc<-length(all_label)
parsesa<-spacy_parse(tot_corpus_clean[1:250], dependency=TRUE)
parsesb<-spacy_parse(tot_corpus_clean[251:ndoc], dependency=TRUE)

parses<-rbind(parsesa, parsesb)
  
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
    set<-parses[which(parses[,1]==i & parses[,2]==j),]
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
varchoice<-c(4,7)

file_gn_jlsp<-read.table("unretracted_jlsp.txt", header=TRUE)
file_fn_jlsp<-read.table("fraudulent_jlsp.txt", header=TRUE)

file_gn_jlsp<-file_gn_jlsp[,varchoice]
file_fn_jlsp<-file_fn_jlsp[,varchoice]

LIWC<-rbind(file_gn_jlsp, file_fn_jlsp)

### Combine

final<-cbind(all_id[,-3], ngram_data[,-1], ns_ngram_data, readability[,-1], my_fcms, LIWC, endata[,3], dependencies_data)
colnames(final)[c(1,2)]<-c("id","fraud")

### Split data into training and testing set

#keep(final, sure=TRUE)
set.seed(1)

final<-na.omit(final)
final<-final[,-(which(colSums(final[,-c(1,2)])==0)+2)]

final[,1]<-c(1:length(final[,1]))
inTraining <- createDataPartition(final$id, p = .6, list = FALSE)
training <- final[ inTraining,-1]
testing  <- final[-inTraining,-1]