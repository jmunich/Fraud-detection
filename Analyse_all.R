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
#spacy_initialize(python_executable = 'D:/Python/WinPython-PyGaze-0.6.0/conda/New folder/pythonw.exe') 
#source('./get_entity_count.R')
#ner<-get_entity_count(tot_corpus_clean$metadata$source, tot_corpus_clean$documents)

###N-grams with stopwords

ngrams<-dfm(tot_corpus_clean, remove_punct=TRUE, remove_numbers=TRUE, ngrams=2:3)
select<-dfm_trim(ngrams, sparsity=.95)
select1<-dfm_tfidf(select)
ngram_data<-as.data.frame(select1)

###Ngrams nonstop
##NB Get better stopwords

source("get_sw.R")

nostop_tot_corp<-tokens(tot_corpus_clean)
nostop_tot_corp<-tokens_remove(nostop_tot_corp, pattern=c(sws, "et","al", "p","c"))

ns_ngrams<-dfm(nostop_tot_corp, remove_punct=TRUE, remove_numbers=TRUE, ngrams=1:3)
ns_select<-dfm_trim(ns_ngrams, sparsity=.95)
ns_select1<-dfm_tfidf(ns_select)
ns_ngram_data<-as.data.frame(ns_select1)

dup<-which(colnames(ns_ngram_data) %in% colnames(ngram_data))

ns_ngram_data<-ns_ngram_data[,-dup]

keep(c(all_id, tot_corpus_clean, ngram_data, ns_ngram_data), sure=TRUE)


###Readability
toks<-tokens(tot_corpus)
vocabulary<-read.csv("vocab.txt")
toks<-tokens_replace(toks, as.character(vocabulary[,1]), as.character(vocabulary[,2])) 
toks<-tokens_remove(toks, pattern = " ")
ctoksup<-unlist(lapply(toks, function(x) paste(x, sep=" ", collapse=" ")))
tot_corpus_cleanup<-corpus(ctoksup)
readability<-textstat_readability(tot_corpus_cleanup, "Flesch")
names(readability)<-c("id","Flesch")

keep(c(all_id, tot_corpus_clean, ngram_data, ns_ngram_data, readability), sure=TRUE)

### Semantic network: here, I create a 
### boolean co-occurence network for every individual document. 
### The networks consist of the 30 most frequent tokens in all aggregated documents.


my_toks<-tokens(tot_corpus_clean, remove_punct = TRUE, remove_numbers = TRUE)
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

keep(c(all_id, tot_corpus_clean, ngram_data, ns_ngram_data, readability, my_fcms), sure=TRUE)


### Add sentiment
#4 analytic 11 function 33 posemo 34 negemo 43 cogproc 44 insight 45 cause 48 certain 66 focuspresent 

#file_gn<-read.table("unretracted_new.txt", header=TRUE)
#file_fn<-read.table("fraudulent_new.txt", header=TRUE)

#file_gn<-file_gn[,c(4,7)]
#file_fn<-file_fn[,c(4,7)]

#LIWC<-rbind(file_gn,file_fn)


### Transform from long to wide

final_data<-cbind(all_id[,1], as.factor(all_label), ngram_data[,-1], ns_ngram_data, readability[,-1], my_fcms)

### Split data into training and testing set

#keep(final, sure=TRUE)
set.seed(2)

final<-na.omit(final_data)
final<-final[,(-which(colSums(final[,-c(1,2)])==0)+2)]
colnames(final)[c(1,2)]<-c("id","fraud")
inTraining <- createDataPartition(final$id, p = .6, list = FALSE)
training <- final[ inTraining,-1]
testing  <- final[-inTraining,-1]

### Train svm
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
svm <- train(as.factor(fraud)~., data=training, method="svmLinear2", trcontrol=fitControl, scale=FALSE)

trellis.par.set(caretTheme())
plot(svm) 

pred<-predict(svm, newdata=testing[,-1])
confusionMatrix(testing[,1], pred)