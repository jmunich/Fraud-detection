metadata<-read.csv("Data_log.csv")
APA<-metadata[,3]
APA<-as.character(APA)
sAPA<-strsplit(APA,split="\\(")
authors<-c()
for(i in 1:length(sAPA)){
  authors[i]<-sAPA[[i]][1]
}
unique_authors<-unlist(strsplit(authors, fixed=TRUE, split=".,"))
unique_authors<-gsub("...", "", unique_authors, fixed = TRUE)
unique_authors<-gsub("&", "", unique_authors)
unique_authors<-gsub(" ", "", unique_authors)

Reps<-names(which(table(unique_authors)>1))

aulist<-strsplit(authors, fixed=TRUE, split=".,")

for(i in 1:length(aulist)){
  aulist[[i]]<-gsub("...", "", aulist[[i]], fixed = TRUE)
  aulist[[i]]<-gsub("&", "", aulist[[i]], fixed = TRUE)
  aulist[[i]]<-gsub(" ", "", aulist[[i]], fixed = TRUE)
}

multiple<-list()
for(i in 1:length(aulist)){
multiple[[i]]<-aulist[[i]][which(aulist[[i]]%in%Reps)]
}

shares<-matrix(0,nrow=length(aulist), ncol=length(aulist))

for(i in 1:length(aulist)){
  for(j in 1:length(aulist)){
    if(sum(aulist[[i]]%in%aulist[[j]])>1){
      shares[i,j]<-1
    }
  }
}

gg<-igraph::graph_from_adjacency_matrix(shares, mode = "directed")

mem<-components(gg)
group<-mem$membership
memberdata<-cbind(id=metadata$ID,group=group)

summary(mem$csize)