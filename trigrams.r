dir.list3 = list.files("D:/CenterCase/fm/autonomous vehicles" , full.name = TRUE)

library(Hmisc)
library(quanteda)
library(tm)
library(NLP)
library(openNLP)

find_tri = matrix(ncol = 2,nrow = 4)
for (i in 1:length(dir.list3)) {
  file0 = dir.list3[i]
  s3 =readLines(file0,encoding="ASCII")
  find_tri[i,1] = s3[1]
  for(i in 1:length(dir.list3)){
    pus3 <- Corpus(VectorSource(find_tri[i,1]))
    pus3 <- tm_map(pus3, removeWords, capitalize(c(stopwords("english"))))
    pus3 <- tm_map(pus3, removePunctuation)
    pus3 <- tm_map(pus3, removeNumbers)
    a3 = unlist(strsplit(pus3[[1]]$content,"[[:space:]]+"))
    ngrams(a3, 3L)
    trigrams = vapply(ngrams(a3, 3L), paste, "", collapse = "_")
    b3 = grep("[A-Z].*_[a-z].*_[A-Z].*[^ ]",trigrams)
    find_tri[i,2] = paste(trigrams[b3],collapse = ",")
  }
}
write(find_tri[,2], "D:/CenterCase/fm/findtri/trigrams.txt" , sep = "\n")

data1 <- read.table("D:/CenterCase/fm/findtri/trigrams.txt", sep="")
tri_list <- strsplit(as.character(data1$V1),",", fixed = TRUE)
write(as.character(tri_list), "D:/CenterCase/fm/findtri/trigramslist.txt" , sep = ",")
