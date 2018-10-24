dir.list2 = list.files("D:/CenterCase/fm/output" , full.name = TRUE)

library(Hmisc)
library(quanteda)
library(tm)
library(NLP)
library(openNLP)

find_bi = matrix(ncol = 2,nrow = 4)
for (i in 1:length(dir.list2)) {
  file2 = dir.list2[i]
  s2 =readLines(file2,encoding="ASCII")
  find_bi[i,1] = s2[1]
  for(i in 1:length(dir.list2)){
    pus2 <- Corpus(VectorSource(find_bi[i,1]))
    ## Split into words:
    w <- strsplit(pus2[[1]]$content, " ", fixed = TRUE)[[1L]]
    ## Word bi-grams:
    ngrams(w, 2L)
    ## Word bi-grams pasted together:
    bigrams = vapply(ngrams(w, 2L), paste, "", collapse = "_")
    find_bi[i,2] = paste(bigrams , collapse = ",")
  }
}
write(find_bi[,2], "D:/CenterCase/fm/findbi/bigrams.txt" , sep = "\n")

data <- read.table("D:/CenterCase/fm/findbi/bigrams.txt", sep="")
bi_list <- strsplit(as.character(data$V1),",", fixed = TRUE)
write(as.character(bi_list), "D:/CenterCase/fm/findbi/bigramslist.txt" , sep = ",")