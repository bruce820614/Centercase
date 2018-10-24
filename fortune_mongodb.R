dir.list4 = list.files("D:/CenterCase/magazine_pass" , full.name = TRUE)
dir.list5 = list.files("D:/CenterCase/magazine_pass" , full.name = FALSE)
filename = gsub(".txt","",dir.list5)

library(Hmisc)
library(quanteda)
library(tm)
library(NLP)
library(openNLP)
library(rjson)
library(RMongo)

fortune_mag = matrix(ncol = 2,nrow = 630)
for(i in 1:length(filename)){
  fortune_mag[i,1] = filename[i]
}
for(i in 1:length(dir.list4)){
  file4 = dir.list4[i]
  s4 =readLines(file4,encoding="ASCII")
  fortune_mag[i,2] = paste(s4,collapse = "")
  #fortune_mag[i,1] = s4[1]
  #pus4 <- Corpus(VectorSource(s4))
  #pus4 <- corpus(pus4)
  #json <- toJSON(pus4)
  #output <- fromJSON(json)
}
colnames(fortune_mag) = c("filename","content")
write.csv(fortune_mag,"D:/CenterCase/fortune_article.csv")

library(xlsx)
write.xlsx(fortune_mag,"D:/CenterCase/fortune_article.xlsx", sheetName = "Sheet1")
mongo <- mongoDbConnect("fortune_article", "localhost", port=27017)
dbShowCollections(mongo)
dbInsertDocument(mongo, "fortunearticle", doc) 
b=mongo.bson.from.df(df)


