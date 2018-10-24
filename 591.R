# load httr package
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(xml2)

#獨立套房
#拿標題連結
urllist <- c()

for(i in c(0:1)){

url <- paste0('https://rent.591.com.tw/index.php?module=search&action=rslist&is_new_list=1&type=1&searchtype=1&region=1&orderType=desc&listview=txt&kind=2&firstRow=',i*40)
doc <- GET(url)
df <- fromJSON(content(doc, "text"))
rent_data <- df[["main"]]
rent_html <- read_html(rent_data)
name <- html_nodes(rent_html, ".address a")
namelink <- html_attr(name, "href")
namelink <- paste0('https://rent.591.com.tw/',namelink)
urllist <- c(urllist,namelink)
}

#拿文章內容
textdf <- data.frame( title="", region="",price="",size="", floor="")
textdf <- textdf[-1,]

for(i in c(1:20)){
  texturl <- urllist[i]
  doc1 <- read_html(texturl)
  title <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[1]')))
  region <- trimws(xml_text(xml_find_all(doc1, '//*[@id="propNav"]/a[4]')))
  price <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/div[1]')))
  size <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[1]')))
  floor <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[2]')))

  tempdf <- data.frame(title=title,region=region,price=price,size=size, floor=floor)
  textdf <- rbind(textdf, tempdf)
}

#數字處理
textdf$price <-  str_replace_all(textdf$price, ",|元/月|租金波動", "")
textdf$price <- as.numeric(textdf$price)
textdf$size <- str_replace_all(textdf$size, "坪數|坪|\\:", "")
textdf$size <- str_trim(textdf$size)
textdf$size <- as.numeric(textdf$size)
textdf$size1 <- textdf$price/textdf$size

write.csv(textdf, 'textdf.csv')

textdf <-read.csv("/Users/ivytien/Downloads/獨立套房.csv")


#頂加
roofselector <- textdf[grepl('頂樓加蓋', textdf$floor), ]
roofselector <- data.frame(roofselector)

#算分區數量
regionnum <- tapply(roofselector$title,roofselector$region,length)
regionnum <- data.frame(regionnum)

write.csv(regionnum, 'regionnum.csv')

#算全體價格
allprice<-mean(roofselector$price)

#算分區價格
regionprice <- tapply(roofselector$price,roofselector$region,mean)
regionprice <- round(regionprice)
regionprice <- data.frame(regionprice)
write.csv(regionprice, 'regionprice.csv')
regionpricesize <- tapply(roofselector$size1,roofselector$region,mean)
regionpricesize <- round(regionpricesize)
regionpricesize <- data.frame(regionpricesize)
write.csv(regionpricesize, 'regionpricesize.csv')


###非頂加
noroofselector <- textdf[!grepl('頂樓加蓋', textdf$floor), ]
noroofselector <- data.frame(noroofselector)

#算全體非頂加價格
noallprice<-mean(noroofselector$price)


###分租套房
#拿標題連結
urllist2 <- c()

for(i in c(0:22)){
  
  url <- paste0('https://rent.591.com.tw/index.php?module=search&action=rslist&is_new_list=1&type=1&searchtype=1&region=1&orderType=desc&listview=txt&kind=3&firstRow=',i*40)
  doc <- GET(url, add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"))
  df <- fromJSON(content(doc, "text"))
  rent_data <- df[["main"]]
  rent_html <- read_html(rent_data)
  name <- html_nodes(rent_html, ".address a")
  namelink <- html_attr(name, "href")
  namelink <- paste0('https://rent.591.com.tw/',namelink)
  urllist2 <- c(urllist2,namelink)
}

#拿文章內容
textdf2 <- data.frame( title="", region="",price="",size="", floor="")
textdf2 <- textdf2[-1,]

for(i in c(1:868)){
  texturl <- urllist2[i]
  doc <- GET(texturl, add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"))
  doc1 <- content(doc, "parsed")
  title <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[1]')))
  region <- trimws(xml_text(xml_find_all(doc1, '//*[@id="propNav"]/a[4]')))
  price <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/div[1]')))
  size <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[1]')))
  floor <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[2]')))
  
  tempdf <- data.frame(title=title,region=region,price=price,size=size, floor=floor)
  textdf2 <- rbind(textdf2, tempdf)
}

#數字處理
textdf2$price <-  str_replace_all(textdf2$price, ",|元/月|租金波動", "")
textdf2$price <- as.numeric(textdf2$price)
textdf2$size <- str_replace_all( textdf2$size, "坪數|坪|\\:", "")
textdf2$size <- str_trim(textdf2$size)
textdf2$size <- as.numeric(textdf2$size)
textdf2$size1 <- textdf2$price/textdf2$size

write.csv(textdf2, 'textdf2.csv')

#頂加
roofselector2 <- textdf2[grepl('頂樓加蓋', textdf2$floor), ]
roofselector2 <- data.frame(roofselector2)

#算分區數量
regionnum2 <- tapply(roofselector2$title,roofselector2$region,length)
regionnum2 <- data.frame(regionnum2)

write.csv(regionnum2, 'regionnum2.csv')

#算全體價格
allprice2<-mean(roofselector2$price)

#算分區價格
regionprice2 <- tapply(roofselector2$price,roofselector2$region,mean)
regionprice2 <- round(regionprice2)
regionprice2 <- data.frame(regionprice2)
write.csv(regionprice2, 'regionprice2.csv')
regionpricesize2 <- tapply(roofselector2$size1,roofselector2$region,mean)
regionpricesize2 <- round(regionpricesize2)
regionpricesize2 <- data.frame(regionpricesize2)
write.csv(regionpricesize2, 'regionpricesize2.csv')

###非頂加
noroofselector2 <- textdf2[!grepl('頂樓加蓋', textdf2$floor), ]
noroofselector2 <- data.frame(noroofselector2)

#算全體非頂加價格
noallprice2<-mean(noroofselector2$price)



#雅房
#拿標題連結
urllist3 <- c()

for(i in c(0:15)){
  
  url <- paste0('https://rent.591.com.tw/index.php?module=search&action=rslist&is_new_list=1&type=1&searchtype=1&region=1&orderType=desc&listview=txt&kind=4&firstRow=',i*40)
  doc <- GET(url, add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"))
  df <- fromJSON(content(doc, "text"))
  rent_data <- df[["main"]]
  rent_html <- read_html(rent_data)
  name <- html_nodes(rent_html, ".address a")
  namelink <- html_attr(name, "href")
  namelink <- paste0('https://rent.591.com.tw/',namelink)
  urllist3 <- c(urllist3,namelink)
}

#拿文章內容
textdf3 <- data.frame( title="", region="",price="",size="", floor="")
textdf3 <- textdf3[-1,]

for(i in c(580:600)){
  texturl <- urllist3[i]
  doc <- GET(texturl, add_headers("user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"))
  doc1 <- content(doc, "parsed")
  title <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[1]')))
  region <- trimws(xml_text(xml_find_all(doc1, '//*[@id="propNav"]/a[4]')))
  price <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/div[1]')))
  size <- trimws(xml_text(xml_find_first(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[1]')))
  floor <- trimws(xml_text(xml_find_all(doc1, '//*[@id="main"]/div[2]/div[2]/div[2]/div[1]/ul/li[2]')))
  
  tempdf <- data.frame(title=title,region=region,price=price,size=size, floor=floor)
  textdf3 <- rbind(textdf3, tempdf)
}

#數字處理
textdf3$price <-  str_replace_all(textdf3$price, ",|元/月|租金波動", "")
textdf3$price <- as.numeric(textdf3$price)
textdf3$size <- str_replace_all( textdf3$size, "坪數|坪|\\:", "")
textdf3$size <- str_trim(textdf3$size)
textdf3$size <- as.numeric(textdf3$size)
textdf3$size1 <- textdf3$price/textdf3$size

write.csv(textdf3, 'textdf3.csv')

#頂加
roofselector3 <- textdf3[grepl('頂樓加蓋', textdf3$floor), ]
roofselector3 <- data.frame(roofselector3)

#算分區數量
regionnum3 <- tapply(roofselector3$title,roofselector3$region,length)
regionnum3 <- data.frame(regionnum3)

write.csv(regionnum3, 'regionnum3.csv')

#算全體價格
allprice3<-mean(roofselector3$price)

#算分區價格
regionprice3 <- tapply(roofselector3$price,roofselector3$region,mean)
regionprice3 <- round(regionprice3)
regionprice3 <- data.frame(regionprice3)
write.csv(regionprice3, 'regionprice3.csv')
regionpricesize3 <- tapply(roofselector3$size1,roofselector3$region,mean)
regionpricesize3 <- round(regionpricesize3)
regionpricesize3 <- data.frame(regionpricesize3)
write.csv(regionpricesize3, 'regionpricesize3.csv')

###非頂加
noroofselector3 <- textdf3[!grepl('頂樓加蓋', textdf3$floor), ]
noroofselector3 <- data.frame(noroofselector3)

#算全體非頂加價格
noallprice3<-mean(noroofselector3$price)



