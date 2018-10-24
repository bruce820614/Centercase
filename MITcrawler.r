library(XML)
library(RCurl)
library(rvest)
library(magrittr)
library(xml2)
page.source_test <- read_html("http://eds.b.ebscohost.com/ehost/results?sid=cd5cc592-f818-4009-a867-ad85acc4de9a%40sessionmgr106&vid=3&hid=113&bquery=JN+%22MIT+Technology+Review%22+AND+DT+20160101&bdata=JmRiPWJ0aCZsYW5nPXpoLXR3JnR5cGU9MCZzaXRlPWVob3N0LWxpdmU%3d")
version.block_test <- html_nodes(page.source_test, ".color-p4")
title = version.block_test %>% html_attr('title')
url = version.block_test %>% html_attr('href')
title = gsub("[[:punct:]]",replacement = "",title)
for(k in 1:length(url)){
  content_article = read_html(url[k]) %>% html_nodes(".body-paragraph") %>% html_text()
  Sys.sleep(runif(1,2,4))
  a1 = "D:/CenterCase/MIT2016/"
  b1 = paste(a1,title[k], sep = "")
  c1 = ".txt"
  d1 = paste(b1,c1, sep = "")
  write(content_article, file = d1)
}