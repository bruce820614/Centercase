library(XML)
library(RCurl)
library(rvest)
library(magrittr)
num = c(1:13)
target_url = paste("https://www.wired.com/?s=autonomous+vehicles&page=", num, sep = "")
for (i in 1:length(target_url)){
  page.source <- read_html(target_url[i])
  version.block <- html_nodes(page.source, ".border-micro")
  url = version.block[2] %>% html_nodes('li') %>% html_nodes('a') %>% html_attr('href')
  content_title = read_html(target_url[i]) %>% html_nodes(".clamp-5") %>% html_text()
  content_title = gsub("[[:punct:]]",replacement = "",content_title)
  for(j in 1:length(url)){
    content_article = read_html(url[j]) %>% html_nodes("p") %>% html_text()
    Sys.sleep(runif(1,2,4))
    a = "D:/CenterCase/wired/"
    b = paste(a,content_title[j], sep = "")
    c = ".txt"
    d = paste(b,c, sep = "")
    write(content_article, file = d)
  }
}
