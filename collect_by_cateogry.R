# original code:
# curl을 활용한 멀티 수집
# https://github.com/forkonlp/N2H4/wiki/curl%EC%9D%84-%ED%99%9C%EC%9A%A9%ED%95%9C-%EB%A9%80%ED%8B%B0-%EC%88%98%EC%A7%91

library(curl)
library(rvest)
library(N2H4)

options(stringsAsFactors = F)

# check collection:
success <- function(res){
  res$content<-iconv(rawToChar(res$content),from="CP949",to="UTF-8")
  data <<- c(data, list(res))
}
failure <- function(msg){
  cat("Oh noes! Request failed!", msg, "\n")
}

# get category:
cate<-getMainCategory()
# A tibble: 6 x 2
#   cate_name sid1 
#   <chr>     <chr>
# 1 정치      100  
# 2 경제      101  
# 3 사회      102  
# 4 생활/문화 103  
# 5 세계      104  
# 6 IT/과학   105
subcate<-lapply(c("100","101","102","104","105"), getSubCategory)

scate<-c()
for(i in 1:length(subcate)){
  scate<-rbind(scate, data.frame(cate_name=cate[i,1],sid1=cate[i,2],subcate[[i]]))
}

# set date:
strDate<-as.Date("2019-12-19")
endDate<-as.Date("2017-03-29")

strTime<-Sys.time()
midTime<-Sys.time()


# set data dir:
data_dir = '/media/data1/Korpora/navernews/'

for (date in strDate:endDate){
  date<-gsub("-","",as.character(as.Date(date,origin = "1970-01-01")))
  for (i in 1:nrow(scate)){
    print(paste0(date," / ",scate[i,1] ," - ",scate[i,3]))
    
    pageUrli<-paste0("http://news.naver.com/main/list.nhn?sid2=",scate[i,4],"&sid1=",scate[i,2],"&mid=shm&mode=LS2D&date=",date)
    trym<-0
    max<-try(getMaxPageNum(pageUrli), silent = T)
    while(trym<=5&&class(max)=="try-error"){
      max<-try(getMaxPageNum(pageUrli), silent = T)
      Sys.sleep(abs(rnorm(1)))
      trym<-trym+1
      print(paste0("try again max num: ",pageUrli))
    }
    closeAllConnections()
    for (pageNum in 1:max){
      data_path =  paste0(data_dir, "cate_",scate[i,4],"/news",scate[i,2],"_",scate[i,4],"_",date,"_",pageNum,".csv")
      # check isExists?
      if(file.exists(data_path)){
        print(paste0("Skip ", date," / ",scate[i,1]," / ",scate[i,3]," / ",pageNum))
        next
      }

      print(paste0(date," / ",scate[i,1]," / ",scate[i,3]," / ",pageNum))
      midTime<-Sys.time()
      pageUrl<-paste0(pageUrli,"&page=",pageNum)
      tryp<-0
      newsList<-try(getUrlListByCategory(pageUrl), silent = T)
      while(tryp<=5&&class(newsList)=="try-error"){
        newsList<-try(getUrlListByCategory(pageUrl), silent = T)
        Sys.sleep(abs(rnorm(1)))
        tryp<-tryp+1
        print(paste0("try again max num: ",pageUrl))
      }

      if(nrow(newsList)==0){
        print(paste0("no news link: ", pageUrl))
        next
      }

      closeAllConnections()
      pool <- new_pool()
      data <- list()
      try({
          sapply(newsList$links, function(x) curl_fetch_multi(x,success,failure))
          res <- multi_run()

          if( identical(data, list()) ){
            res <- multi_run()
          }

          cont<-sapply(data, function(x) x$content)
          mask<-is.na(cont)
          cont<-cont[!mask]
          html2text <- lapply(cont,function(x) read_html(x))

          titles<-unlist(lapply(html2text,function(x) getContentTitle(x)))
          bodies<-unlist(lapply(html2text,function(x) getContentBody(x)))
          presses<-unlist(lapply(html2text,function(x) getContentPress(x)))
          urls<-unlist(newsList$links[!mask])
          datetimes<-lapply(html2text,function(x) getContentDatetime(x)[1])
          datetimes<-sapply(datetimes, function(x) (as.character(x)[1]))
          df<-data.frame(url=urls,datetime=datetimes,press=presses,title=titles,body=bodies)

          dir.create(paste0(data_dir, "/cate_",scate[i,4]),showWarnings=F)
          write.csv(df, file=paste0(data_path))
          closeAllConnections()
                            
          })
      
    }
  }
}
