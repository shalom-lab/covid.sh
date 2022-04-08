library(rvest)
library(tidyverse)
library(lubridate)
library(furrr)
library(jsonlite)
future::plan(multisession)

# VARIABLE
v.date<-'2022-04-07'
v.url.case<-'https://mp.weixin.qq.com/s/h_nGXZEav52TrJfIzaC1FQ'
v.url.location<-'https://mp.weixin.qq.com/s/HTM47mUp0GF-tWXkPeZJlg'
v.baiduapi<-'https://api.map.baidu.com/geocoding/v3/?address=ADDRESS&output=json&ak=GwzPkV44UAHW3f9gnhT80ZsLLy3zpSI6'

# Tools
mf.tag <- function(tag,startRows,totalRow){
  time=diff(c(startRows,totalRow+1))
  rep(tag,time=time)
}


# Case and Asym -----------------------------------------------------------


html.case<-read_html(v.url.case)

df.case.1<-data.frame(text=html.case %>% html_elements('p') %>% html_text()) %>%
  filter(str_detect(text,pattern = "病例\\d+.*，居住于")) %>%
  separate(text, into= c("t1","t2",'t3'),sep= "，") %>%
  rowwise() %>%
  mutate(n1=as.numeric(str_extract(t1,'\\d+')),
         n2=as.numeric(tail(str_extract_all(t1,'\\d+')[[1]],1)),
         n=n2-n1+1,
         district=str_extract(t2,'(?<=居住于).+'),
         date=ymd(v.date))

df.asym.1<-data.frame(text=html.case %>% html_elements('p') %>% html_text()) %>%
  filter(str_detect(text,pattern = "无症状感染者\\d+.*，居住于")) %>%
  separate(text, into= c("t1","t2",'t3'),sep= "，") %>%
  rowwise() %>%
  mutate(n1=as.numeric(str_extract(t1,'\\d+')),
         n2=as.numeric(tail(str_extract_all(t1,'\\d+')[[1]],1)),
         n=n2-n1+1,
         district=str_extract(t2,'(?<=居住于).+'),
         date=ymd(v.date))

# fill group
df.case.1$group<-mf.tag(c('isolation','screen','asym'),c(1,5,19),34)
df.asym.1$group<-mf.tag(c('isolation','screen'),c(1,17),32)

df.case.2 <-df.case.1 %>%
  select(date,district,group,n)

df.asym.2 <-df.asym.1 %>%
  select(date,district,group,n)

# Location ----------------------------------------------------------------

html.location<-read_html(v.url.location)

df.map.1<-html.location %>% html_elements('section > p ') %>% html_text() %>%
  as_tibble() %>%
  rownames_to_column(var = 'id') %>%
  mutate(id=as.integer(id),
         wordcount=str_count(value),
         districtIndex=str_detect(value,'2022年4月7日')) %>%
  arrange(-wordcount)

df.map.2<-df.map.1 %>%
  filter(wordcount>2,
         value!='已对相关居住地落实终末消毒措施。',
         value!='（滑动查看更多↓）')  %>%
  slice(-1) %>%
  arrange(id) %>%
  slice(-1) %>%
  select(-id) %>%
  rownames_to_column(var = 'id') %>%
  mutate(id=as.integer(id))

district.order<- df.map.2 %>%
  filter(districtIndex) %>%
  mutate(district=str_extract(value,"(?<=，).+区"))

# fill district
df.map.2$district<-mf.tag(district.order$district,district.order$id,dim(df.map.2)[1])

df.map.3 <- df.map.2 %>%
  filter(!districtIndex) %>%
  mutate(address=paste0('上海市',district,str_replace_all(value,'[[:punct:]]|[:space:]','')),
         url=str_replace(v.baiduapi,'ADDRESS',address),
         results=future_map(url,function(x){
           fromJSON(x)
         }),
         date=ymd(v.date)) %>%
  rowwise() %>%
  mutate(lng=results$result$location$lng,
         lat=results$result$location$lat)

df.map.4 <- df.map.3 %>%
  select(district,address,lng,lat,date)

# EXPORT ------------------------------------------------------------------
write.csv(df.case.2,file = paste0('./data/df.case-',v.date,'.csv'),row.names = F)
write.csv(df.asym.2,file = paste0('./data/df.asym-',v.date,'.csv'),row.names = F)
write.csv(df.map.4,file = paste0('./data/df.map-',v.date,'.csv'),row.names = F)
save.image(file= paste0('./RData/',v.date,'.rda'))

# check
sum(df.case.2$n)
sum(df.asym.2$n)


