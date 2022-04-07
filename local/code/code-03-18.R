library(rvest)
library(tidyverse)
library(lubridate)
library(furrr)
library(jsonlite)
future::plan(multisession)

# VARIABLE
v.date<-'2022-03-18'
v.url.case<-'https://mp.weixin.qq.com/s/OFt7LzeHt8fNl6GqPpkD1g'
v.url.location<-'https://mp.weixin.qq.com/s/xLVPnOTErTe3dmAenUyDGQ'
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
  separate(text, into= c("t1","t2",'t3','t4','t5'),sep= "，") %>% 
  rowwise() %>%
  mutate(gender=t2,
         age=as.numeric((str_extract(t3,'\\d+(?=岁)'))),
         district=str_extract(t4,'(?<=居住于).+'),
         date=ymd(v.date))

df.asym.1<-data.frame(text=html.case %>% html_elements('p') %>% html_text()) %>%
  filter(str_detect(text,pattern = "无症状感染者\\d+.*，居住于")) %>% 
  separate(text, into= c("t1","t2",'t3','t4','t5'),sep= "，") %>% 
  rowwise() %>%
  mutate(gender=t2,
         age=as.numeric((str_extract(t3,'\\d+(?=岁)'))),
         district=str_extract(t4,'(?<=居住于).+'),
         date=ymd(v.date))

df.case.1$group<-mf.tag(c('isolation','screen'),c(1,5),8)
df.asym.1$group<-mf.tag(c('isolation','screen'),c(1,179),366)


df.case.2 <-df.case.1 %>%
  group_by(district,group) %>%
  summarise(n=n(),date=v.date) %>%
  select(date,district,group,n)


df.asym.2 <-df.asym.1 %>%
  group_by(district,group) %>%
  summarise(n=n(),date=v.date) %>%
  select(date,district,group,n)

df.case.3 <- df.case.1 %>%
  select(age,gender,district,date,group)

df.asym.3 <- df.asym.1 %>%
  select(age,gender,district,date,group)
# Location ----------------------------------------------------------------

html.location<-read_html(v.url.location)

html.location %>% html_elements('section >strong') %>% html_text()
html.location %>% html_elements('section >strong,section > p > span') %>% html_text()

df.map.1<-html.location %>% html_elements('section >strong,section > p > span') %>% html_text() %>% 
  as_tibble() %>%
  rownames_to_column(var = 'id') %>%
  mutate(id=as.integer(id),
         wordcount=str_count(value),
         districtIndex=str_detect(value,'区$')) %>%
  arrange(-wordcount)

df.map.2<-df.map.1 %>%
  filter(wordcount>2,str_detect(value,'已对相关居住地落实',negate=T)) %>% 
  slice(-1) %>%
  arrange(id) %>% 
  slice(-1) %>% 
  slice(-1) %>% 
  select(-id) %>%
  rownames_to_column(var = 'id') %>%
  mutate(id=as.integer(id))

district.order<- df.map.2 %>%
  filter(districtIndex) %>%
  mutate(district=value)

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
write.csv(df.case.3,file = paste0('./data/df.case.3-',v.date,'.csv'),row.names = F,)
write.csv(df.asym.3,file = paste0('./data/df.asym.3-',v.date,'.csv'),row.names = F)
write.csv(df.case.2,file = paste0('./data/df.case-',v.date,'.csv'),row.names = F)
write.csv(df.asym.2,file = paste0('./data/df.asym-',v.date,'.csv'),row.names = F)
write.csv(df.map.4,file = paste0('./data/df.map-',v.date,'.csv'),row.names = F)
save.image(file= paste0('./RData/',v.date,'.rda'))




