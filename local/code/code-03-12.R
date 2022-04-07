library(rvest)
library(tidyverse)
library(lubridate)
library(furrr)
library(jsonlite)
future::plan(multisession)

# VARIABLE
v.date<-'2022-03-12'
v.url <-'https://mp.weixin.qq.com/s/MbQoeN54jg0xVDsMl4oTxw'
v.baiduapi<-'https://api.map.baidu.com/geocoding/v3/?address=ADDRESS&output=json&ak=GwzPkV44UAHW3f9gnhT80ZsLLy3zpSI6'

# Tools
mf.tag <- function(tag,startRows,totalRow){
  time=diff(c(startRows,totalRow+1))
  rep(tag,time=time)
}

# Case and Asym -----------------------------------------------------------

html.case<-read_html(v.url)
df.case.1<-data.frame(text=html.case %>% html_elements('p') %>% html_text()) %>%
  filter(str_detect(text,pattern = "病例\\d+.*，居住于")) %>%
  separate(text, into= c("t1","t2",'t3','t4','t5'),sep= "，") %>%
  rowwise() %>%
  mutate(gender=t2,
         age=as.numeric((str_extract(t3,'\\d+(?=岁)'))),
         address=str_extract(t4,'(?<=居住于).+'),
         district=str_extract(str_replace_all(address,'小区|生活区|工业园区','XIAOQU'),'.+区'),
         date=ymd(v.date))

df.asym.1<-data.frame(text=html.case %>% html_elements('p') %>% html_text()) %>%
  filter(str_detect(text,pattern = "无症状感染者\\d+.*，居住于")) %>%
  separate(text, into= c("t1","t2",'t3','t4','t5'),sep= "，") %>%
  rowwise() %>%
  mutate(gender=t2,
         age=as.numeric((str_extract(t3,'\\d+(?=岁)'))),
         address=str_extract(t4,'(?<=居住于).+'),
         district=str_extract(str_replace_all(address,'小区|生活区|工业园区','XIAOQU'),'.+区'),
         date=ymd(v.date))

df.case.1$group<-NA_character_
df.asym.1$group<-mf.tag(c('isolation','screen'),c(1,61),63)

# ADD
df.case.1 <- df.case.1 %>%
  add_row(gender='男',age=18,address='闵行区东川路800号',district="闵行区",group="isolation")

df.asym.1 <- df.asym.1 %>%
  add_row(gender='女',age=62,address='长宁区',district="长宁区",group="isolation")


df.case.2 <-df.case.1 %>%
  group_by(district,group) %>%
  summarise(n=n(),date=v.date) %>%
  select(date,district,group,n)

df.asym.2 <-df.asym.1 %>%
  group_by(district,group) %>%
  summarise(n=n(),date=v.date) %>%
  select(date,district,group,n)

df.case.3 <- df.case.1 %>%
  select(age,gender,district,address,date,group)

df.asym.3 <- df.asym.1 %>%
  select(age,gender,district,address,date,group)
# Location ----------------------------------------------------------------

df.map.1<-bind_rows("case"=df.case.3,"asym"=df.asym.3,.id="group") %>%
  mutate(address=paste0('上海市',address),
         url=str_replace(v.baiduapi,'ADDRESS',address),
         results=future_map(url,function(x){
           fromJSON(x)
         }))

df.map.2 <- df.map.1 %>%
  mutate(lng=results$result$location$lng,
         lat=results$result$location$lat)

df.map.3 <- df.map.2 %>%
  distinct(district,address,.keep_all = T) %>%
  arrange(district,address) %>%
  select(district,address,lng,lat,date)


# EXPORT ------------------------------------------------------------------
write.csv(df.case.3,file = paste0('./data/df.case.3-',v.date,'.csv'),row.names = F)
write.csv(df.asym.3,file = paste0('./data/df.asym.3-',v.date,'.csv'),row.names = F)
write.csv(df.case.2,file = paste0('./data/df.case-',v.date,'.csv'),row.names = F)
write.csv(df.asym.2,file = paste0('./data/df.asym-',v.date,'.csv'),row.names = F)
write.csv(df.map.3,file = paste0('./data/df.map-',v.date,'.csv'),row.names = F)
save.image(file= paste0('./RData/',v.date,'.rda'))


