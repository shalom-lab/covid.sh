library(rvest)
library(tidyverse)
library(lubridate)
library(furrr)
library(jsonlite)
library(janitor)
library(here)
library(geojsonio)
future::plan(multisession)

rm(list=ls())
# VARIABLE
df.url<-readRDS('data/df.url.RDS')
df.url<-df.url %>%
  bind_rows(c(v.date='2022-06-24',
            v.url.case='',
            v.url.location='')) %>%
  distinct(v.date,.keep_all = T) %>%
  arrange(v.date)
saveRDS(df.url,'data/df.url.RDS')

tem.df<-filter(df.url,v.date=='2022-06-24')

v.date<-pull(tem.df,v.date)
v.url.case<-pull(tem.df,v.url.case)
v.url.location<-pull(tem.df,v.url.location)
v.baiduapi<-'https://api.map.baidu.com/geocoding/v3/?address=ADDRESS&output=json&ak=GwzPkV44UAHW3f9gnhT80ZsLLy3zpSI6'
# Tools
mf.tag <- function(tag,startRows,totalRow){
  time=diff(c(startRows,totalRow+1))
  rep(tag,time=time)
}


# Case and Asym -----------------------------------------------------------

html.case<-read_html(v.url.case)

tag<-'p'
df.case.1<-data.frame(text=html.case %>% html_elements(tag) %>% html_text()) %>%
  filter(str_detect(text,pattern = "病例\\d+.*，居住于")) %>%
  separate(text, into= c("t1","t2",'t3'),sep= "，") %>%
  rowwise() %>%
  mutate(n1=as.numeric(str_extract(t1,'\\d+')),
         n2=as.numeric(tail(str_extract_all(t1,'\\d+')[[1]],1)),
         n=n2-n1+1,
         district=str_extract(t2,'(?<=居住于).+'),
         date=ymd(v.date)) %>%
  filter(!is.na(district))

df.asym.1<-data.frame(text=html.case %>% html_elements(tag) %>% html_text()) %>%
  filter(str_detect(text,pattern = "^无症状感染者")) %>%
  separate(text, into= c("t1","t2",'t3'),sep= "，") %>%
  rowwise() %>%
  mutate(n1=as.numeric(str_extract(t1,'\\d+')),
         n2=as.numeric(tail(str_extract_all(t1,'\\d+')[[1]],1)),
         n=n2-n1+1,
         district=str_extract(t2,'(?<=居住于).+'),
         date=ymd(v.date)) %>%
  mutate(district=str_remove(district,'[:punct:]')) %>%
  filter(!is.na(district))

# fill group
df.case.1$group<-mf.tag(c('isolation'),c(1),dim(df.case.1)[1])
df.asym.1$group<-mf.tag(c('isolation'),c(1),dim(df.asym.1)[1])

df.case.2 <-df.case.1 %>%
  select(date,district,group,n)

df.asym.2 <-df.asym.1 %>%
  select(date,district,group,n)

df.case.2 <- tribble(
  ~date,~district,~group,~n,
  ymd('2022-06-24'),'宝山区','isolation',0,
)
df.asym.2 <- tribble(
 ~date,~district,~group,~n,
 ymd('2022-06-24'),'宝山区','isolation',0,
)


# Location ----------------------------------------------------------------

html.location<-read_html(v.url.location)

df.map.1<-html.location %>% html_elements('section > p ') %>% html_text() %>%
  as_tibble() %>%
  rownames_to_column(var = 'id') %>%
  mutate(id=as.integer(id),
         wordcount=str_count(value),
         districtIndex=str_detect(value,'2022年6月.+日')) %>%
  arrange(-wordcount)

df.map.2<-df.map.1 %>%
  filter(wordcount>2,
         !str_detect(value,'已对相关.+落实.+'),
         !str_detect(value,'各区信息如下'),
         !str_detect(value,'滑动查看更多'),
         !str_detect(value,'市卫健委')) %>%
  arrange(id) %>%
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
         date=ymd(v.date)) %>%
  mutate(results=future_map(url,function(x){
    fromJSON(x)
  })) %>%
  rowwise() %>%
  mutate(lng=results$result$location$lng,
         lat=results$result$location$lat)

df.map.4 <- df.map.3 %>%
  select(district,address,lng,lat,date)

df.map.4 <- tribble(
 ~district,~address,~lng,~lat,~date,
 NA_character_,NA_character_,NA_real_,NA_real_,ymd('2022-06-24')
)

# Save Daily Data ------------------------------------------------------------------
write_excel_csv(df.case.2,file = paste0('./data/df.case-',v.date,'.csv'))
write_excel_csv(df.asym.2,file = paste0('./data/df.asym-',v.date,'.csv'))
write_excel_csv(df.map.4,file = paste0('./data/df.map-',v.date,'.csv'))
save.image(file= paste0('./RData/',v.date,'.rda'))

# check
sum(df.case.2$n)
sum(df.asym.2$n)

# Update Database -----------------------------------------------------------

case.2.all<-read_csv(here::here('data/database/case.2.all.csv'),show_col_types = FALSE)
asym.2.all<-read_csv(here::here('data/database/asym.2.all.csv'),show_col_types = FALSE)
map.2.all<-read_csv(here::here('data/database/map.2.all.csv'),show_col_types = FALSE)

#merge & save
bind_rows(case.2.all,df.case.2) %>%
  distinct_all() %>% write_excel_csv(here::here('data/database/case.2.all.csv'))

bind_rows(asym.2.all,df.asym.2) %>%
  distinct_all() %>% write_excel_csv(here::here('data/database/asym.2.all.csv'))

bind_rows(map.2.all,df.map.4) %>%
  distinct_all() %>% write_excel_csv(here::here('data/database/map.2.all.csv'))


# Data Transform ----------------------------------------------------------
remove(list=ls())
#read data
case.2.all<-read_csv(here::here('data/database/case.2.all.csv'),show_col_types = FALSE)
asym.2.all<-read_csv(here::here('data/database/asym.2.all.csv'),show_col_types = FALSE)
map.2.all<-read_csv(here::here('data/database/map.2.all.csv'),show_col_types = FALSE)

case.asym<-bind_rows(list(case=case.2.all,asym=asym.2.all),.id='type')

case.asym.wider <- case.asym %>%
  mutate(date=as.character(date)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(type,group),values_from = n,id_expand = T) %>%
  ungroup() %>%
  rowwise() %>%
  replace(is.na(.),0) %>%
  mutate(case=sum(c_across(starts_with('case_')),na.rm = T),
         asym=sum(c_across(starts_with('asym_')),na.rm = T),
         pos=case+asym,
         pos_real=pos-case_asym) %>%
  group_by(district) %>%
  arrange(district,date) %>%
  mutate(across(c(starts_with('case'),starts_with('asym'),pos,pos_real),cumsum,.names = "cum_{.col}"))

# 全市
names(case.asym.wider)
case.asym.wider.sh<-case.asym.wider %>%
  group_by(date) %>%
  summarise(across(case_isolation:pos_real,sum,na.rm=T)) %>%
  arrange(date) %>%
  mutate(across(c(starts_with('case'),starts_with('asym'),pos,pos_real),cumsum,.names = "cum_{.col}")) %>%
  mutate(all.new=case+asym,
         prop.new.case=round(100*case/all.new,1),
         prop.new.asym=round(100*asym/all.new,1),
         all.cum=cum_case+cum_asym,
         prop.cum.case=round(100*cum_case/all.cum,1),
         prop.cum.asym=round(100*cum_asym/all.cum,1)
  ) %>%
  mutate(across(c(case_screen,case_asym,case_isolation),~round(.x*100/case,1),.names = "prop.{col}"),
         across(c(asym_screen,asym_isolation),~round(.x*100/asym,1),.names = "prop.{col}"),
         across(c(cum_case_screen,cum_case_asym,cum_case_isolation),~round(.x*100/cum_case,1),.names = "prop.{col}"),
         across(c(cum_asym_screen,cum_asym_isolation),~round(.x*100/cum_asym,1),.names = "prop.{col}"),
  )

# Variable Description  ---------------------------------------------------
names(case.asym.wider)
variables<-data.frame(
  name=names(case.asym.wider),
  class=sapply(case.asym.wider,class),
  description=c('日期','区','病例-隔离管控发现','病例-筛查发现','病例-转自无症状',
                '无症状-隔离管控发现','无症状-筛查发现','新增病例(case_isolation+case_screen+case_asym)','新增无症状(asym_isolation+asym_screen)','阳性(case+asym)',
                '净增阳性(case+asym-case_asym)',rep('累计(从3月9日计起)~',time=9))
) %>%
  set_names(c('变量名','类型','描述')) %>%
  remove_rownames()


# Map Data Node js  -------------------------------------------------------

write_json(map.2.all,here::here('data/database/map.2.all.json'))
shell("cd ./go-transfer && ls -F && node index.js")

map.2.all.new<-fromJSON(here::here('data/database/map.2.all.new.json'))
shanghai<-geojson_read(here::here('data/database/shanghai.json'),what = "sp")


# Share Data --------------------------------------------------------------

save(map.2.all.new,case.asym.wider,case.asym.wider.sh,shanghai,variables,file='./share/data.rda')
write_excel_csv(case.asym.wider,'./share/case.asym.wider.csv')
write_excel_csv(case.asym.wider.sh,'./share/case.asym.wider.sh.csv')
write_excel_csv(map.2.all.new,'./share/map.2.new.csv')
write_excel_csv(variables,'./share/variables')


# Render HTML -------------------------------------------------------------

rmarkdown::render('./map.Rmd',output_file = paste0('../www/map/index.html'))

rmarkdown::render('./index.Rmd',output_file = paste0('../www/index.html'))




# git add . && git commit -m "update"
