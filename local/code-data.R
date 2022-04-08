library(tidyverse)
library(geojsonio)
library(readr)
library(jsonlite)

rm(list=ls())

# Data Import --------------------------------------------------------------

case.1<-dir(path = 'data/',pattern = "^df.case-",full.names = T) %>%
  as_tibble()

case.1$data <- map(case.1$value,~read_csv(.x,locale = locale(encoding = "GB2312")))

case.2<-case.1 %>%
  unnest(cols=data) %>%
  select(-value)

# asym
asym.1<-dir(path = 'data/',pattern = "^df.asym-",full.names = T) %>%
  as_tibble()

asym.1$data <- map(asym.1$value,~read_csv(.x,locale = locale(encoding = "GB2312")))

asym.2<-asym.1 %>%
  unnest(cols=data) %>%
  select(-value)

# map
map.1<-dir(path = 'data/',pattern = "^df.map",full.names = T) %>%
  as_tibble()

map.1$data <- map(map.1$value,~read_csv(.x,locale = locale(encoding = "GB2312")))

map.2<-map.1 %>%
  unnest(cols=data) %>%
  select(-value)

# case and asym
case.asym<-bind_rows(list(case=case.2,asym=asym.2),.id='type')


# Data Transform ----------------------------------------------------------

case.asym.wider <- case.asym %>%
  mutate(date=as.character(date)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(type,group),values_from = n) %>%
  ungroup() %>%
  rowwise() %>%
  replace(is.na(.),0) %>%
  mutate(case=sum(c_across(starts_with('case_')),na.rm = T),
         asym=sum(c_across(starts_with('asym_')),na.rm = T),
         pos=case+asym) %>%
  group_by(district) %>%
  arrange(district,date) %>%
  mutate(across(c(starts_with('case'),starts_with('asym'),pos),cumsum,.names = "cum_{.col}"))

# 全市
names(case.asym.wider)
case.asym.wider.sh<-case.asym.wider %>%
  group_by(date) %>%
  summarise(across(case_isolation:cum_asym,sum,na.rm=T)) %>%
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


# Data Export ------------------------------------------------------------------

# nodejs
write_json(map.2,'./data/map.2.json')
shell("cd go-transfer && ls -F && node index.js")

# load
map.2.new<-fromJSON('./data/map.2.new.json')
shanghai<-geojson_read('./data/shanghai.json',what = "sp")

# save rda
save(map.2.new,case.asym.wider,case.asym.wider.sh,shanghai,file='./data.rda')

# for shiny
save(map.2.new,case.asym.wider,case.asym.wider.sh,shanghai,file='../data-raw/data.rda')

# render
rmarkdown::render('./index.Rmd',output_file = "./index.html")
file.copy('./index.html','../www/index.html',overwrite = T)





