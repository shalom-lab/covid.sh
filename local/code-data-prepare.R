library(tidyverse)
library(geojsonio)
library(readr)
library(jsonlite)

rm(list=ls())

# Date merge --------------------------------------------------------------

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


write_json(map.2,'./data/map.2.json')

# nodejs
# export

map.2.new<-fromJSON('./data/map.2.new.json')
shanghai<-geojson_read('./data/shanghai.json',what = "sp")

save(map.2.new,case.asym,shanghai,file='./data.rda')
# for shiny
save(map.2.new,case.asym,shanghai,file='../data-raw/data.rda')

# render
rmarkdown::render('./index.Rmd',output_file = "./index.html")
file.copy('./index.html','../www/index.html',overwrite = T)
