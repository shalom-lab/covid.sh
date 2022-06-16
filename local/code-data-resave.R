library(tidyverse)
library(purrr)


# Resave ------------------------------------------------------------------

paths<-dir('RData',full.names = T)

walk(paths,function(path){
  load(path)
  dfName<-ifelse('df.map.4' %in% ls(),'df.map.4','df.map.3')
  print(path)
  write_excel_csv(df.case.2,file = paste0('./data/df.case-',v.date,'.csv'))
  write_excel_csv(df.asym.2,file = paste0('./data/df.asym-',v.date,'.csv'))
  write_excel_csv(get(dfName),file = paste0('./data/df.map-',v.date,'.csv'))
})


# Load all data -----------------------------------------------------------

# case
case.1<-dir(path = 'data/',pattern = "^df.case-",full.names = T) %>%
  as_tibble()
case.1$data <- map(case.1$value,~read_csv(.x))

case.2<-case.1 %>%
  unnest(cols=data) %>%
  select(-value)

# asym
asym.1<-dir(path = 'data/',pattern = "^df.asym-",full.names = T) %>%
  as_tibble()

asym.1$data <- map(asym.1$value,~read_csv(.x))

asym.2<-asym.1 %>%
  unnest(cols=data) %>%
  select(-value)

# map
map.1<-dir(path = 'data/',pattern = "^df.map",full.names = T) %>%
  as_tibble()

map.1$data <- map(map.1$value,~read_csv(.x))

map.2<-map.1 %>%
  unnest(cols=data) %>%
  select(-value)


# save all data to database -----------------------------------------------
write_excel_csv(case.2,file = here::here('data/database/case.2.all.csv'))
write_excel_csv(asym.2,file = here::here('data/database/asym.2.all.csv'))
write_excel_csv(map.2,file = here::here('data/database/map.2.all.csv'))

