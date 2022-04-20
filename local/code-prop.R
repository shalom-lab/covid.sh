library(tidyverse)
library(echarts4r)
# import data
case.asym.wider.sh<-read.csv('https://raw.githubusercontent.com/shalom-lab/covid.sh/main/local/share/case.asym.wider.sh.csv')

##
data<-case.asym.wider.sh %>%
  select(1:8) %>%
  mutate(isolation=case_isolation+asym_isolation,
         screen=case_screen+asym_screen,
         all=isolation+screen) %>%
  mutate(across(c(isolation,screen),~round(100*.x/all,1),.names = 'prop.{.col}'))

## plot
data %>%
  e_chart(date) %>%
  e_bar(name='隔离管控',prop.isolation,stack='all') %>%
  e_bar(name='筛查发现',prop.screen,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',max=100,nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_title("每日阳性感染者来源构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  )
