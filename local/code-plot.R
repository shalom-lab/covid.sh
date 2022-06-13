library(tidyverse)
library(echarts4r)
library(leaflet)
rm(list=ls())
# Load Data--------------------------------------------------------------
load('./share/data.rda')

# yaxis limit
max.cum.asym.sh<-max(case.asym.wider.sh$cum_asym)
max.cum.case.sh<-max(case.asym.wider.sh$cum_case)
max.new.asym.sh<-max(case.asym.wider.sh$asym)
max.new.case.sh<-max(case.asym.wider.sh$case)

max.new.case.di<-case.asym.wider %>% group_by(district) %>% slice_max(case,n=1) %>% pull(case) %>% max()
max.new.asym.di<-case.asym.wider %>% group_by(district) %>% slice_max(asym,n=1) %>% pull(asym) %>% max()
max.cum.case.di<-case.asym.wider %>% group_by(district) %>% slice_max(cum_case,n=1) %>% pull(cum_asym) %>% max()
max.cum.asym.di<-case.asym.wider %>% group_by(district) %>% slice_max(cum_asym,n=1) %>% pull(cum_asym) %>% max()

# By district  --------------------------------------------------------------------

plot.district.new<-case.asym.wider %>%
  group_by(district) %>%
  e_charts(date) %>%
  e_bar(case,stack="case") %>%
  e_bar(asym,stack="asym") %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(name='每日新增数',nameLocation='end',nameGap=20,max=max.new.asym.sh,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'item',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_grid(top='20%') %>%
  e_legend(top='10%') %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_title("每日新增病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='1%',itemGap=5) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.district.new


plot.district.cum<-case.asym.wider %>%
  group_by(district) %>%
  e_charts(date) %>%
  e_bar(cum_case,stack="case") %>%
  e_bar(cum_asym,stack="asym") %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(name='累计人数',nameLocation='end',nameGap=20,axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'item',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_grid(top='23%') %>%
  e_legend(top='10%') %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_title("累计病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='1%',itemGap=5)%>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.district.cum
# Leaflet -------------------------------------------------------------------

map.2.new.sp<-split(map.2.new,map.2.new$date)

groups<-names(map.2.new.sp) %>%
  as_tibble() %>%
  mutate(date=as.Date(value)) %>%
  arrange(desc(date)) %>%
  rownames_to_column(var='id')  %>%
  mutate(id=as.integer(id)) %>%
  mutate(group.name=case_when(
    id<=7~value,
    T~'七天以前'
  ))

pal <-colorFactor(c('#fff5eb','#fee6ce','#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04'),unique(groups$group.name))

base <-leaflet() %>%
  addTiles(options = providerTileOptions(minZoom = 8)) %>%
  setView(121.480473,lat = 31.235988,zoom = 9.3) %>%
  setMaxBounds(lng1 = 121.480473 + .05,
               lat1 = 31.235988 + .05,
               lng2 = 121.480473 - .05,
               lat2 = 31.235988 - .05)  %>%
  addPolygons(data = shanghai,stroke = T, weight = 1,color = '#225ea8',smoothFactor = 0.3, fillOpacity = 0.3)

base

names(map.2.new.sp) %>%
  walk(function(df){
    base <<- base %>%
      addCircles(data=map.2.new.sp[[df]],
                 lng=~lng1, lat=~lat1,
                 label=~paste0(address,'-',date),
                 radius = 50,
                 stroke = F,
                 color=~pal(groups$group.name[groups$value==df]),
                 fillOpacity = 0.5,
                 popup=~paste0(address,'-',date),
                 group = groups$group.name[groups$value==df],
                 labelOptions = labelOptions(noHide = F,direction = 'auto'))
  })

base<- base %>%
  addLayersControl(
    overlayGroups = unique(groups$group.name),
    options = layersControlOptions(collapsed = T)
  )

plot.map.location<-base
plot.map.location
# TimeLine ----------------------------------------------------------------

# 新增数
plot.timeline.new<-case.asym.wider %>%
  group_by(district) %>%
  arrange(date) %>%
  e_charts(date,timeline = T) %>%
  e_line(name="新增病例数",case,y_index = 0) %>%
  e_line(name="新增无症状",asym,y_index = 0) %>%
  e_grid(right='18%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(name='每日新增',nameLocation='end',nameGap=20,max=max.new.asym.di,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "line",
      axis='x'
    )
  ) %>%
  e_timeline_opts(right='5%',width=80,top='5%',bottom='5%',orient='vertical',
                  inverse=T,autoPlay= T,playInterval=5000,currentIndex=9,
                  label=list(position='right')) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.timeline.new
# 累计数
plot.timeline.cum<-case.asym.wider %>%
  group_by(district) %>%
  arrange(date) %>%
  e_charts(date,timeline = T) %>%
  e_line(name="累计病例数",cum_case) %>%
  e_line(name="累计无症状",cum_asym) %>%
  e_grid(left='10%',right='18%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(name='累计人数',nameLocation='end',nameGap=20,max=max.cum.asym.di,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "line",
      axis='x'
    )
  ) %>%
  e_timeline_opts(right='5%',width=80,top='5%',bottom='5%',orient='vertical',
                  inverse=T,autoPlay= T,playInterval=5000,currentIndex=9,
                  label=list(position='right')) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.timeline.cum

# bar chart 新增
plot.timeline.new.bar<-case.asym.wider %>%
  group_by(district) %>%
  arrange(date) %>%
  e_charts(date,timeline = T) %>%
  e_bar(name="病例-隔离管控",case_isolation,stack="case") %>%
  e_bar(name="病例-筛查发现",case_screen,stack="case") %>%
  e_bar(name="病例-转自无症状",case_asym,stack="case") %>%
  e_bar(name="无症状-隔离管控",asym_isolation,stack="asym") %>%
  e_bar(name="无症状-筛查发现",asym_screen,stack="asym") %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,max=max.new.asym.di,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_timeline_opts(right='5%',width=80,top='5%',bottom='5%',orient='vertical',
                  inverse=T,autoPlay= T,playInterval=3000,
                  label=list(position='right')) %>%
  e_grid(top='25%',right='18%') %>%
  e_legend(top='12%') %>%
  e_title("每日新增病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='2%',itemGap=5) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.timeline.new.bar

# bar chart 累计
plot.timeline.cum.bar<-case.asym.wider %>%
  group_by(district) %>%
  arrange(date) %>%
  e_charts(date,timeline = T) %>%
  e_bar(name="病例-隔离管控",cum_case_isolation,stack="case") %>%
  e_bar(name="病例-筛查发现",cum_case_screen,stack="case") %>%
  e_bar(name="病例-转自无症状",cum_case_asym,stack="case") %>%
  e_bar(name="无症状-隔离管控",cum_asym_isolation,stack="asym") %>%
  e_bar(name="无症状-筛查发现",cum_asym_screen,stack="asym") %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,max=max.cum.asym.di,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_timeline_opts(right='5%',width=80,top='5%',bottom='5%',orient='vertical',
                  inverse=T,autoPlay= T,playInterval=3000,
                  label=list(position='right')) %>%
  e_grid(top='25%',right='18%') %>%
  e_legend(top='12%') %>%
  e_title("累计病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='2%',itemGap=5) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.timeline.cum.bar

# SH----------------------------------------------------------------------
## line
plot.sh.line<-case.asym.wider.sh %>%
  arrange(date) %>%
  e_charts(date) %>%
  e_line(name="新增病例数",case,y_index = 0) %>%
  e_line(name="新增无症状",asym,y_index = 0) %>%
  e_line(name="累计病例数",cum_case,y_index = 0) %>%
  e_line(name="累计无症状",cum_asym,y_index = 0) %>%
  e_grid(right='5%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,
           #max=max.cum.asym.sh,
           axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "line",
      axis='x'
    )
  ) %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_labels() %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.sh.line

## bar
plot.sh.bar<-case.asym.wider.sh %>%
  arrange(date) %>%
  e_charts(date) %>%
  e_bar(name="新增病例数",barCategoryGap='2%',case,y_index = 0) %>%
  e_bar(name="新增无症状",barCategoryGap='2%',asym,y_index = 0) %>%
  e_bar(name="累计病例数",barCategoryGap='2%',cum_case,y_index = 0) %>%
  e_bar(name="累计无症状",barCategoryGap='2%',cum_asym,y_index = 0) %>%
  e_grid(right='5%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,
           #max=max.cum.asym.sh,
           axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.sh.bar


# Prop -------------------------
prop.pos.new<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='病例',prop.new.case,stack='all',
        label = list(
          position='top',
          show = F
        )) %>%
  e_bar(name='无症状感染者',prop.new.asym,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("每日新增阳性人员构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.pos.new

prop.pos.cum<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='病例',prop.cum.case,stack='all',
        label = list(
          position='top',
          show = F
        )) %>%
  e_bar(name='无症状感染者',prop.cum.asym,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("累计阳性人员构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.pos.cum

# Prop.case -------------------------

prop.case.new<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='隔离管控',prop.case_isolation,stack='all') %>%
  e_bar(name='筛查发现',prop.case_screen,stack='all') %>%
  e_bar(name='转自无症状',prop.case_asym,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',max=100,nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("每日新增病例来源构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.case.new

prop.case.cum<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='隔离管控',prop.cum_case_isolation,stack='all') %>%
  e_bar(name='筛查发现',prop.cum_case_screen,stack='all') %>%
  e_bar(name='转自无症状',prop.cum_case_asym,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',max=100,nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("累计病例来源构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.case.cum

# Prop.asym -------------------------

prop.asym.new<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='隔离管控',prop.asym_isolation,stack='all',label = list(
    position='top',
    show = F
  )) %>%
  e_bar(name='筛查发现',prop.asym_screen,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',max=100,nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("每日新增无症状感染者来源构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.asym.new

prop.asym.cum<-case.asym.wider.sh %>%
  e_chart(date) %>%
  e_bar(name='隔离管控',prop.cum_asym_isolation,stack='all',label = list(
    position='top',
    show = F
  )) %>%
  e_bar(name='筛查发现',prop.cum_asym_screen,stack='all') %>%
  e_y_axis(name='百分比',nameLocation='end',max=100,nameGap=20,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_title("累计无症状感染者来源构成",left='center',top='2%',itemGap=5) %>%
  e_legend(top='10%') %>%
  e_grid(top='20%') %>%
  # e_datazoom(type='inside') %>%
  # e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

prop.asym.cum

# 每日阳性 每日累计 ---------------------------------------------------------------------

plot.sh.pos_real<-case.asym.wider.sh %>%
  arrange(date) %>%
  e_charts(date) %>%
  e_bar(name='每日新增感染者',pos_real,barCategoryGap='2%') %>%
  e_grid(right='5%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='每日新增感染者',nameLocation='end',nameGap=20,
           #max=max.cum.asym.sh,
           axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_legend(show=F) %>%
  e_toolbox_feature(
    feature = "magicType",
    type = list("line", "bar")
  ) %>%
  e_color(c('#AD002AFF')) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.sh.pos_real


plot.sh.cum_pos_real<-case.asym.wider.sh %>%
  arrange(date) %>%
  e_charts(date) %>%
  e_bar(name='累计阳性感染者',cum_pos_real,barCategoryGap='2%') %>%
  e_grid(right='5%') %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45)) %>%
  e_y_axis(index=0,name='累计阳性感染者',nameLocation='end',nameGap=20,
           #max=max.cum.asym.sh,
           axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_datazoom(type='inside') %>%
  e_datazoom(type='slider') %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "shadow",
      axis='x'
    )
  ) %>%
  e_legend(show=F) %>%
  e_toolbox_feature(
    feature = "magicType",
    type = list("line", "bar")
  ) %>%
  e_color(c('#AD002AFF')) %>%
  e_toolbox_feature(feature = "saveAsImage",title='保存')

plot.sh.cum_pos_real


# value Box ---------------------------------------------------------------
last<-case.asym.wider.sh %>%
  arrange(date) %>%
  slice_tail(n=1)
names(last)
count.new.case<-pull(last,case)
count.new.asym<-pull(last,asym)
count.new.pos_real<-pull(last,pos_real)
count.cum.case<-pull(last,cum_case)
count.cum.pos_real<-pull(last,cum_pos_real)

# Facet -------------------------------------------------------------------

plot.facet.new<-case.asym.wider %>%
  group_by(district) %>%
  arrange(date) %>%
  e_charts(date) %>%
  e_line(name="新增无症状",asym,y_index = 0) %>%
  e_x_axis(axisLabel = list(interval = 1, rotate = 45,show=FALSE)) %>%
  e_y_axis(name='每日新增',nameLocation='end',nameGap=20,max=max.new.asym.di,axisLine=list(show=T),axisTick=list(show=T)) %>%
  e_tooltip(
    trigger = 'axis',
    axisPointer = list(
      type = "line",
      axis='x'
    )
  ) %>%
  e_facet(rows = 4, cols=4, legend_pos = "top", legend_space = 12)

plot.facet.new
