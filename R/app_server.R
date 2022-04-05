#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  library(dplyr)
  library(tidyr)
  library(echarts4r)
  library(leaflet)
  library(purrr)
  library(tibble)
  # Load Data--------------------------------------------------------------
  data('case.asym')
  data('map.2.new')
  data('shanghai')
  print(class(shanghai))
  # Data transfer -----------------------------------------------------------
  case.asym.wider <- case.asym %>%
    mutate(date=as.character(date)) %>%
    ungroup() %>%
    pivot_wider(names_from = c(type,group),values_from = n) %>%
    ungroup() %>%
    rowwise() %>%
    replace(is.na(.),0) %>%
    mutate(case=sum(c_across(starts_with('case_')),na.rm = T),
           asym=sum(c_across(starts_with('asym_')),na.rm = T)) %>%
    group_by(district) %>%
    arrange(district,date) %>%
    mutate(across(c(starts_with('case'),starts_with('asym')),cumsum,.names = "cum_{.col}"))

  # 全市
  names(case.asym.wider)
  case.asym.wider.sh<-case.asym.wider %>%
    group_by(date) %>%
    summarise(across(case_screen:cum_asym,sum,na.rm=T))

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
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
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
    e_title("每日新增病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='1%',itemGap=5)

  plot.district.new


  plot.district.cum<-case.asym.wider %>%
    group_by(district) %>%
    e_charts(date) %>%
    e_bar(cum_case,stack="case") %>%
    e_bar(cum_asym,stack="asym") %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
    e_y_axis(name='累计人数',nameLocation='end',nameGap=20,max=max.cum.asym.sh,axisLine=list(show=T),axisTick=list(show=T)) %>%
    e_tooltip(
      trigger = 'item',
      axisPointer = list(
        type = "shadow",
        axis='x'
      )
    ) %>%
    e_grid(top='23%') %>%
    e_legend(top='10%') %>%
    e_title("累计病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='1%',itemGap=5)

  plot.district.cum
  # Leaflet Split -------------------------------------------------------------------
  ## goo transfer

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
      options = layersControlOptions(collapsed = FALSE)
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
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
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
                    label=list(position='right'))
  plot.timeline.new
  # 累计数
  plot.timeline.cum<-case.asym.wider %>%
    group_by(district) %>%
    arrange(date) %>%
    e_charts(date,timeline = T) %>%
    e_line(name="累计病例数",cum_case) %>%
    e_line(name="累计无症状",cum_asym) %>%
    e_grid(left='10%',right='18%') %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
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
                    label=list(position='right'))

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
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
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
    e_title("每日新增病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='2%',itemGap=5)

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
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
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
    e_title("累计病例和无症状感染者人数",'左:病例,右:无症状感染者',left='center',top='2%',itemGap=5)

  plot.timeline.cum.bar

  # 全市 ----------------------------------------------------------------------
  # line
  plot.sh.line<-case.asym.wider.sh %>%
    arrange(date) %>%
    e_charts(date) %>%
    e_line(name="新增病例数",case,y_index = 0) %>%
    e_line(name="新增无症状",asym,y_index = 0) %>%
    e_line(name="累计病例数",cum_case,y_index = 0) %>%
    e_line(name="累计无症状",cum_asym,y_index = 0) %>%
    e_grid(right='18%') %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
    e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,max=max.cum.asym.sh,axisLine=list(show=T),axisTick=list(show=T)) %>%
    e_tooltip(
      trigger = 'axis',
      axisPointer = list(
        type = "line",
        axis='x'
      )
    ) %>%
    e_labels()

  plot.sh.line

  # bar
  plot.sh.bar<-case.asym.wider.sh %>%
    arrange(date) %>%
    e_charts(date) %>%
    e_bar(name="新增病例数",barCategoryGap='2%',case,y_index = 0) %>%
    e_bar(name="新增无症状",barCategoryGap='2%',asym,y_index = 0) %>%
    e_bar(name="累计病例数",barCategoryGap='2%',cum_case,y_index = 0) %>%
    e_bar(name="累计无症状",barCategoryGap='2%',cum_asym,y_index = 0) %>%
    e_grid(right='18%') %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
    e_y_axis(index=0,name='人数',nameLocation='end',nameGap=20,max=max.cum.asym.sh,axisLine=list(show=T),axisTick=list(show=T)) %>%
    e_tooltip(
      trigger = 'axis',
      axisPointer = list(
        type = "shadow",
        axis='x'
      )
    )

  plot.sh.bar
  print(class(plot.sh.bar))
  #shiny
  output$introduction <- renderUI(
    tagList(
      span('2022.3.17日起上海疫情情况，数据摘自上海发布微信公众号',
      span('遇到数据错误请反馈至-'),
      tags$a(href="https://docs.qq.com/form/page/DWEh0V2FCdU5lWW9j", "收集表"),
      span('谢谢您的支持!'),
      tags$strong(style="color:red","同心协力,共克时艰!")),
    )
  )
  output$plot.district.cum<-renderEcharts4r(plot.district.cum)
  output$plot.district.new<-renderEcharts4r(plot.district.new)
  output$plot.map.location<-renderLeaflet(plot.map.location)
  output$plot.sh.bar<-renderEcharts4r(plot.sh.bar)
  output$plot.sh.line<-renderEcharts4r(plot.sh.line)
  output$plot.timeline.cum<-renderEcharts4r(plot.timeline.cum)
  output$plot.timeline.cum.bar<-renderEcharts4r(plot.timeline.cum.bar)
  output$plot.timeline.new<-renderEcharts4r(plot.timeline.new)
  output$plot.timeline.new.bar<-renderEcharts4r(plot.timeline.new.bar)
}
