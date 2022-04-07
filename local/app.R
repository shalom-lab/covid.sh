
library(shinythemes)
library(shiny)


# Data-plot ---------------------------------------------------------------
source('./code-plot.R',encoding = 'utf-8')

# Shiny -------------------------------------------------------------------

## shiny
ui <- navbarPage(
  theme = shinythemes::shinytheme("spacelab"),
  "上海新冠疫情概览",
  tabPanel("全市",
           htmlOutput('introduction'),
           echarts4r::echarts4rOutput('plot.sh.line'),
           echarts4r::echarts4rOutput('plot.sh.bar')
  ),
  tabPanel("新增及累计",
           echarts4r::echarts4rOutput('plot.district.new'),
           echarts4r::echarts4rOutput('plot.district.cum')
  ),
  tabPanel("各区情况",
           echarts4r::echarts4rOutput('plot.timeline.new'),
           echarts4r::echarts4rOutput('plot.timeline.cum'),
           echarts4r::echarts4rOutput('plot.timeline.new.bar'),
           echarts4r::echarts4rOutput('plot.timeline.cum.bar')
  ),
  tabPanel("居住地信息",
           leaflet::leafletOutput('plot.map.location', width = "100%", height = "700px")
  )
)



server <- function(input, output, session) {
  output$introduction <- renderUI(
    tagList(
      span('数据摘取自上海发布微信公众号','2022.3.17-2022.4.3',tags$strong(style="color:red","同心协力,共克时艰!")),
      span('遇到数据错误请反馈至-'),
      tags$a(href="https://docs.qq.com/form/page/DWEh0V2FCdU5lWW9j", "收集表"),
      span('谢谢您的支持!'),
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

shinyApp(ui = ui, server = server)

