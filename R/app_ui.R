#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      theme = shinythemes::shinytheme("spacelab"),
      "上海新冠疫情概览",
      tabPanel("全市",
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
               htmlOutput('introduction'),
               leaflet::leafletOutput('plot.map.location', width = "100%", height = "700px")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "covid.sh"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
