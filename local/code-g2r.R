library(tidyverse)
library(g2r)

# Load Data--------------------------------------------------------------
load('./share/data.rda')

g2r.pos_real<-g2(case.asym.wider,asp(y=pos_real,x=date,color=district)) %>%
  fig_line() %>%
  planes(~district,type='list',cols=4,padding=c(30,10)) %>%
  motif(padding = 30) %>%
  tooltip(shared = TRUE,
          title=htmlwidgets::JS("(title, datum) => {
                return(datum['date']+'\\n净增阳性感染人数(pos_real)')
          }"),
          showCrosshairs=TRUE)

g2r.pos_real
