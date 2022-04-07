FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
RUN apt-get install fonts-wqy-zenhei
RUN R -e 'install.packages(c("lubridate","echarts4r","jsonlite","leaflet","geojsonio","shinythemes"))'
    
COPY /data.shanghai.json /srv/shiny-server/covid-sh/data.shanghai.json
COPY /data.map.2.new.csv /srv/shiny-server/covid-sh/data.map.2.new.csv
COPY /data.case.asym.csv /srv/shiny-server/covid-sh/data.case.asym.csv
COPY /code-plot.R /srv/shiny-server/covid-sh/code-plot.R
COPY /app.R /srv/shiny-server/covid-sh/app.R
