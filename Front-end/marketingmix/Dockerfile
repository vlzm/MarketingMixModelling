FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*


RUN install2.r --error --skipinstalled \
    shiny \
    forecast \
    jsonlite \
    ggplot2 \
    htmltools \
    plotly

RUN install2.r --error --skipinstalled \
    data.table

RUN install2.r --error --skipinstalled \
    gridExtra 

RUN install2.r --error --skipinstalled \
    shinyWidgets 

RUN install2.r --error --skipinstalled \
    plyr 

RUN install2.r --error --skipinstalled \
    minpack.lm

RUN install2.r --error --skipinstalled \
    readxl

RUN install2.r --error --skipinstalled \
    corrplot

RUN install2.r --error --skipinstalled \
    lubridate

RUN install2.r --error --skipinstalled \
    pbapply

RUN install2.r --error --skipinstalled \
    Rsolnp

RUN install2.r --error --skipinstalled \
    shinydashboard

RUN install2.r --error --skipinstalled \
    tableHTML

RUN install2.r --error --skipinstalled \
    dashboardthemes

RUN install2.r --error --skipinstalled \
    shinymanager

RUN install2.r --error --skipinstalled \
    shinythemes

RUN install2.r --error --skipinstalled \
    shinyjs

RUN install2.r --error --skipinstalled \
    shinycssloaders

RUN install2.r --error --skipinstalled \
    shinyalert

# copy the app to the image
RUN mkdir /root/app
COPY app /root/app

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app', host='0.0.0.0', port=3838)"]

