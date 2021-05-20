FROM rocker/shiny

RUN install2.r shinyWidgets shinydashboard data.table plotly DT ggplot2

RUN mkdir /app
COPY as_term_project/* /app/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]

