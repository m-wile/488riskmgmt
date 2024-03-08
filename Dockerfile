FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y git \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmysqlclient-dev

RUN git clone https://github.com/m-wile/488riskmgmt.git /srv/shiny-server/interestRisk
RUN Rscript /srv/shiny-server/interestRisk/packages.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/interestRisk', host = '0.0.0.0', port = 3838)"]