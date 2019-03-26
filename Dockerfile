FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','plotly','leaflet','mapview'), repos='https://cran.rstudio.com/')"
## fixing running as non root
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinythemes','shinycssloaders','FSA'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# copy shiny-server config file
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY www /srv/shiny-server/www
COPY CompleteAgeCases20190321.rds /srv/shiny-server/
COPY CompleteLengthCases20190321.rds /srv/shiny-server/
COPY cc.age.sample20190321.rds /srv/shiny-server/
COPY bio.data.sample20190321.rds /srv/shiny-server/
COPY google-analytics.js /srv/shiny-server/
COPY README.md /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY ["Supplemental data.csv","/srv/shiny-server/"]
COPY ui.R /srv/shiny-server/
COPY FullData.rds /srv/shiny-server/
COPY ["Data extraction and formatting.R", "/srv/shiny-server/"]
RUN Rscript -e "install.packages(c('rgdal'), repos='https://cran.rstudio.com/')"
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
