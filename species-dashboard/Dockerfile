FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','plotly','leaflet','mapview','tidyverse'), repos='https://cran.rstudio.com/')"
## fixing running as non root
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinyWidgets','shinythemes','shinycssloaders','FSA','rgdal'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


COPY www /srv/shiny-server/speciesdash/www
COPY Data /srv/shiny-server/speciesdash/Data
COPY R /srv/shiny-server/speciesdash/R
COPY google-analytics.js /srv/shiny-server/speciesdash/
COPY README.md /srv/shiny-server/speciesdash/
COPY server.R /srv/shiny-server/speciesdash/
COPY ui.R /srv/shiny-server/speciesdash/


EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
