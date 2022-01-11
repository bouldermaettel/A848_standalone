
FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
# libcairo2-dev for RpostgreSQL package
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libpq-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the shiny app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# install packages
RUN R -e "install.packages(c('shiny', 'RPostgreSQL', 'rmarkdown', 'DT', 'data.table','shinythemes' ,'shinyWidgets', 'DT', 'tidyverse', 'shinydashboard', 'shinydashboardPlus','data.table', 'fresh','shinyjs', 'shinyBS','openxlsx', 'excelR'), repos='https://cloud.r-project.org/')"

#'dplyr','tibble','readr'
## install dependencies if required
#RUN R -e "install.packages('DT', repos='https://cloud.r-project.org/')"

################################################################################
# Python is not required
################################################################################

#RUN apt-get update && \
#    apt-get install -y python3 python3-pip && \
#    rm -rf /var/lib/apt/lists/*
#
## Dash and dependencies
#RUN pip3 install matplotlib

# copy the app to the image
RUN mkdir /root/app/
COPY . /root/app/

COPY Rprofile.site /usr/lib/R/etc/
#RUN mkdir /root/data/
#COPY data /root/data/
#RUN mkdir /root/www/
#COPY www /root/www/

EXPOSE 3838
EXPOSE 5432

CMD ["R", "-e", "shiny::runApp('/root/app.R')"]


# docker build -t test-app .
# docker run -it -p 3838:3838 test-app
# docker kill $(docker ps -q)

# docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-net -p 8080:8080 sp-built
# bouldermaettel@bouldermaettelsZBOOK:~/PycharmProjects/A848/Docker_container$ R -e "shiny::runApp('app')"

# docker tag duplicate-finder:latest bouldermaettel/dupliate-finder:latest
# docker login -u bouldermaettel
# docker push bouldermaettel/dupliate-finder:latest
# bouldermaettel@bouldermaettelsZBOOK:~/PycharmProjects/A848/Docker_container$ R -e "shiny::runApp('app')"