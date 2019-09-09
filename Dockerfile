FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the filesystem-ui app
#RUN apt-get update && apt-get install -y \
#    libmpfr-dev
#RUN apt-get update && apt-get install -y \
	libgdal1-dev\
	libgeos-dev\
	libproj-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the filesystem-ui app
RUN R -e "install.packages(c('Rcpp', 'units', 'shinyjs', 'shinyTime', 'leaflet', 'dplyr', 'plyr', 'tidyr', 'stringr', 'sf', 'zoo', 'data.table', 'DT', 'leaflet.extras', 'sp', 'rgdal', 'raster'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/filesystem-ui
COPY filesystem-ui /root/filesystem-ui

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/filesystem-ui')"]
