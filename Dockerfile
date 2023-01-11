# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    

RUN R -e "install.packages(pkgs=c('shiny','tidyverse','shinydashboard','flexdashboard', 'shinythemes', 'DT','dplyr', 'wordcloud', 'tensorflow', 'keras', 'tfdatasets', 'coro', 'shinycssloaders', 'reticulate', 'tidytext'), repos='https://cran.rstudio.com/')" 

#******************
## Install R packages
RUN R -q -e 'install.packages("remotes")'
RUN R -q -e 'remotes::install_github("rstudio/reticulate")'

# Install Autogluon
RUN R -q -e 'reticulate::install_miniconda()'
RUN R -q -e 'reticulate::conda_create(envname = "r-autogluon", packages = c("python=3.8.13", "numpy"))'
# RUN R -q -e 'reticulate::conda_list()'
RUN R -q -e 'reticulate::conda_install(envname = "r-autogluon", packages = "autogluon", pip = TRUE)'

RUN R -q -e 'reticulate::conda_install(envname = "r-autogluon", packages = "keras", pip = TRUE)'

RUN R -q -e 'reticulate::conda_install(envname = "r-autogluon", packages = "tensorflow", pip = TRUE)'


## Modify Rprofile
RUN R -e 'write("reticulate::use_condaenv(\"r-autogluon\", required = TRUE)",file=file.path(R.home(),"etc","Rprofile.site"),append=TRUE)'
RUN R -e 'write("reticulate::import(\"autogluon.tabular\")",file=file.path(R.home(),"etc","Rprofile.site"),append=TRUE)'
#*******************

RUN mkdir /root/app

COPY Projet_DE /root/shiny_save

COPY Rprofile.site /usr/lib/R/etc/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# RUN dos2unix /usr/bin/shiny-server.sh && apt-get --purge remove -y dos2unix && rm -rf /var/lib/apt/lists/*
CMD ["R", "-e", "shiny::runApp('/root/shiny_save', host='0.0.0.0', port=3838)"]

#CMD ["R", "-e", "shiny::runApp('/root/shiny_save')"]
