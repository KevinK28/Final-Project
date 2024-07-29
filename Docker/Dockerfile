# start from RStudio/plumber image
FROM rocker/r-ver:4.3.2

RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev
    
    
# install plumber, GGally
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('caret')"
Run R -e "install.packages('plumber')"

# copy everything from the current directory into the container
COPY api.r api.r
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv

# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('api.r'); pr$run(host='0.0.0.0', port=8000)"]