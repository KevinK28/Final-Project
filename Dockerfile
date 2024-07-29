# start from RStudio/plumber image
FROM rocker/r-ver:3.5.0

RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev
    
    
# install plumber, GGally
RUN R -e "install.packages('tidverse')"
RUN R -e "install.packages('caret')"

# copy everything from the current directory into the container
COPY api.R api.R

# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"]