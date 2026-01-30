FROM rocker/shiny:4.5.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Install packages (and fail build if anything goes wrong)
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')" && \
    R -e "remotes::install_github('rstudio/httpuv')" && \
    R -e "install.packages(c('shiny','shinyjs','dplyr','tidyr','purrr','readr','lubridate','plotly','DT','httr','ellmer','jsonlite'), repos='https://cloud.r-project.org')" && \
    R -e "stopifnot(requireNamespace('shiny', quietly = TRUE))"

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860)"]
