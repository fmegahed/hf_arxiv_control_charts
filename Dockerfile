FROM rocker/r-ver:4.5.2

# Install system dependencies for R packages
# - libcurl4-openssl-dev, libssl-dev: for httr/curl (ellmer dependency)
# - libxml2-dev: for xml2 (ellmer dependency)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /code

# Install remotes package first, then install specific versions of required packages
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('shiny', version = '1.12.1', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('shinyjs', version = '2.1.0', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('dplyr', version = '1.1.4', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('tidyr', version = '1.3.1', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('purrr', version = '1.2.1', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('readr', version = '2.1.5', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('lubridate', version = '1.9.4', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('plotly', version = '4.10.4', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('DT', version = '0.33', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_version('httr', version = '1.4.7', repos = 'https://cloud.r-project.org')" \
    && R -e "remotes::install_github('tidyverse/ellmer')"

COPY . .

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
