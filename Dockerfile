FROM rocker/shiny:4.5.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Install packages with pinned versions for reproducibility
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('shiny', version='1.12.1', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('shinyjs', version='2.1.1', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('dplyr', version='1.1.4', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('tidyr', version='1.3.2', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('purrr', version='1.2.1', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('readr', version='2.1.6', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('lubridate', version='1.9.4', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('plotly', version='4.10.4', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('DT', version='0.33', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('httr', version='1.4.7', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('ellmer', version='0.4.0', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('jsonlite', version='2.0.0', repos='https://cloud.r-project.org')" \
    && R -e "remotes::install_version('commonmark', version='2.0.0', repos='https://cloud.r-project.org')" \
    && R -e "stopifnot(requireNamespace('shiny', quietly = TRUE))"

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860)"]
