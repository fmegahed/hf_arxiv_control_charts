FROM rocker/shiny:4.5.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy renv files first for better layer caching
# (packages only reinstall if renv.lock changes)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages from lockfile
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" \
    && R -e "renv::restore()"

# Copy application code (changes more frequently).
# app.R needs R/ (all logic), config/ (factsheet specification, app settings,
# field ratings), www/ (styles, scripts, logos) and data/ (factsheets, metadata).
COPY app.R app.R
COPY R/ R/
COPY config/ config/
COPY www/ www/
COPY data/ data/

# Verify that the packages the app loads are available and that it can be built
RUN R -e "for (p in c('shiny', 'shinychat', 'ellmer', 'plotly', 'DT', 'dplyr', 'readr', 'jsonlite', 'htmltools', 'httr2', 'promises', 'coro', 'withr')) stopifnot(requireNamespace(p, quietly = TRUE))"

ENV QEW_DATA_DIR=data

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860)"]
