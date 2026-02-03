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

# Copy application code (changes more frequently)
COPY app.R app.R
COPY data/ data/
COPY www/ www/

# Verify critical package is available
RUN R -e "stopifnot(requireNamespace('shiny', quietly = TRUE))"

EXPOSE 7860

CMD ["R", "--quiet", "-e", "shiny::runApp('/app', host='0.0.0.0', port=7860)"]
