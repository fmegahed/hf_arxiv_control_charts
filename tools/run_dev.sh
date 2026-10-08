#!/usr/bin/env bash
# Start the app on the development dataset (Git Bash, from the app directory):
#   bash tools/run_dev.sh [port]
# Stop it with Ctrl+C. Uses the system R library, not renv.
export RENV_CONFIG_AUTOLOADER_ENABLED=FALSE
export QEW_DATA_DIR="${QEW_DATA_DIR:-data/dev_v2}"
PORT="${1:-7862}"
"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" --vanilla -e "shiny::runApp('.', port = ${PORT}, launch.browser = FALSE)"
