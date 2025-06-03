# This script is used to record a Shiny app session for load testing.
library(shinyloadtest)
library(shiny)
# Ensure the Shiny app is running before recording
# You can start the Shiny app in a separate R session or uncomment the next line to run it here.
# runApp("app.R")
#> Listening on http://127.0.0.1:7716

shinyloadtest::record_session("http://127.0.0.1:6687")
# output file will be in root directory