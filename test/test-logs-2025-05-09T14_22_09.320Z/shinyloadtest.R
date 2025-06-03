# run app in another process or deployed server
# shiny::runApp("app.R")
shinyloadtest::record_session("http://127.0.0.1:4845") # http://127.0.0.1:8600/

df <- shinyloadtest::load_runs("./test/test-logs-2025-05-09T14_22_09.320Z")
shinyloadtest::shinyloadtest_report(df, "run1.html")
