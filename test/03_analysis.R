library(shinyloadtest)
library(lubridate)
# install.packages("lubridate")
df <- load_runs("./test/replay2")
# html report
shinyloadtest_report(df, "./test/replay2/report_dplyr.html")
