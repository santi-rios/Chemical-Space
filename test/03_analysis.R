library(shinyloadtest)
library(lubridate)
# install.packages("lubridate")
df <- load_runs("./test/replay1")
# html report
shinyloadtest_report(df, "report.html")
