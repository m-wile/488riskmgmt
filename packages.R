packages <- c("bslib", "DT", "htmlwidgets", "plotly", "Rcpp", "RTL", "stats", "shinyWidgets", "tidyquant", "tidyverse", "TTR")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)){
  install.packages(new.packages, dependencies = TRUE)
}
