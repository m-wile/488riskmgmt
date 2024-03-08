packages <- c("bslib", "DT", "htmlwidgets", "plotly", "Rcpp", "RTL", "tidyquant", "tidyverse", "TTR", "zoo")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)){
  install.packages(new.packages, dependencies = TRUE)
}
