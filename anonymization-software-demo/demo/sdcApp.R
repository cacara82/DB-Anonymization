library(sdcMicro)
wd <- "anonymization-software-demo/data"
setwd(wd)
file_name <- "adult.csv"
adult <- read.csv(file_name, na.strings = " ?", stringsAsFactors = TRUE)
x <- sdcApp()
