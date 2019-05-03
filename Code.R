library(dplyr)


data <- read.csv("KaggleV2-May-2016.csv")
data <- tbl_df(data)

summary(data)

