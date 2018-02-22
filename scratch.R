# Scratch sheet
rm(list=ls())
library(riskvisrr)
dat <- data.table(stroke1yr)
var.names <- c("age",
               "heartfailure",
               "hypertension",
               "diabetes",
               "vascular",
               "female",
               "stroke")
setkeyv(dat, var.names)
head(dat[.(20, "no", "no", "no", "no")])


returnRisk <- function(data, user_age, hf, ht, db, vasc, f, stroke){
  # Returns only matching rows
  # browser()
  head(data[.(user_age, hf, ht, db, vasc, f, stroke)])
    
}
returnRisk(data = dat, user_age = 20, hf = "no")

