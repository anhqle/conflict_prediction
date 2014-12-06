# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "plyr", "dplyr"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

eoi <- "rebellion"
f_prepData(crisp.data, "rebellion", hier=TRUE, naOmit=FALSE)
assign(eoi, data.frame(diff(get(eoi)[ , eoi]),
                       get(eoi)[2:nrow(get(eoi)), setdiff(names(get(eoi)), eoi)]),
       envir=.GlobalEnv)