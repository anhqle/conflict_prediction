# Set up
rm(list=ls())
source("./R/functions.R")
f_install_and_load(c("spikeslab", "crisp.data.package"))

# Load data
data(crisp.data)
data(cutoffs)

# Training and Test subset
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Create data frame with relevant features
cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "ic", "coup")
for (eoi in cEOIs) {
  f_prepData(crisp.data, eoi, hier=TRUE)
}
