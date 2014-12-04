# Set up
rm(list=ls())
source("./R/functions.R")
f_install_and_load(c("e1071", "crisp.data.package"))

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

system.time(
  tune.out <- tune(svm, coup ~ ., data=coup[cTRAIN, ], kernel="radial", scale=TRUE,
                 ranges = list(cost = c(10, 100),
                               gamma = c(2, 3)))
)
summary(tune.out)

# Predict
table(true=coup[-cTRAIN, "y"], pred=predict(tune.out$best.model, newx=coup[-cTRAIN, ]))
