# Set up
rm(list=ls())
source("functions.R")
f_install_and_load(c("spikeslab", "crisp.data.package"))

# Load data
data(crisp.data)
data(cutoffs)

# Training and Test subset
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Do it
cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "ic", "coup")
for (eoi in cEOIs) {
  f_prepData(crisp.data, eoi, hier=TRUE) # Create data frame with relevant features
  formula <- paste(eoi, "~ .")
  # Train the model
  assign(paste0("model_", eoi), spikeslab(formula, data=get(eoi)[cTRAIN, ]))
  cat(eoi, "training done \n")
  # Predict new value
  assign(paste0("pred_", eoi), predict(get(paste0("model_", eoi)), data=get(eoi)[cTEST, ]))
  cat(eoi, "predicting done \n")

  # Print precision and recall
  table(get(paste0("pred_", eoi))$yhat.gnet, get(eoi)[ , eoi])
}
