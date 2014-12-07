rm(list=ls())
source("functions.R")
source("logit_spike_constants.R")
f_install_and_load(c("crisp.data.package", "tree", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Load the result
load("../result/cart_pruned_prediction_nohier.RData")

performance <- lapply(pruned_prediction, function(x) {
  in_performance <- f_predictiveDiagnose(x[["in_pred"]], x[["in_true"]])
  out_performance <- f_predictiveDiagnose(x[["out_pred"]], x[["out_true"]])
  list(in_performance=in_performance, out_performance=out_performance)
})

performance
