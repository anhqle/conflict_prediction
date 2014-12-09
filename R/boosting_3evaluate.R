rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "tree", "xtable"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Load the full tree
load("../result/boosting_prediction_nohier.RData")

str(boosting_prediction, max.level=2)
performance <- lapply(boosting_prediction, function(x) {
  in_performance <- f_predictiveDiagnose(x[["in_pred_prob"]], x[["in_true"]])
  out_performance <- f_predictiveDiagnose(x[["out_pred_prob"]], x[["out_true"]])
  list(in_performance=in_performance, out_performance=out_performance)
})

print(xtable(sapply(performance[1:4], function(x) x[[1]]), digits=3),
      file="../writeup/tab/boosting_insample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)
print(xtable(sapply(performance[1:4], function(x) x[[2]]), digits=3),
      file="../writeup/tab/boosting_outsample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)