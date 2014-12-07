# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
source("logit_spike_constants.R")
f_install_and_load(c("crisp.data.package", "tree", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Load the result
load("../result/cart_result_nohier.RData")

class(Res[[1]])

prediction <- foreach (i=(1:length(Res))) %dopar% {
  eoi <- names(Res)[i]
  f_prepData(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=TRUE)
  cat(eoi, "preparing data done \n")
  in_pred_prob <- predict(model=Res[[i]], newdata=get(eoi)[cTRAIN, ])
  out_pred_prob <- predict(model=Res[[i]], newdata=get(eoi)[cTEST, ])

  in_true <- get(eoi)[cTRAIN, eoi]
  out_true <- get(eoi)[cTEST, eoi]

  return(list(in_pred_prob=in_pred_prob,
              out_pred_prob=out_pred_prob,
              in_true=in_true,
              out_true=out_true))
}
