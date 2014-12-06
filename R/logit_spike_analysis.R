# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
source("logit_spike_constants.R")
f_install_and_load(c("crisp.data.package", "BoomSpikeSlab", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Load the result
load("../result/logit_spike_result_nextMonth.RData")

f_predict <- function(model, newdata, nburn) {
  apply(predict(model, newdata=newdata,
    burn=nburn, type="response", na.action=na.pass), 1, mean)
}

registerDoMC(min(detectCores()/2, length(Res)))

prediction <- foreach (i=(1:length(Res))) %dopar% {
  eoi <- names(Res)[i]
  f_prepData(crisp.data, eoi, hier=TRUE, naOmit=FALSE, nextMonth=TRUE)
  cat(eoi, "preparing data done \n")
  in_pred_prob <- f_predict(model=Res[[i]], newdata=get(eoi)[cTRAIN, ], nburn=nburn)
  out_pred_prob <- f_predict(model=Res[[i]], newdata=get(eoi)[cTEST, ], nburn=nburn)

  in_true <- get(eoi)[cTRAIN, eoi]
  out_true <- get(eoi)[cTEST, eoi]

  return(list(in_pred_prob=in_pred_prob,
              out_pred_prob=out_pred_prob,
              in_true=in_true,
              out_true=out_true))
}

names(prediction) <- names(Res)
save(prediction, file="../result/logit_spike_prediction_nextMonth.RData")