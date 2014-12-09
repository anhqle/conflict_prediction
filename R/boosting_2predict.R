rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "gbm", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

load("../result/boosting_result_nohier.RData")

registerDoMC(min(detectCores()/2, length(Res)))
boosting_prediction <- foreach (i=(1:length(Res))) %dopar% {
  eoi <- names(Res)[i]
  f_prepData(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=TRUE)
  cat(eoi, "preparing data done \n")

  boosted_tree <- Res[[i]]$boosted_tree
  in_pred_prob <- predict(boosted_tree, newdata=get(eoi)[cTRAIN, ],
    n.trees=boosted_tree$gbm.call$best.trees, type="response")
  out_pred_prob <- predict(boosted_tree, newdata=get(eoi)[cTEST, ],
    n.trees=boosted_tree$gbm.call$best.trees, type="response")
  cat(eoi, "predicting done\n")

  in_true <- as.numeric(as.character(get(eoi)[cTRAIN, eoi]))
  out_true <- as.numeric(as.character(get(eoi)[cTEST, eoi]))

  return(list(in_pred_prob=in_pred_prob,
              out_pred_prob=out_pred_prob,
              in_true=in_true,
              out_true=out_true))
}

names(boosting_prediction) <- names(Res)
str(boosting_prediction)
save(boosting_prediction, file="../result/boosting_prediction_nohier.RData")
cat("result saved\n")