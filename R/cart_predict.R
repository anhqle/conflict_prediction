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

# ---- Predict ----
# registerDoMC(min(detectCores()/2, length(Res)))
pruned_prediction <- vector("list", length(Res))
for (i in seq_along(Res)) {
  eoi <- names(Res)[i]
  data <- f_prepDataLocal(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=TRUE)
  traindata <- na.omit(data[cTRAIN, ])
  testdata <- na.omit(data[cTEST, ])
  print(ls())
  cat("prep data done\n")

  full_tree <- Res[[i]][["tree"]]
  cv_tree <- cv.tree(full_tree, FUN=prune.misclass)
  cat("cross validation done\n")
  optimal_size <- cv_tree$size[which(cv_tree$dev == min(cv_tree$dev))]
  if (length(optimal_size) > 1) {
    optimal_size <- optimal_size[1]
  }
  print(optimal_size)

  pruned_tree <- prune.misclass(full_tree, best=optimal_size)
  cat("pruning done \n")

  in_pred <- predict(pruned_tree, newdata=traindata, type="class")
  out_pred <- predict(pruned_tree, newdata=testdata, type="class")
  cat("predicting done \n")

  in_true <- traindata[ , eoi]
  out_true <- testdata[ , eoi]

  pruned_prediction[[i]] <- list(in_pred=in_pred, out_pred=out_pred,
                                 in_true=in_true, out_true=out_true)
}

names(pruned_prediction) <- names(Res)

str(pruned_prediction)
save(pruned_prediction, file="../result/cart_pruned_prediction_nohier.RData")