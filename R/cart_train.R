# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "tree", "gbm", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart
# Running the model in parallel
# cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "ic", "coup")
cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "mp")
registerDoMC(min(detectCores()/2, length(cEOIs)))
Res <- foreach(i=(1:length(cEOIs)), .export="f_prepDataLocal") %dopar% {
  eoi <- cEOIs[i]
  # Create data frame with relevant features
  data <- f_prepDataLocal(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=FALSE)
  traindata <- na.omit(data[cTRAIN, ])
  cat(eoi, "prepping data done\n")

  # Train the model
  formula <- as.formula(paste(eoi, "~ ."))

  grown_tree <- tree(formula, data=traindata)
  cat(eoi, "tree done\n")

  pruned_tree <- cv.tree(grown_tree, FUN=prune.misclass)
  cat(eoi, "pruned tree done\n")

  boosted_tree <- gbm(formula, data=traindata,
                       distribution="bernoulli", n.trees=5000,
                       interaction.depth=4, shrinkage=0.2, verbose=F)
  cat(eoi, "boosted tree done \n")

  list(grown_tree=grown_tree, pruned_tree=pruned_tree, boosted_tree=boosted_tree)
}

names(Res) <- cEOIs
str(Res, maxlevel=1)
save(Res, file="../result/cart_result_nohier.RData")
cat("Result saved \n")