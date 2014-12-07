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
Res <- foreach(i=(1:length(cEOIs)), .packages="tree") %dopar% {
  eoi <- cEOIs[i]
  # Create data frame with relevant features
  f_prepData(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=FALSE)
  cat(eoi, "prepping data done\n")

  # Train the model
  formula <- as.formula(paste(eoi, "~ ."))
  assign(paste0("tree_", eoi), tree(formula, data=get(eoi)[cTRAIN, ]))
  cat(eoi, "tree done\n")
  print(class(get(paste0("tree_", eoi))))
  # assign(paste0("pruned_tree_", eoi), cv.tree(get(paste0("tree_", eoi)), FUN=prune.misclass))
  # cat(eoi, "pruned tree done\n")
  assign(paste0("boosted_tree_", eoi), gbm(formula, data=get(eoi)[cTRAIN, ],
                                         distribution="bernoulli", n.trees=5000,
                                         interaction.depth=4, shrinkage=0.2, verbose=F))
  cat(eoi, "boosted tree done \n")
  cat(eoi, "training done\n")

  list(tree=get(paste0("tree_", eoi)),
       pruned_tree=get(paste0("pruned_tree_", eoi)),
       boosted_tree=get(paste0("boosted_tree_", eoi)))
}

names(Res) <- cEOIs
str(Res, maxlevel=1)
save(Res, file="../result/cart_result_nohier.RData")
cat("Result saved \n")