# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "BoomSpikeSlab", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Running the model in parallel
# cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "ic", "coup")
cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "mp")
registerDoMC(min(detectCores()/2, length(cEOIs)))
niter <- 5000 ; nburn <- 500
Res <- foreach (i=(1:length(cEOIs))) %dopar% {
  eoi <- cEOIs[i]
  # Create data frame with relevant features
  f_prepData(crisp.data, eoi, hier=TRUE, na_omit=FALSE)
  cat(eoi, "prepping data done\n")

  # Train the model
  formula <- paste(eoi, "~ .")
  assign(paste0("m_", eoi), logit.spike(formula, data=get(eoi)[cTRAIN, ], niter=niter))
  cat(eoi, "training done \n")

  # Predict in of sample
  # in_pred_prob <- apply(predict(get(paste0("m_", eoi)), newdata=get(eoi)[cTRAIN, ],
  #                      burn=nburn, type="response", na.action=na.pass), 1, mean)
  # cat(eoi, "in-sample predicting done \n")

  # Predict out of sample
  # out_pred_prob <- apply(predict(get(paste0("m_", eoi)), newdata=get(eoi)[cTEST, ],
  #                     burn=nburn, type="response", na.action=na.pass), 1, mean)
  # cat(eoi, "out-sample predicting done \n")

  # Print precision and recall
  # in_true <- get(eoi)[cTRAIN, eoi]
  # out_true <- get(eoi)[cTEST, eoi]

  list(model=get(paste0("m_", eoi)))
}

names(Res) <- cEOIs
save(Res, file="../result/logit_spike_result.RData")
