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
cEOIs <- c("insurgency", "rebellion", "dpc", "erv")
registerDoMC(min(detectCores()/2, length(cEOIs)))
Res <- foreach (i=(1:length(cEOIs))) %dopar% {
  eoi <- cEOIs[i]
  # Create data frame with relevant features
  f_prepData(crisp.data, eoi, hier=TRUE, na_omit=FALSE)
  cat(eoi, "prepping data done\n")

  # Train the model
  formula <- paste(eoi, "~ .")
  assign(paste0("m_", eoi), logit.spike(formula, data=get(eoi)[cTRAIN, ], niter=1000))
  cat(eoi, "training done \n")

  # Predict in of sample
  in_pred_prob <- apply(predict(get(paste0("m_", eoi)), newdata=get(eoi)[cTRAIN, ],
                        burn=100, type="response", na.action=na.pass), 1, mean)
  cat(eoi, "in-sample predicting done \n")

  # Predict out of sample
  out_pred_prob <- apply(predict(get(paste0("m_", eoi)), newdata=get(eoi)[cTEST, ],
                       burn=100, type="response", na.action=na.pass), 1, mean)
  cat(eoi, "out-sample predicting done \n")

  # Print precision and recall
  in_true <- get(eoi)[cTRAIN, eoi]
  out_true <- get(eoi)[cTEST, eoi]

  # table(true=true, pred=pred)
  # in_performance <- f_predictiveDiagnose(in_pred_prob, in_true)
  # out_performance <- f_predictiveDiagnose(out_pred_prob, out_true)
  # cat(eoi, "predictive diagnostic done \n")

  list(model=get(paste0("m_", eoi)))
}

names(Res) <- cEOIs
save(Res, file="result_logit_spike_parallel.RData")
