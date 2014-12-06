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

source("logit_spike_constants.R")
Res <- foreach (i=(1:length(cEOIs))) %dopar% {
  eoi <- cEOIs[i]
  # Create data frame with relevant features
  f_prepData(crisp.data, eoi, hier=FALSE, naOmit=FALSE, nextMonth=FALSE)
  cat(eoi, "prepping data done\n")

  # Train the model
  formula <- paste(eoi, "~ .")
  assign(paste0("m_", eoi), logit.spike(formula, data=get(eoi)[cTRAIN, ], niter=niter))
  cat(eoi, "training done \n")

  get(paste0("m_", eoi))
}

names(Res) <- cEOIs
str(Res, maxlevel=1)
save(Res, file="../result/logit_spike_result_nohier.RData")
cat("Result saved \n")
