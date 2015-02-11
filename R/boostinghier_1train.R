rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "dismo", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Train the model
cEOIs <- c("insurgency", "rebellion", "dpc", "erv", "mp")
registerDoMC(min(detectCores()/2, length(cEOIs)))
Res <- foreach(i=(1:length(cEOIs))) %dopar% {
  eoi <- cEOIs[i]
  data <- f_prepDataLocal(crisp.data, eoi, hier=TRUE, naOmit=FALSE, nextMonth=FALSE)
  cat(eoi, "prepping data done\n")
  traindata <- na.omit(data[cTRAIN, ])
  traindata[,1] <- as.numeric(as.character(traindata[,1]))

  cat(eoi, "start training\n")
  boosted_tree <- gbm.step(data=traindata, gbm.x=2:ncol(traindata), gbm.y=1,
                           family="bernoulli", tree.complexity=1,
                           learning.rate=0.01, bag.fraction=0.75)
  cat(eoi, "done training\n")

  list(boosted_tree=boosted_tree)
}
names(Res) <- cEOIs
str(Res, maxlevel=1)
save(Res, file="../result/boosting_result_nohier.RData")
cat("Result saved \n")