# cd to the correct folder
if (Sys.getenv("LOGNAME") == "anh") {
  setwd("~/projects/conflict_prediction/R/")
}

rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "xtable", "ROCR", "BoomSpikeSlab", "doMC", "foreach"))
load("result_logit_spike_parallel.RData")

cEOIs <- c("insurgency", "rebellion", "dpc", "erv")
names(Res) <- cEOIs

print(xtable(sapply(Res, function(l) l[[1]]), digits=3),
      file="../poster/in_sample_performance.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)
print(xtable(sapply(Res, function(l) l[[2]]), digits=3),
      file="../poster/out_sample_performance.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)

# ROC curve
data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

registerDoMC(length(cEOIs))
cEOIplot <- c("insurgency", "dpc")

foreach (i=1:length(cEOIplot)) %dopar% {
  eoi <- cEOIplot[i]
  f_prepData(crisp.data, eoi, hier=TRUE, na_omit=FALSE)
  model <- Res[[eoi]][[3]]
  in_pred_prob <- apply(predict(model, newdata=get(eoi)[cTRAIN, ],
                                burn=100, type="response", na.action=na.pass), 1, mean)
  out_pred_prob <- apply(predict(model, newdata=get(eoi)[cTEST, ],
                                 burn=100, type="response", na.action=na.pass), 1, mean)
  in_true <- get(eoi)[cTRAIN, eoi]
  out_true <- get(eoi)[cTEST, eoi]

  in_pred <- prediction(in_pred_prob, in_true)
  in_perf <- performance(in_pred,"tpr", "fpr")

  out_pred <- prediction(out_pred_prob, out_true)
  out_perf <- performance(out_pred, "tpr", "fpr")

  filename <- paste0("../poster/figures/roc_", eoi, ".pdf")
  pdf(filename, w=8, h=4)
  par(mfrow=c(1, 2))
  plot(in_perf, colorize=TRUE, main="In-sample")
  plot(out_perf, colorize=TRUE, main="Out-sample")
  par(mfrow=c(1, 1))
  dev.off()
}

pdf("../poster/figures/variable_selection.pdf", w=7, h=4)
plot(Res[["insurgency"]][[3]], burn=100, inclusion.threshold=0.001,
     main="Insurgency", cex.names=0.7)
dev.off()
