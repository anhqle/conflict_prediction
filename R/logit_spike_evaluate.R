rm(list=ls())
source("functions.R")
source("logit_spike_constants.R")
f_install_and_load(c("crisp.data.package", "xtable", "ROCR", "BoomSpikeSlab", "doMC", "foreach"))

# Load prediction
load("../result/logit_spike_prediction_nohier.RData")
# registerDoMC(min(detectCores()/2, length(prediction)))
performance <- lapply(prediction, function(x) {
  in_performance <- f_predictiveDiagnose(x[["in_pred_prob"]], x[["in_true"]])
  out_performance <- f_predictiveDiagnose(x[["out_pred_prob"]], x[["out_true"]])
  list(in_performance=in_performance, out_performance=out_performance)
})

print(xtable(sapply(performance, function(x) x[[1]]), digits=3),
      file="../writeup/tab/nohierspikeslab_in_sample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)
print(xtable(sapply(performance, function(x) x[[2]]), digits=3),
      file="../writeup/tab/nohierspikeslab_out_sample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)

# ROC curve

lapply(seq_along(prediction), function(i) {
  eoi <- names(prediction)[i]

  in_pred <- prediction(prediction[[i]]$in_pred_prob, prediction[[i]]$in_true)
  in_perf <- performance(in_pred,"tpr", "fpr")
  out_pred <- prediction(prediction[[i]]$out_pred_prob, prediction[[i]]$out_true)
  out_perf <- performance(out_pred, "tpr", "fpr")
  filename <- paste0("../writeup/fig/spikeslab_roc_", eoi, ".pdf")

  pdf(filename, w=8, h=4)
  par(mfrow=c(1, 2))
  plot(in_perf, colorize=TRUE, main=paste(eoi, "In-sample"))
  plot(out_perf, colorize=TRUE, main=paste(eoi, "Out-sample"))
  par(mfrow=c(1, 1))
  dev.off()
})

# Plot variable selection result
load("../result/logit_spike_result_nohier.RData")
lapply(seq_along(Res), function(i) {
  pdf(paste0("../writeup/fig/nohierspikeslab_", names(Res)[i], "var.pdf"), w=20, h=15)
  par(mfrow=c(1, 2))
  plot(Res[[i]], burn=nburn,
       main=paste(names(Res)[i], ": all variables"),
       cex.main=2, cex.names=1.5)
  plot(Res[[i]], burn=nburn, inclusion.threshold=0.001,
       main=paste(names(Res)[i], ": variables with inclusion prob > 0.001"),
       cex.main=2, cex.names=1.5)
  par(mfrow=c(1, 1))
  dev.off()
})

