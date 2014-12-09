rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "tree", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

# Load the full tree
load("../result/cart_result_nohier.RData")

pdf("../writeup/fig/insurgency_tree.pdf", w=10, h=6)
par(mfrow=c(1,2))
plot(Res[["insurgency"]][["tree"]])
title(main="Insurgency: full tree")
text(Res[["insurgency"]][["tree"]], pretty=0)
plot(pruned_prediction[["insurgency"]]$pruned_tree)
text(pruned_prediction[["insurgency"]]$pruned_tree, pretty=0)
title(main="Insurgency: pruned tree")
par(mfrow=c(1,1))
dev.off()

# Load the pruned result
load("../result/cart_pruned_prediction_nohier.RData")
performance <- lapply(pruned_prediction, function(x) {
  in_performance <- f_predictiveDiagnose(x[["in_pred"]], x[["in_true"]])
  out_performance <- f_predictiveDiagnose(x[["out_pred"]], x[["out_true"]])
  list(in_performance=in_performance, out_performance=out_performance)
})

print(xtable(sapply(performance[1:4], function(x) x[[1]]), digits=3),
      file="../writeup/tab/cart_in_sample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)
print(xtable(sapply(performance[1:4], function(x) x[[2]]), digits=3),
      file="../writeup/tab/cart_out_sample.tex",
      floating=FALSE, floating.environment="center", table.placement=NULL)

# Plot

str(pruned_prediction, max.level=2)

