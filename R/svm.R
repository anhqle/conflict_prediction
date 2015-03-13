rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "e1071", "doMC", "foreach"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

data <- f_prepDataLocal(crisp.data, "insurgency", hier=FALSE, naOmit=FALSE, nextMonth=FALSE)

m_all <- svm(insurgency ~ ., data=data[cTRAIN, ], kernel="radial", gamma=1, cost=1)
cat("Single model done\n")

tune_all <- tune(svm, insurgency ~ ., data=data[cTRAIN, ], kernel="radial",
                 ranges=list(cost=c(0.1, 1),
                             gamma=c(0.5, 1))
)
cat("Tuning done\n")

save(m_all, tune_all, file="../result/svm_firstrun.RData")