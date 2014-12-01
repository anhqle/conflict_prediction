# Set up
rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "BoomSpikeSlab"))

data(crisp.data)
data(cutoffs)

d <- crisp.data

f_prepData(d, "insurgency", hier=TRUE)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

m_fit <- logit.spike(insurgency ~ ., data=insurgency[cTRAIN, ],
                     niter=5000, nthreads=6)
save(m_fit, file="logit_spike.RData")
summary(m_fit)
plot(m_fit)

summary(m_fit)["reb.l.count.one.l1" , ]
mean(m_fit$beta[ , "reb.l.count.one.l1"])
mean(m_fit$beta[ , "reb.l.count.one.l1"] != 0)

pred_prob <- predict(m_fit, newdata=insurgency[cTEST, ], burn=1000, type="response")
pred <- ifelse(apply(pred_prob, 1, mean) >= 0.5, 1, 0)
true <- insurgency[cTEST, "insurgency"]

table(true=true, pred=pred)
brier <- f_brier(pred_prob, true)
auc <- somers2(pred, true)["C"]
precision <- sum(pred == 1 & true == 1) / sum(pred == 1)
recall <- sum(pred == 1 & true == 1) / sum(true == 1)

f_predictiveDiagnose(pred_prob, true)
