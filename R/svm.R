# Set up
rm(list=ls())
source("./R/functions.R")
f_install_and_load(c("crisp.data.package", "e1071"))

data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend
cTEST <- crisp.data$date >= cutoffs$teststart

d <- crisp.data
wantedColPosition <- grepl("^(miss)", names(crisp.data))

missing_names <- names(crisp.data)[wantedColPosition]
tmp <- gsub("missX61.", "X61", missing_names)
tmp <- gsub("(?<=[AB])\\.", "..", tmp, perl=TRUE)
tmp <- gsub("X61Z", "X61...Z", tmp, perl=TRUE)
tmp <- gsub("ZF", "ZF...", tmp)

tmp[!(tmp %in% names(crisp.data))]

# Recode 0 to NA for all the var in tmp
for (i in seq_along(missing_names)) {
  missingvar <- missing_names[i]
  var <- tmp[i]
  d[ , var] <- ifelse(d[ , missingvar]==1, NA, d[, var])
}

f_prepData(d, "dpc", hier=T)

m_all <- svm(dpc ~ AG.LND.TOTL.K2, data=dpc[cTRAIN, ], kernel="radial", gamma=1, cost=1)

