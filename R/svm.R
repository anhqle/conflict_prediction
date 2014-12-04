# Set up
rm(list=ls())
source("functions.R")
f_install_and_load(c("crisp.data.package", "e1071"))

start <- proc.time()

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

# tmp[!(tmp %in% names(crisp.data))]

# Recode 0 to NA for all the var in tmp
for (i in seq_along(missing_names)) {
  missingvar <- missing_names[i]
  var <- tmp[i]
  d[ , var] <- ifelse(d[ , missingvar]==1, NA, d[, var])
}

f_prepData(d, "dpc", hier=T)
dpc <- dpc[, setdiff(names(dpc), tmp)]

# Remove everything except dpc since for some reasons svm looks for X...SF object sth like that
rm(list=(setdiff(ls(), c("dpc", "cTRAIN"))))

m_all <- svm(dpc ~ ., data=dpc[cTRAIN, ], kernel="radial", gamma=1, cost=1)
plot(m_all, dpc[cTRAIN, ])

tune_all <- tune(svm, dpc ~ ., data=dpc[cTRAIN, ], kernel="radial",
                 ranges=list(cost=c(0.1, 1),
                             gamma=c(0.5, 1))
                 )
summary(tune_all)

# table(true=dpc[-cTRAIN, dpc],
#      pred=predict(tune_all$best.model, newx=dpc[-cTRAIN, ]))

save.image(file="svm_firstrun.RData")

proc.time() - start
