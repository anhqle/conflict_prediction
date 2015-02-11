rm(list=ls())
source("functions.R")
f_install_and_load("xtable")

load("../result/boosting_result_nohier.RData")

# Get the contribution
contributions <- lapply(Res, function(x) head(x$boosted_tree$contributions, n=10))

for (i in seq_along(contributions)) {
  print(xtable(contributions[[i]], digits=3),
        file=paste0("../writeup/tab/boosting_contributions_", names(contributions)[i], ".tex"),
        include.rownames=FALSE,
        floating=FALSE, floating.environment="center", table.placement=NULL)
}

