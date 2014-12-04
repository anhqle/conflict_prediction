rm(list=ls())
source("functions.R")
f_install_and_load(c("spikeSlabGAM", "crisp.data.package", "dplyr"))

# Load data
data(crisp.data)
data(cutoffs)
cTRAIN <- crisp.data$date <= cutoffs$trainingend

# Run MCMC
options(mc.cores=6)
mcmc <- list(nChains=8, chainLength=1000, burnin=500, thin=5)

cEOIs <- c("insurgency")
for (eoi in cEOIs) {
  f_prepData(crisp.data, eoi, hier=FALSE) # Create data frame with relevant features
  df <- get(eoi)


  # Train the model
  formula <- as.formula(paste(eoi, "~", paste(names(df)[-1], collapse=" + ")))
  assign(paste0("model_", eoi), spikeSlabGAM(formula=formula,
                                             family="binomial", data=df[cTRAIN, ],
                                             mcmc=mcmc))
  cat(eoi, "training done \n")
  # Predict new value. Data for predict excludes response y
  assign(paste0("pred_", eoi), predict(get(paste0("model_", eoi)),
                                       newdata=df[-cTRAIN, ]))
  cat(eoi, "predicting done \n")

  # Print precision and recall
  table(real=df[-cTRAIN, eoi], pred=as.numeric(paste0("pred_", eoi) >= 0.5))
}

save.image()


