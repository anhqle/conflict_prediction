f_install_and_load <- function(packs) {
  new.packs <- packs[!(packs %in% installed.packages()[ ,"Package"])]
  lapply(new.packs, install.packages, repos="http://cran.rstudio.com/", dependencies=TRUE)
  lapply(packs, library, character.only=TRUE)
}

f_prepData <- function(data, eoi, header=NULL, wantedColPosition=NULL, hier=FALSE,
                       write=FALSE, naOmit=TRUE, nextMonth=FALSE, filepath="") {
  if (!is.null(header)) {
    # header is a string ("training", "testing")
    newDFName <- paste(eoi, header, sep="_")
  } else {
    newDFName <- eoi
  }

  if (is.null(wantedColPosition)) {
    # Remove identifiers and only select lags
    wantedColPosition <- grepl("^(?!ccode|cocom|crispname|iso|date|year|month|month.counter|monthID|insurgency|rebellion|dpc|erv|ic|coup|mp).*[l0-9 ]$",
                               names(crisp.data), perl=TRUE)
  }

  # Create the new DF
  # Predict nextMonth instead of current month
  if (nextMonth == TRUE) {
    assign(newDFName, data.frame( data[2:nrow(data), eoi],
                                  data[1:(nrow(data)-1), wantedColPosition]), envir=.GlobalEnv)
  } else {
    assign(newDFName, data.frame( data[ , eoi],
                                  data[ , wantedColPosition]), envir=.GlobalEnv)
  }

  # Rename first column
  assign(newDFName, setNames(get(newDFName), c(eoi, colnames(data[ , wantedColPosition]))), envir=.GlobalEnv)

  # Convert integer to numeric
  integerToNumeric <- function(d) {
    modifyList(d, lapply(d[ , sapply(d, is.integer)], as.numeric))
  }
  assign(newDFName, integerToNumeric(get(newDFName)), envir=.GlobalEnv)

  if (hier == TRUE) {
    data$country <- gsub(pattern=",|-", rep="_", as.character(data$country))
    country_vars <- rep(NA, length(unique(data$country)))
    i <- 1
    for (country in unique(data$country)) {
      country_var <- paste("country", country, sep="_")
      country_vars[i] <- country_var
      assign(country_var, as.numeric(data$country==country))
      if (nextMonth == FALSE) {
        assign(newDFName, data.frame( get(newDFName), get(country_var) ), envir=.GlobalEnv)
      } else {
        assign(newDFName, data.frame( get(newDFName), get(country_var)[1:(nrow(data)-1)] ), envir=.GlobalEnv)
      }
      i <- i + 1
    }
    assign(newDFName, setNames(get(newDFName), c(eoi, colnames(data[ , wantedColPosition]), country_vars)), envir=.GlobalEnv)
  }
  if (naOmit == TRUE) {
    assign(newDFName, na.omit(get(newDFName)), envir=.GlobalEnv)
  }

  if (write == TRUE) {
    write.csv(get(newDFName), paste0(filepath, newDFName, ".csv"), row.names=FALSE)
  }

  cat(newDFName, "done \n")
}

f_brier <- function(pred_prob, true) mean((pred_prob - true)^2, na.rm=TRUE)
f_auc <- function(pred_prob, true) somers2(pred_prob, true)["C"]
f_precision <- function(pred, true) sum(pred == 1 & true == 1, na.rm=TRUE) / sum(pred == 1, na.rm=TRUE)
f_recall <- function(pred, true) sum(pred == 1 & true == 1, na.rm=TRUE) / sum(true == 1, na.rm=TRUE)

f_predictiveDiagnose <- function(pred_prob, true) {
  pred <- ifelse(pred_prob >= 0.5, 1, 0)
  return(c(brier=f_brier(pred_prob, true),
           auc=f_auc(pred_prob, true),
           precision=f_precision(pred, true),
           recall=f_recall(pred, true)))
}

f_baselineComparison <- function(x, y){
  .miss <- is.na(x)
  .nObsThis <- sum(!.miss)
  .baseModelThis <- rep(0, .nObsThis)
  .xThis <- x[!.miss]
  .yThis <- y[!.miss]
  num.wrong <- .nObsThis-sum((.xThis>.5)*.yThis + (.xThis<.5)*(1-.yThis))
  baseline.wrong <- .nObsThis-sum(.baseModelThis==.yThis)
  (baseline.wrong - num.wrong)/baseline.wrong
}

f_roc <- function(model, data, eoi, cTRAIN=cTRAIN, cTEST=cTEST) {
  in_pred_prob <- apply(predict(model, newdata=data[cTRAIN, ],
                                burn=100, type="response", na.action=na.pass), 1, mean)
  cat(eoi, "in-sample predicting done \n")

  # Predict out of sample
  out_pred_prob <- apply(predict(model, newdata=data[cTEST, ],
                                 burn=100, type="response", na.action=na.pass), 1, mean)
  cat(eoi, "out-sample predicting done \n")

  # Print precision and recall
  in_true <- data[cTRAIN, eoi]
  out_true <- data[cTEST, eoi]

  pred <- prediction(ifelse(in_pred_prob >= 0.5, 1, 0), in_true)
  perf <- performance(pred,"tpr", "fpr")
  plot(perf,colorize = TRUE)
}