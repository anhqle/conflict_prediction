f_install_and_load <- function(packs) {
  new.packs <- packs[!(packs %in% installed.packages()[ ,"Package"])]
  lapply(new.packs, install.packages, repos="http://cran.rstudio.com/")
  lapply(packs, library, character.only=TRUE)
}

f_prepData <- function(data, eoi, header=NULL, wantedColPosition=NULL, hier=FALSE, write=FALSE, filepath="") {
  if (!is.null(header)) {
    # header is a string ("training", "testing")
    newDFName <- paste(eoi, header, sep="_")
  } else {
    newDFName <- eoi
  }

  if (is.null(wantedColPosition)) {
    # Remove identifiers and only select lags
    wantedColPosition <- grepl("^(?!ccode|cocom|crispname|iso|date|year|month|month.counter|monthID|insurgency|rebellion|dpc|erv|ic|coup).*[l0-9 ]$",
                               names(crisp.data), perl=TRUE)
  }

  # Create the new df
  assign(newDFName, data.frame( data[ , eoi], data[ , wantedColPosition]), envir=.GlobalEnv)
  # Rename first column
  assign(newDFName, setNames(get(newDFName), c(eoi, colnames(data[ , wantedColPosition]))), envir=.GlobalEnv)

  if (hier == TRUE) {
    data$country <- as.character(data$country)
    country_vars <- c()
    i <- 1
    for (country in unique(data$country)) {
      country_var <- paste("country", country, sep="_")
      country_vars[i] <- country_var
      assign(country_var, as.numeric(data$country==country))
      assign(newDFName, data.frame( get(newDFName), get(country_var) ), envir=.GlobalEnv)
      i <- i + 1
    }
    assign(newDFName, setNames(get(newDFName), c(eoi, colnames(data[ , wantedColPosition]), country_vars)), envir=.GlobalEnv)
  }

  if (write == TRUE) {
    write.csv(get(newDFName), paste0(filepath, newDFName, ".csv"), row.names=FALSE)
  }
  cat(newDFName, "done \n")
}