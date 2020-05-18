#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

# Raport function.
generateRaport <- function(med_data, outfile, n_missing){
  # Init raport description.
  sink(file = outfile, append = T)
  cat('Raport generated usingmed_analize program..\n')
  cat('Author -> Miłosz Chodkowski PUT\n')
  cat('======================================================\n\n')
  sink()
  
  # Count missing data.
  sink(file = outfile, append = T)
  cat('In given file', n_missing, 'record/s with Not Available data.\n\n')
  sink()
  
  if(n_missing){
    sink(file = outfile, append = T)
    cat('Changes in', n_missing, 'records\n\n')
    sink()
  }
  # Short summary.
  sink(file = outfile, append = T)
  cat('Short summary of the given data.\n')
  print(summary(med_data))
  sink()
  
  
  cat('Raport saved to', outfile, '\n')
}

# Mode function for numeric and character vectors.
getmode <- function(v) {
  uniqv <- unique(v)
  res <- uniqv[which.max(tabulate(match(v, uniqv)))]
  return(res)
}

# Imputation if neccessary -> notify user.
imputeAndNotify <- function(med_data, n_missing){
  if (n_missing)
    cat('Changes in', n_missing, 'records\n\n')
  
  for(col in colnames(med_data)){
    if(is.numeric(med_data[[col]])){
      med_data[[col]] <- Hmisc::impute(med_data[[col]], fun = mean)
    }else{
      med_data[[col]] <- Hmisc::impute(med_data[[col]], fun = getmode)
    }
  }
  return(med_data)
}