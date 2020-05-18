#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'
suppressMessages(library('Hmisc', quietly = T))
suppressMessages(library('dplyr', quietly = T))

# Raport function.
generateReport <- function(med_data, outfile, n_missing){
  # Init raport description.
  sink(file = outfile, append = T)
  cat('Report generated usingmed_analize program..\n')
  cat('Author -> Miłosz Chodkowski PUT\n')
  cat('======================================================\n\n')
  sink()
  
  # Count missing data.
  sink(file = outfile, append = T)
  cat('In given file', n_missing, 'record/s with Not Available data.\n')
  sink()
  
  if(n_missing){
    sink(file = outfile, append = T)
    cat('Changes in', n_missing, 'records\n\n')
    sink()
  }
  # Short summary.
  sink(file = outfile, append = T)
  cat('Short summary of the given data.\n')
  cat('==================================\n')
  print(summary(med_data))
  cat('==================================\n \n')
  sink()
  
  # Long summary by groups.
  sink(file = outfile, append = T)
  cat('Summary by possible groups\n')
  cat('==================================\n')
  summariseNumericByCharacter(med_data)
  cat('==================================\n')
  sink()
  
  cat('Full report saved to', outfile, '\n')
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
      med_data[[col]] <- impute(med_data[[col]], fun = mean)
    }else{
      med_data[[col]] <- impute(med_data[[col]], fun = getmode)
    }
  }
  return(med_data)
}

summariseNumeric <- function(med_data, grName){
  for(cl in colnames(med_data)){
    if(is.numeric(med_data[[cl]]) && cl != grName){
      summ <- group_by(med_data, eval(parse(text = grName))) %>% summarise(
        count = n(),
        mean = round(mean(eval(parse(text = cl))), 2),
        sd = round(sd(eval(parse(text = cl))), 2),
        median = round(median(eval(parse(text = cl))), 2),
        var = round(var(eval(parse(text = cl))), 2))
      ungroup(med_data)
      
      res <- as.data.frame(summ)
      colnames(res)[1] <- grName
      
      cat('*', cl, '*\n')
      print(res)
      cat('\n')
    }
  }
}

summariseNumericByCharacter <- function(med_data){
  for(x in colnames(med_data)){
    if(is.character(as.vector(med_data[[x]]))){
      cat('---> Grouped by:', x, '\n')
      summariseNumeric(med_data, x)
    }
  }
}

