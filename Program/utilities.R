#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'
suppressMessages(library('Hmisc', quietly = T))
suppressMessages(library('dplyr', quietly = T))
options(device = 'jpeg')
# Raport function.
generateReport <- function(reportData){
  # Init raport description.
  sink(file = reportData$args$outfile, append = T)
  cat('Report generated usingmed_analize program..\n')
  cat('Author -> Miłosz Chodkowski PUT\n')
  cat('======================================================\n\n')
  sink()
  
  # Count missing data.
  sink(file = reportData$args$outfile, append = T)
  cat('In given file', reportData$n_missing, 'record/s with Not Available data.\n')
  sink()
  
  if(reportData$n_missing){
    sink(file = reportData$args$outfile, append = T)
    cat('Changes in', reportData$n_missin, 'records\n\n')
    sink()
  }
  # Short summary.
  sink(file = reportData$args$outfile, append = T)
  cat('Short summary of the given data.\n')
  cat('==================================\n')
  print(reportData$shortSummary)
  cat('==================================\n \n')
  sink()
  
  # Long format summary.
  sink(file = reportData$args$outfile, append = T)
  cat('Full summary of the given data.\n')
  cat('==================================\n')
  summariseByGroup(reportData$fullData, reportData$args$grp)
  cat('==================================\n \n')
  sink()
  
  # Write numeric data numeric data
  for(summ in reportData$numericSummary){
    write.table(summ, file = reportData$args$xlsfile, quote = F, na = '', row.names = F, append = T, sep = ';')
  }
 
  # Data significance
  sink(file = reportData$args$outfile, append = T)
  cat('Data significance according to normal distribution.\n')
  cat('==================================\n')
  nd_group_test(reportData$fullData)
  cat('==================================\n \n')
  sink()
  
  cat('Full report saved to', reportData$args$outfile, '\n')
  # REPORT END
  # =========================
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


# I know, I know ... There is summarise function in Hmisc
# But by making grouping like that, it makes writing it far more effective for me.
summariseByGroup <- function(med_data, grName){
  final <- list()
  if(grName == 'all'){
    grps <- unique(med_data[[1]])
    for(gr in grps){
      grouped <- droplevels(med_data[med_data[[1]] == gr,])
      final[[gr]] <- summary(grouped)
      print(final[[gr]])
      cat('\n**************************\n')
    }
  }else{
    grouped <- droplevels(med_data[med_data[[1]] == gr,])
    final[[gr]] <- summary(grouped)
    print(final[[gr]])
    cat('\n**************************\n')
  }
  return(final)
}

# Make a shapiro test for all numeric columns for all groups.
nd_group_test <- function(med_data){
  
  grps <- unique(med_data[[1]])
  final <- list()
  
  for(gr in grps){
    grouped <- droplevels(med_data[med_data[[1]] == gr,])
    numCols <- grouped[,unlist(lapply(grouped, is.numeric))]
    test <- sapply(numCols, shapiro.test)
    cat('In group:', gr, '\n')
    for(cl in colnames(test)){
      if(test[,cl]$p.value > 0.05){
        cat('Attribute', paste('\"', cl, '\"', sep = ''), 'not different from ND. p-value =', test[,cl]$p.value, '\n')
      }else{
        cat('Attribute', paste('\"', cl, '\"', sep = ''), 'different from ND. p-value =', test[,cl]$p.value, '\n')
      }
    }
    final[[gr]] <- test
  }
  return(final)
}

nd_group_plot <- function(med_data){
  # TODO: 
  # make density plot for all groups and parameters.
  graphics.off()
  grps <- unique(med_data[[1]])
  for(gr in grps){
    grouped <- droplevels(med_data[med_data[[1]] == gr,])
    numCols <- grouped[,unlist(lapply(grouped, is.numeric))]
    l <- lapply(colnames(numCols), function(col){
      ggpubr::ggdensity(data = numCols, x = col, title = paste(col, 'density plot'), xlab = paste(col, 'value'))
    })
    res <- suppressMessages(ggpubr::ggarrange(plotlist = l))
    jpeg(filename = paste(gr, 'density_plot.jpeg', sep = '_'), width = 1280, height = 720)
    ggpubr::annotate_figure(res, top = paste(gr, 'attributions distribution plot'))
    print(res)
    dev.off()
  }
  return(res)
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
