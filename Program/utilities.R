#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

suppressMessages(library('Hmisc', quietly = T))
suppressMessages(library('dplyr', quietly = T))
suppressMessages(library('ggpubr', quietly = T))
options(device = 'jpeg', warn = -1)

# Report function.
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
  cat('ATTENTION!.\n')
  cat('==================================\n')
  cat('Full summary of the given data is a very big table. It has been saved in', reportData$args$xlsfile, '\n')
  cat('==================================\n \n')
  sink()
  
  # Write full summary to xls.
  write.table(reportData$fullSummary, file = reportData$args$xlsfile, quote = F, na = '', row.names = F, sep = ';')
  
  # Number of outliers.
  sink(file = reportData$args$outfile, append = T)
  cat('Number of outliers in given data sets\n')
  cat('==================================\n')
  cat(reportData$outliersStr, sep = '\n')
  cat('==================================\n')
  sink()
  
  # Data significance.
  sink(file = reportData$args$outfile, append = T)
  cat('Data significance according to normal distribution.\n')
  cat('==================================\n')
  cat(reportData$dataSignificanceStr, sep = '\n')
  cat('==================================\n \n')
  sink()
  
  # Stat analysis.
  # TODO
  
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


# Summarise every attribute.
summariseAllData <- function(med_data){
  grCol <- as.name(colnames(med_data)[1])
  res <- med_data %>% group_by(!!grCol) %>% summarise_if(.predicate = is.numeric, .funs = list(
    median = median,
    mean = mean,
    var = var, 
    min = min, 
    max = max, 
    sd = sd, 
    p.val = function(x) return(shapiro.test(x)$p.value))
  ) 
  return(res)
}

# Make a shapiro test for all numeric columns for all groups.
report_data_significance <- function(attrs, summarisedData){
  grps <- unique(summarisedData[[1]])
  for(gr in grps){
    cat('In group:', gr, '\n')
    df <- summarisedData %>% filter(.[[1]] == gr)
    for(a in attrs){
      attr <- paste(a,'_p.val', sep = '')
      if(df[[attr]] > 0.05){
        cat(sprintf('Attribute %s not different from ND. p.value: %g \n', a, df[[attr]]))
      }else{
        cat(sprintf('Attribute %s different from ND. p.value: %g \n', a, df[[attr]]))
      }
    }
  }
}

# plot densities of attributes by group
nd_group_plot <- function(med_data, path){
  # make density plot for all groups and parameters.
  grCol <- as.name(colnames(med_data)[1])
  graphics.off()
  
  med_data %>% group_by(!!grCol) %>% do(... = {
    gr <- as.character(unique(.[[1]]))
    selected <- as.data.frame(select_if(., is.numeric))
    l <- lapply(colnames(selected), function(col){
      ggpubr::ggdensity(data = selected, x = col, title = paste(col, 'density plot'), xlab = paste(col, 'value'))
    })
    res <- suppressMessages(ggpubr::ggarrange(plotlist = l))
    jpeg(filename = file.path(path, paste(gr, 'density_plot.jpeg', sep = '_')), width = 1280, height = 720)
    ggpubr::annotate_figure(res, top = paste(gr, 'attributions distribution plot'))
    print(res)
    dev.off()
  })
}

# plot boxplots by group
grouped_box_plot <- function(med_data, path){
  graphics.off()
  grCol <- as.name(colnames(med_data)[1])
  cat('**Plotting data** ...\n')
  med_data %>% group_by(!!grCol) %>% do(... = {
    grName <- as.character(unique(.[[1]]))
    flpath = file.path(path, paste(paste(grName, 'boxplot', sep = '_'), '.jpeg', sep = ''))
    jpeg(file = flpath, width = 1280, height = 720)
    dataf <- as.data.frame(select_if(., is.numeric))
    for(i in seq(ncol(dataf))){
      dataf[,i] <- as.numeric(dataf[,i])
    }
    boxplot(dataf)
    dev.off()
    })
}

analize_two <- function(med_data){
  # TODO
  gr <- as.name(colnames(med_data)[1])
  res <- med_data %>% group_by(!!grCol)
  return(res)
}
    
analize_multiple <- function(){
  # TODO
}

outliers <- function(vec){
  qnt <- quantile(vec)
  H <- 1.5 * IQR(vec)
  res <- vec
  res <- res[res < (qnt[1] - H) | res > (qnt[2] + H)]
  return(res)
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
