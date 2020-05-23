#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

suppressMessages(library('Hmisc', quietly = T))
suppressMessages(library('dplyr', quietly = T))
suppressMessages(library('ggpubr', quietly = T))
suppressMessages(library('stargazer', quietly = T))
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
  stargazer(reportData$fullData, summary = T, type = 'text')
  cat('\n\n')
  sink()
  
  # Long format summary.
  sink(file = reportData$args$outfile, append = T)
  cat('ATTENTION! Full summary is also saved in', reportData$args$xlsfile, '\n')
  cat('\nFull summary of numeric data.\n')
  for(gr in names(reportData$fullSummary)){
    cat(sprintf('Group %s:\n', gr))
    for(df in names(reportData$fullSummary[[gr]])){
      cat(sprintf('Attribute %s stats:', df))
      stargazer(as.data.frame(reportData$fullSummary[[gr]][[df]][,-1]), type = 'text', summary = F, rownames = F)
      cat('\n')
    }
  }
  cat('\n\n')
  sink()
  
  # Write full summary to xls.
  write.table(reportData$fullSummary, file = reportData$args$xlsfile, quote = F, na = '', row.names = F, sep = ';')
  
  # Number of outliers.
  sink(file = reportData$args$outfile, append = T)
  cat('Number of outliers in given data sets\n')
  cat('==================================\n')
  cat(reportData$outliersStr, sep = '\n')
  cat('\nFor more details -> check boxplots in', reportData$args$plotDir, 'folder.\n\n')
  sink()
  
  # Data significance.
  sink(file = reportData$args$outfile, append = T)
  cat('Data significance according to normal distribution.\n')
  cat('==================================\n')
  report_data_significance(reportData$fullSummary)
  cat('\nFor more details -> check density plots in', reportData$args$plotDir, 'folder.\n\n')
  sink()
  
  # Stat analysis.
  sink(file = reportData$args$outfile, append = T)
  cat('Statistical differences between groups:\n')
  cat('Tests and p-value results')
  stargazer(reportData$analysis %>% as.data.frame, type = 'text', summary = F, rownames = F)
  cat('\nInterpretation: \n')
  cat(reportData$analysisStr, sep = '\n')
  cat('\n\n')
  sink()
  
  # REPORT END
  cat('Full report saved to', reportData$args$outfile, '\n')
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
  res_list <- list()
  grps <- unique(med_data[[1]])
  grCol <- as.name(colnames(med_data)[1])
  for(gr in grps){
    filtered <- med_data %>% group_by(!!grCol) %>% subset(.[[1]] == gr)
    res_list[[gr]] <- list()
    for(attr in colnames(filtered %>% as.data.frame %>% select_if(is.numeric))){
      numerics <- filtered %>% select_if(is.numeric)
      nm <- as.name(attr)
      res_list[[gr]][[attr]] <- numerics %>% summarise(median = median(!!nm),
                                                         mean = mean(!!nm), 
                                                         var = var(!!nm), 
                                                         min = min(!!nm), 
                                                         max = max(!!nm), 
                                                         sd = sd(!!nm),
                                                         p.val = shapiro.test(!!nm)$p.value)
    } 
  }
  return(res_list)
}

# Make a shapiro test for all numeric columns for all groups.
report_data_significance <- function(summarisedData){
  for(gr in names(summarisedData)){
    cat(sprintf('Group %s:\n', gr))
    cat('Attribute: \n')
    for(df in names(summarisedData[[gr]])){
      if(summarisedData[[gr]][[df]]$p.val > 0.05){
        cat(sprintf('\t%s: not different from ND. p.value: %e \n', df, summarisedData[[gr]][[df]]$p.val))
      }else{
        cat(sprintf('\t%s: different from ND. p.value: %e \n', df, summarisedData[[gr]][[df]]$p.val))
      }
    }
    cat('\n')
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
      ggpubr::ggdensity(data = selected, x = col, title = paste(col, 'density plot'), xlab = 'Value')
    })
    res <- suppressMessages(ggpubr::ggarrange(plotlist = l))
    jpeg(filename = file.path(path, paste(gr, 'density_plot.jpeg', sep = '_')), width = 1280, height = 720)
    ggpubr::annotate_figure(res, top = paste(gr, 'attributions density plot'))
    print(res)
    dev.off()
  })
}

# plot boxplots by group
grouped_box_plot <- function(med_data, path){
  graphics.off()
  grCol <- as.name(colnames(med_data)[1])
  med_data %>% group_by(!!grCol) %>% do(... = {
    grName <- as.character(unique(.[[1]]))
    flpath = file.path(path, paste(paste(grName, 'boxplot', sep = '_'), '.jpeg', sep = ''))
    jpeg(file = flpath, width = 1280, height = 720)
    dataf <- as.data.frame(select_if(., is.numeric))
    for(i in seq(ncol(dataf))){
      dataf[,i] <- as.numeric(dataf[,i])
    }
    boxplot(dataf, col = sample(colors(), ncol(dataf)), main = paste(grName, 'group', 'boxplot'), main.col = 'seagreen', ylab = 'Value')
    dev.off()
    })
}

analize_two <- function(med_data, summarisedData){
  attrs <- colnames(med_data %>% as.data.frame %>% select_if(is.numeric))
  grps <- unique(med_data[[1]])
  df <- data.frame(setNames(rep(list(rep(NA, 2)), length(attrs) + 1), c('XXX', attrs)))
  df[['XXX']] <- c('p-value', 'Test')
  
  cat('Attribute: \n')
  for(attr in attrs){
    eq_nd <- equal_nd(grps[[1]], grps[[2]], attr, summarisedData)
    eq_var <- car::leveneTest(med_data[[attr]] ~ med_data[[1]])$`Pr(>F)`[1] > 0.05
    if(eq_nd & eq_var){
      t <- t.test(med_data[[attr]] ~ med_data[[1]], var.equal = TRUE)$p.value
      if(t > 0.05){
        cat(sprintf('\t%s: t-test has shown no statictical differences.\n', attr))
      }else{
        cat(sprintf('\t%s: t-test has shown large statictical differences.\n', attr))
      }
      df[[attr]] <- c(formatC(t, format = 'e'), 'Student\'s t-test')
    }else if(eq_nd & !eq_var){
      t <- t.test(med_data[[attr]] ~ med_data[[1]], var.equal = FALSE)$p.value
      if(t > 0.05){
        cat(sprintf('\t%s: Welch test has shown no statictical differences.\n', attr))
      }else{
        cat(sprintf('\t%s: Welch test has shown large statictical differences.\n', attr))
      }
      df[[attr]] <- c(formatC(t, format = 'e'), 'Welch test')
    }else{
      t <- wilcox.test(med_data[[attr]] ~ med_data[[1]])$p.value
      if( t > 0.05){
        cat(sprintf('\t%s: wilcoxon test has shown no statictical differences.\n', attr))
      }else{
        cat(sprintf('\t%s: wilcoxon test has shown large statictical differences.\n', attr))
      }
      df[[attr]] <- c(formatC(t, format = 'e'), 'Wilcoxon test')
    }
  }
  return(df)
}
    
analize_multiple <- function(attrs, summarisedData){
  # TODO

}

equal_nd <- function(gr1, gr2, attr, summarisedData){
  return(summarisedData[[gr1]][[attr]]$p.val > 0.05 & summarisedData[[gr2]][[attr]]$p.val > 0.05)
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
