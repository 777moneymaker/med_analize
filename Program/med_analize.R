#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

# Suppress warnings.
options(warn = -1)

# Load library.
source('~/med_analize/utilities.R')

# Description
med_desc <- "Medical data analyzer.\\n\\
Program was written for statistical analysis of medical data.\\n\\
The required format of data file is *.csv file.\\n\\
Another requirement is to set first column as the group name e.g. gr1, gr2 etc.\\n\\ \\n\\
90 % of the analysis is the analysis of numeric data. That was my intention.\\n\\
Author: Miłosz Chodkowski PUT"

# Arguments parser.
parser <- argparse::ArgumentParser(description = med_desc, 
								   formatter_class= 'argparse.RawTextHelpFormatter')
parser$add_argument('-o', '--out', dest = 'outfile', help = 'output file with generated report', required = F)
parser$add_argument('-l', '--long', dest = 'long_f', help = 'allow printing long reports on the screen', required = F, action = 'store_false')
required_args <- parser$add_argument_group('required arguments')
required_args$add_argument('-xls', '--xls', dest = 'xlsfile', help = '*.xls file for group summary to be saved', required = F)
required_args$add_argument('-f', '--file', dest = 'file', help = 'path to file containing data', required = TRUE)
required_args$add_argument('-s', '--sep', dest = 'sep', help = 'separator used in csv file', required = TRUE)
required_args$add_argument('-plt', '--plot', dest = 'plotDir', help = 'path to directory in which all plots will be saved.', required = TRUE)
args <- parser$parse_args()

# File load for batch mode.
L <- readLines(args$file, n = 1)
loadedData <- read.csv2(file = args$file, sep = args$sep)

# Empty list with data for report.
reportData <- list()

# Calculate number of missing data cells.
n_missing <- sum(is.na(loadedData))
cat('In given file', n_missing, 'record/s with Not Available data.\n')
reportData$n_missing <- n_missing

# Impute the data if neccessary.
loadedData <- imputeAndNotify(loadedData, n_missing)
reportData$fullData <- loadedData

# Print short summary.
cat('Short summary of the given data.\n')
stargazer(loadedData, summary = T, type = 'text')
cat('\n\n')
reportData$shortSummary <- quiet(summary(loadedData))

# If user wants long format.
# reportData$fullSummaryStr <- capture.output(print(as.data.frame(summariseAllData(med_data = loadedData)), quote = T, row.names = F, right = F))
reportData$fullSummary <- summariseAllData(med_data = loadedData)
if(!args$long_f){
  cat('\nLong Summary of numeric data.\n')
  for(gr in names(reportData$fullSummary)){
    cat(sprintf('Group %s:\n', gr))
    for(df in names(reportData$fullSummary[[gr]])){
      cat(sprintf('Attribute %s stats:', df))
      stargazer(as.data.frame(reportData$fullSummary[[gr]][[df]][,-1]), type = 'text', summary = F, rownames = F)
      cat('\n')
    }
    cat('\n')
  }
  cat('\n\n')
}else{
  cat('Skipped long summary report.\n\n')
}

# Report about outliers.
cat('Number of outliers in given data sets\n')
cat('==================================\n')
cat('WARNING! Number of outliers may differ in specific groups!\n')
l_outliers <- lapply(loadedData %>% select_if(is.numeric), outliers)
f <- function(outs){
  cat('Attribute: \n')
  for(attr in names(outs))
    cat(sprintf('\t%s: %i outliers.\n', attr, length(outs[[attr]])))
}
reportData$outliersStr <- capture.output(f(l_outliers))

cat(reportData$outliersStr, sep = '\n')
cat('\n\n')

# Plot outliers
quiet(grouped_box_plot(loadedData, args$plotDir))

# Make shapiro test. Plot distributions. -> Data significance.
if(!args$long_f){
  cat('Data significance according to normal distribution.\n')
  cat('==================================\n')
  report_data_significance(reportData$fullSummary)
  cat('\n\n')
}else{
  cat('Skipped data significance report.\n\n')
}
cat('**Plotting data** ...\n')
quiet(nd_group_plot(loadedData, args$plotDir))


# Make statistical analysis
if(length(unique(loadedData[[1]])) < 2){
  warning('Data contains only 1 group. Statistical comparsion is not possible beetween 1 group.\n Skipping analysis ...')
}else if(length(unique(loadedData[[1]])) == 2){
  cat('Group-wise analysis stats:')
  reportData$analysis <- quiet(analize_two(loadedData, reportData$fullSummary))
  reportData$analysisStr <- capture.output(invisible(analize_two(loadedData, reportData$fullSummary)))
  stargazer(reportData$analysis %>% as.data.frame, type = 'text', summary = F, rownames = F)
  cat('Interpretation: \n')
  cat(reportData$analysisStr, sep = '\n')
}else{
  NULL
}

# Generate raport.
if(!is.null(args$outfile)){
  reportData$args <- args
  generateReport(reportData)
}

