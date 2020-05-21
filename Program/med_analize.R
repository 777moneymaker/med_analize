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
parser$add_argument('-gr', '--group', dest = 'grp', help = 'group name for group summary. No group -> no summary. Default \"all\" -> all groups', required = F, default = 'all')
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
n_missing <- sum(!stats::complete.cases(loadedData))
cat('In given file', n_missing, 'record/s with Not Available data.\n')
reportData$n_missing <- n_missing

# Impute the data if neccessary.
loadedData <- imputeAndNotify(loadedData, n_missing)
reportData$fullData <- loadedData

# Print short summary.
cat('Short summary of the given data.\n')
cat('==================================\n')
summary(loadedData)
cat('==================================\n')
reportData$shortSummary <- quiet(summary(loadedData))

# If user wants long format.
reportData$numericSummary <- capture.output(invisible(summariseByGroup(med_data = loadedData, grName = args$grp)))
reportData$fullSummary <- quiet(summariseByGroup(med_data = loadedData, grName = args$grp))
if(!args$long_f){
  cat('\nLong Summary of numeric data\n')
  cat('==================================\n')
  cat(reportData$numericSummary, sep = '\n')
  cat('==================================\n \n')
}else{
  cat('Skipped long summary report.\n\n')
}

# Report about outliers.
cat('\nNumber of outliers in given data sets\n')
cat('==================================\n')
cat('WARNING! Number of outliers may differ in specific groups!\n')
l_outliers <- lapply(loadedData[,sapply(loadedData, is.numeric)], outliers)
reportData$outliersReport <- invisible(capture.output((function(outs)
  for(attr in names(outs))
    cat('In attribute', attr, length(outs[[attr]]), 'outliers.\n'))(l_outliers)))
cat(reportData$outliersReport, sep = '\n')
cat('==================================\n')

# Plot outliers
grouped_box_plot(loadedData, args$plotDir)

# Make shapiro test. Plot distributions.
reportData$dataSignificance <- capture.output(invisible(nd_group_test(loadedData)))
if(!args$long_f){
  cat('Data significance according to normal distribution.\n')
  cat('==================================\n')
  cat(reportData$dataSignificance, sep ='\n')
  cat('==================================\n \n')
}else{
  cat('Skipped data significance report.\n\n')
}
quiet(nd_group_plot(loadedData, args$plotDir))

# Generate raport.
if(!is.null(args$outfile)){
  reportData$args <- args
  generateReport(reportData)
}

