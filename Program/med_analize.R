#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

options(warn = -1)

# Load library.
source('~/med_analize/utilities.R')

# Description
med_desc <- "Medical data analyzer.\\n\\
Program was written for statistical analysis of medical data.\\n\\
The required format of data file is *.csv file.\\n\\
Another requirement is to set first column as the group name e.g. gr1, gr2 etc."

# Arguments parser.
parser <- argparse::ArgumentParser(description = med_desc, 
								   formatter_class= 'argparse.RawTextHelpFormatter')
parser$add_argument('-o', '--out', dest = 'outfile', help = 'output file with generated report', required = F)
parser$add_argument('-l', '--long', dest = 'long_f', help = 'allow printing long reports on the screen', required = F, action = 'store_false')
parser$add_argument('-gr', '--group', dest = 'grp', help = 'group name for group summary. No group -> no summary. Default \"all\" -> all groups', required = F, default = 'all')
required_args <- parser$add_argument_group('required arguments')
required_args$add_argument('-xls', '--xls', dest = 'xlsfile', help = '*.xls file for group summary to be saved', required = F)
required_args$add_argument('-f', '--file', dest = 'file', help = 'path to file containing data', required = TRUE)
args <- parser$parse_args()


# # File load for interactive use.
# L <- readLines('przykladoweDane-Projekt.csv', n = 1)
# if(grepl(";", L)){
#   fh_int <- read.csv2(file = 'przykladoweDane-Projekt.csv', sep = ';')
# }else{
#   fh_int <- read.csv2(file = 'przykladoweDane-Projekt.csv', sep = ',')
# }

# File load for batch mode.
L <- readLines(args$file, n = 1)
if(grepl(";", L)){
  loadedData <- read.csv2(file = args$file, sep = ';')
}else{
  loadedData <- read.csv2(file = args$file, sep = ',')
}

reportData <- list()

n_missing <- sum(!stats::complete.cases(loadedData))
cat('In given file', n_missing, 'record/s with Not Available data.\n')
reportData$n_missing <- n_missing

# Impute the data if neccessary.
loadedData <- imputeAndNotify(loadedData, n_missing)
# fh_int <- usrUtilities$imputeAndNotify(fh_int, n_missing)

reportData$fullData <- loadedData

# Print short summary.
cat('Short summary of the given data.\n')
cat('==================================\n')
summary(loadedData)
cat('==================================\n')
reportData$shortSummary <- quiet(summary(loadedData))

if(!args$long_f){
  cat('\nLong Summary of numeric data\n')
  cat('==================================\n')
  reportData$numericSummary <- summariseByGroup(med_data = loadedData, grName = args$grp)
  cat('==================================\n')
}else{
  cat('Skipped long summary report.\n')
  reportData$numericSummary <- quiet(summariseByGroup(med_data = loadedData, grName = args$grp))
}

# Generate raport.
if(!is.null(args$outfile)){
  reportData$args <- args
  generateReport(reportData)
}

