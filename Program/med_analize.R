#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

# Load library.
source('~/med_analize/utilities.R')

# Description
med_desc <- "Medical data analyzer.\\n\\
Program was written for statistical analysis of medical data.\\n\\
The required format of data file is *.csv file."

# Arguments parser.
parser <- argparse::ArgumentParser(description = med_desc, 
								   formatter_class= 'argparse.RawTextHelpFormatter')
parser$add_argument('-o', '--out', dest = 'outfile', help = 'output file with generated report', required = F)
parser$add_argument('-l', '--long', dest = 'long_f', help = 'allow printing long reports on the screen', required = F, action = 'store_false')
required_args <- parser$add_argument_group('required arguments')
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


n_missing <- sum(!stats::complete.cases(loadedData))
cat('In given file', n_missing, 'record/s with Not Available data.\n')

# Impute the data if neccessary.
loadedData <- imputeAndNotify(loadedData, n_missing)
# fh_int <- usrUtilities$imputeAndNotify(fh_int, n_missing)

# Print short summary.
cat('Short summary of the given data.\n')
cat('==================================\n')
summary(loadedData)
cat('==================================\n')

if(!args$long_f){
  cat('Summary grouped by possible groups\n')
  cat('==================================\n')
  summariseNumericByCharacter(loadedData)
  cat('==================================\n')
}else{
  cat('Skipped long summary report.\n')
}

# Generate raport.
if(!is.null(args$outfile)){
  generateReport(med_data = loadedData, outfile = args$outfile, n_missing = n_missing)
}


