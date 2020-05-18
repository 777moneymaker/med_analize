#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

# Load library.
usrUtilities <- modules::use('~/med_analize/utilities.R')

# Description
med_desc <- "Medical data analyzer.\\n\\
Program was written for statistical analysis of medical data.\\n\\
The required format of data file is *.csv file."

# Arguments parser.
parser <- argparse::ArgumentParser(description = med_desc, 
								   formatter_class= 'argparse.RawTextHelpFormatter')
parser$add_argument('-o', '--out', dest = 'outfile', help = 'output file with generated raport', required = F)
required_args <- parser$add_argument_group('required arguments')
required_args$add_argument('-f', '--file', dest = 'file', help = 'path to file containing data', required = TRUE)
args <- parser$parse_args()


# File load for interactive use.
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
loadedData <- usrUtilities$imputeAndNotify(loadedData, n_missing)
summary(loadedData)

# Generate raport.
if(!is.null(args$outfile)){
  usrUtilities$generate(med_data = loadedData, outfile = args$outfile, n_missing = n_missing)
}


