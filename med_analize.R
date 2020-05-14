#!/usr/bin/Rscript

# __author__ = 'Milosz Chodkowski PUT'

# Description
med_desc <- "Medical data analyzer.\\n\\
Program was written for statistical analysis of medical data.\\n\\
The required format of data file is *.csv file."

# Arguments parser.
parser <- argparse::ArgumentParser(description = med_desc, 
								   formatter_class= 'argparse.RawTextHelpFormatter')

required_args <- parser$add_argument_group('required arguments')
optional_arg <- parser$add_argument_group('optional arguments')
required_args$add_argument('-f', '--file', dest = 'file', help = 'path to file containing data', required = TRUE)
args <- parser$parse_args()

# File load *TEST*.
fh <- read.csv(file = args$file, sep = ';')
print(fh)

# File load for interactive use.
# fh <- read.csv(file = 'path/to/file, sep = ';')
