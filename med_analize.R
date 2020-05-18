#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

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

# File load.
loadedData <- read.csv2(file = args$file, sep = ';')
# fh_int <- read.csv2(file = 'przykladoweDane-Projekt.csv', sep = ';')

cat('W podanym pliku', sum(!stats::complete.cases(loadedData)), 'rekord/y/ów z niedostępnymi danymi.\n')

# Generate raport.
raports <- modules::use('generate_raport.R')
raports$generate(med_data = loadedData, outfile = args$outfile)


