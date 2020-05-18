#!/usr/bin/env Rscript

# __author__ = 'Milosz Chodkowski PUT'

generate <- function(med_data, outfile = 'med_raport.txt'){
  # Init raport description.
  sink(file = outfile, append = T)
  cat('Raport wygenerowany przy użyciu narzędzia med_analize.\n')
  cat('Autor -> Miłosz Chodkowski PUT\n')
  cat('======================================================\n\n')
  sink()
  
  # Count missing data.
  sink(file = outfile, append = T)
  cat('W podanym pliku', sum(!stats::complete.cases(med_data)), 'rekord/y/ów z niedostępnymi danymi.\n\n')
  sink()
  
  # Short summary.
  sink(file = outfile, append = T)
  cat('Krótkie podsumowanie wprowadzonych danych.\n')
  print(summary(med_data))
  sink()
}