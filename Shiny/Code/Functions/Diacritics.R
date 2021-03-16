# function to replace accented characters with unaccented equivalents 
removeDiacritics <- function(string) {
  library(stringi)
  stri_trans_general(string, 'latin-ascii')
}
