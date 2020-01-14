# function to create safe names (for columns and files)
# replaces all non-alphabetic and non-numeric characters with _


safe_names <- function(x){
  gsub("[^[:alnum:]]+", "_", x)
}