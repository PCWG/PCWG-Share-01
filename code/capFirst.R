capFirst <- function(str.in){
  paste0(toupper(substr(str.in, 1, 1)), 
         substr(str.in, 2, nchar(str.in)))
}
