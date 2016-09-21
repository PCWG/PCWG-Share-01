NullToNA <- function(x,xAlt){
  # function to return NA if a single input is NULL, or override this with some alternative value 
  
  if(is.null(x)){
    if(missing(xAlt)){
      return(NA)
    } else {
      return(xAlt)
    }
  } else {
    return(x)
  }
}