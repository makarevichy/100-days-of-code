diagR <- function(df){
  if(!is.matrix(df)) stop("'df' must be a matrix", call. = T)
  
  vec <- vector('numeric', 0L)
  for(i in 1:min(dim(df))){
    x <- df[i, i]
    vec <- c(vec, x)
  }
  vec
}
