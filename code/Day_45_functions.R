#function sum of variables
colsumR <- function(df, na.rm = T){
  if(is.data.frame(df)){
    x <- vapply(df, is.numeric, logical(1))
    df <- df[ ,x]
  }
  if(is.matrix(df) || is.data.frame(df)){
    vapply(df, sum, numeric(1), na.rm = na.rm)
  } else {
    cat('"df" is not a matrix or data.frame\n')
  }
}
# test
colsumR(iris, na.rm = F)

#any function
f <- function(df, expr = mean, na.rm = T){
  if(is.data.frame(df)){
    x <- vapply(df, is.numeric, logical(1))
    df <- df[ ,x]
  }
  if(is.matrix(df) || is.data.frame(df)){
    vapply(df, substitute(expr), numeric(1), na.rm = na.rm)
  } else {
    stop(paste('"df" is not a matrix or data.frame.', '\n'))
  }
}
#test
f(mtcars, median)

#microbenchmark rotation matrix
x <- matrix(1:9, nrow = 3)
microbenchmark::microbenchmark(
  x,
  
  # rotation 90 degres
  apply(t(x), 2, rev),
  
  # rotation 180 degres
  apply(apply(x, 1, rev), 1, rev),
  
  # rotation 270 degree
  t(apply(t(x), 1, rev))
)

#function then rotation matrix
matrix_rotation <-  function(df, rot = c('90', '180', '270')){
  if(!is.matrix(df)) stop("'df' must be a matrix", call. = FALSE)
  rot <- match.arg(rot)
  switch(rot,
         '90' = apply(t(df), 2, rev),
         '180' = apply(apply(df, 1, rev), 1, rev),
         '270' = t(apply(t(df), 1, rev)))
}

#test
matrix_rotation(x, '90')