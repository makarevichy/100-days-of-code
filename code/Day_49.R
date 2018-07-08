# Solution exercises Programmatically creating text output in R
# from https://www.r-bloggers.com/programmatically-creating-text-output-in-r-exercises/

# Exercise 1
x <- c(14.3409087337707, 13.0648270623048, 3.58504267621646, 18.5077076398145,
  16.8279241011882)
sprintf('$%.02f', x)

# Exercise 2
x <- c(25, 7, 90, 16)
sprintf("file_%03d.txt", x)

# Exercise 3
sprintf("file_%0*d.txt", nchar(max(x)), x)

# Exercise 4
x <- c("Stay the patient course.", "Of little worth is your ire.", "The network is down.")
n <- max(nchar(x))
cat(sprintf("%*s", n, x), sep = "\n")

# Exercise 5
tohex <- function(x) {
  sprintf("%1$d is %1$x in hexadecimal", x)
}

tohex(100)

# Exercise 6
x <- "my example"
sprintf("<h1>%s</h1>", x)

# Exrcise 7
x <- c("Stay the patient course.", "Of little worth is your ire.", "The network is down.")
library(dplyr)
x %>%
  sprintf("<li>%s</li>", .) %>%
  paste(., collapse = " ") %>%
  sprintf("<ul>%s</ul>", .)

# Exercise 8
text_l <- function(x) {
  n <- length(x)
  if (n <= 1) {
    return(x)
  }
  paste(paste(x[-n], collapse = ", "), "and", x[n])
}
films <- c("The Shawshank Redemption", "The Godfather", "The Godfather: Part II", "The Dark Knight", "12 Angry Men", "Schindler's List")

sprintf("The top ranked films on imdb.com are %s", text_l(films))

# Exercise 9
f <- function(x, y) {
  sprintf("%.*f%%", y, x * 100)
}
x <- 0.921313
f(x, 2)

# Exercise 10
f2 <- function(x, y) {
  p <- sprintf("%.*f%%", y, x * 100)
  if(any(nchar(p) > 10)) stop("Too long percentage")
  sprintf("%10s", p)
}

cat(f2(rnorm(10), 1), sep="\n")
