install.packages("tidyverse")
install.packages("ISLR")
install.packages("knitr")
mse <- mean((dat$yhat - dat$y)^2)

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}

cladata <- gencla(100)
head(cladata)

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}

library("tidyverse")

gencla


# if X = 1
predat <- mutate(cladata,
                 x_1 <- 1,
                 pB <- 0.8/(1+exp(-(x_1))),
                 y <- map_chr(pB, function(x_1) 
                   sample(LETTERS[1:3], size=1, replace=TRUE,
                          prob=c(0.2, x_1, 1-x_1-0.2))))
head(predat)

# if X = -2

pB2 <- 0.8/(1+exp(-2))
pB2
x_1 =0
predat2 <- mutate(cladata,
                  x_1 <- -2,
                  pB <- 0.8/(1+exp(-x_1)),
                  y <- map_chr(pB, function(x_1) 
                    sample(LETTERS[1:3], size=1, replace=TRUE,
                           prob=c(0.2, x_1, 1-x_1-0.2))))
head(predat2)