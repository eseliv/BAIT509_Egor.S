library("tidyverse")
library("knitr")

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

### Excercise 5.1

# Mean of Y is approximately 2.8

### KKn
n = subset(dat, x >= -1, x < 1.1)

dat$d <- abs(dat$x-0)
dat$d
dat_knn <- arrange(dat, d)
dat_knn
dat_knn_final <- dat_knn[1:5,]
dat_knn_final

y_predict <- mean(dat_knn_final$y)
y_predict


###Lowess

dat$d <- abs(dat$x-0)
dat2 <- arrange(dat, d)
dat2
dat2_final<- dat2[1:5,]
dat2_final

dat_loess = filter(dat2_final, d<10)
mean(dat_loess$y)

### 2nd problem

library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
  dat$d <- abs(dat$x-x)
  dat_new<- arrange(dat, d)
  dat_new <- dat_new[1:8,]
    return(mean(dat_new$y))})
  

loess_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
  dat$d <- abs(dat$x-x)
  dat2 <- arrange(dat, d)
  dat2_final<- dat2[1:5,]
  dat_loess = filter(dat2_final, d<10)
  return(mean(dat_loess$y))
})

est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour = 'orange') +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
