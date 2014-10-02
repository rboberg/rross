#setwd("C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS2")
setwd("F:/Docs/Personal/rross/MIDS/FieldExperiments/PS2")
library(ggplot2)

#########################
### Problem 3.6

p36_df <- read.csv('ex3_6_data.csv')
n <- c(448,510)
p36_df <- transform(p36_df, y0 = round(y0/sum(y0)*n[1]), y1 = round(y1/sum(y1)*n[2]))
act_ate <- with(p36_df, sum((y1/sum(y1) - y0/sum(y0))*rating))

p36_df$all <- p36_df$y0 + p36_df$y1

subjects <- with(p36_df, rep(rating, all))

run_sim <- function(subjects, n){
  assign <- sample(c(rep(0,n[1]),rep(1,n[2])))
  ate <- mean(subjects[assign==1]) - mean(subjects[assign==0])
  return(ate)
}

iter = 10000
est_ate <- replicate(10000, run_sim(subjects, n))

# how many bigger?
n_bigger = sum(est_ate >= act_ate)
n_bigger

# one-tailed p-value estimate
n_bigger/iter

# how many bigger abs?
n_bigger_abs = sum(abs(est_ate) >= act_ate)
n_bigger_abs

# two-tailed p-value estimate
n_bigger_abs/iter

ggplot(data.frame(ATE = est_ate), aes(x=ATE)) + geom_density() + geom_vline(xint=act_ate, col='blue')


#############################
###