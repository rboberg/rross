setwd("C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS2")
library(ggplot2)

####
# Setup Problem 3.6

p36_df <- read.csv('ex3_6_data.csv')
n <- c(448,510)
p36_df <- transform(p36_df, y0 = round(y0/sum(y0)*n[1]), y1 = round(y1/sum(y1)*n[2]))
act_ate <- with(p36_df, sum((y0/sum(y0) - y1/sum(y1))*rating))

p36_df$all <- p36_df$y0 + p36_df$y1

subjects <- with(p36_df, rep(rating, all))

run_sim <- function(subjects, n){
  assign <- sample(c(rep(0,n[1]),rep(1,n[2])))
  ate <- mean(subjects[assign==1]) - mean(subjects[assign==0])
  return(ate)
}

est_ate <- replicate(10000, run_sim(subjects, n))

ecdf(est_ate)(act_ate)

ggplot(data.frame(ATE = est_ate), aes(x=ATE)) + geom_density() + geom_vline(xint=act_ate, col='blue')
