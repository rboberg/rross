#setwd("C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS2")
setwd("F:/Docs/Personal/rross/MIDS/FieldExperiments/PS2")
library(ggplot2)

###########################
### Functions

assign_treat <- function(n0, n1){
  return(sample(c(rep(0,n0),rep(1,n1))))
}

run_sim <- function(subjects, n0, n1){
  assign <- assign_treat(n0, n1)
  ate <- mean(subjects[assign==1]) - mean(subjects[assign==0])
  return(ate)
}


#########################
### (1) Problem 3.6

p36_df <- read.csv('ex3_6_data.csv')
n <- c(448,510)
p36_df <- transform(p36_df, y0 = round(y0/sum(y0)*n[1]), y1 = round(y1/sum(y1)*n[2]))
est_ate <- with(p36_df, sum((y1/sum(y1) - y0/sum(y0))*rating))

p36_df$all <- p36_df$y0 + p36_df$y1

subjects <- with(p36_df, rep(rating, all))

iter = 10000
rand_ate <- replicate(10000, run_sim(subjects, n[1], n[2]))

# how many bigger?
n_bigger = sum(rand_ate >= est_ate)
n_bigger

# one-tailed p-value estimate
n_bigger/iter

# how many bigger abs?
n_bigger_abs = sum(abs(rand_ate) >= est_ate)
n_bigger_abs

# two-tailed p-value estimate
n_bigger_abs/iter

ggplot(data.frame(ATE = rand_ate), aes(x=ATE)) + geom_density() + geom_vline(xint=est_ate, col='blue')


#############################
### (2) Problem 3.7

y0 = c(1,0,0,4,3)
y1 = c(2,11,14,0,3)

all_possible <- function(subjects, n1){
  all_assign <- t(
                combn(
                  1:length(subjects),
                  n1,
                  function(x){
                    zs <- rep(0,length(subjects))
                    zs[x] <- 1
                    return(zs)
                    }
                  )
                )
  
  return(
    apply(all_assign,1,function(x){
      return(mean(subjects[x==1]) - mean(subjects[x==0]))
      })
    )
}

#all_possible(c(y0,y1), length(y1))
sum(all_possible(c(y0,y1-7), length(y1)) >= mean(y1-7) - mean (y0))

# ???? Use a one-sided test because we have a specific hypothesis
# that weight loss will be positive not just a change in direction


#############################
### (3) Problem 3.8

