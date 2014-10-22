setwd("C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS2")
#setwd("F:/Docs/Personal/rross/MIDS/FieldExperiments/PS2")
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

calc_est_se <- function(group, outcome){
  y0 <- outcome[group==0]
  y1 <- outcome[group==1]
  m <- length(y1)
  N <- m + length(y0)
  v0 <- var(y0)
  v1 <- var(y1)
  return(sqrt(v0/(N-m) + v1/m))
}

calc_est_ate <- function(group, outcome){
   return(mean(outcome[group==1]) - mean(outcome[group==0]))
}

#########################
### (1) Problem 3.6

# read & organize a csv created from the book
p36_df <- read.csv('ex3_6_data.csv')
n <- c(448,510)
p36_df <- transform(p36_df, y0 = round(y0/sum(y0)*n[1]), y1 = round(y1/sum(y1)*n[2]))

# estimate ate
est_ate <- with(p36_df, sum((y1/sum(y1) - y0/sum(y0))*rating))

p36_df$all <- p36_df$y0 + p36_df$y1

subjects <- with(p36_df, rep(rating, all))

iter = 10000
rand_ate <- replicate(iter, run_sim(subjects, n[1], n[2]))

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

#ggplot(data.frame(ATE = rand_ate), aes(x=ATE)) + geom_density() + geom_vline(xint=est_ate, col='blue')


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

all_rand <- all_possible(c(y0,y1-7), length(y1))
sum(all_rand >= mean(y1-7) - mean (y0)) / length(all_rand)
sum(all_rand >= mean(y1-7) - mean (y0)) / length(all_rand)

# Use a one-sided test because we have a specific hypothesis
# that weight loss will be positive not just a change in direction


#############################
### (3) Problem 3.8

# load data as data frame w/ 3 columns
# st denotes the state: tx or ar
# group is 0 (4 year term) or 1 (2 year term)
# bills is the number of bills introduced

p38_df <- read.csv('ex3_8_data.csv')

# (a)
est_ate <- c(
  ar = with(subset(p38_df, st == 'ar'), calc_est_ate(group, bills)),
  tx = with(subset(p38_df, st == 'tx'), calc_est_ate(group, bills))
  )
est_ate

# (b)
est_se <- c(
  ar = with(subset(p38_df, st == 'ar'), calc_est_se(group, bills)),
  tx = with(subset(p38_df, st == 'tx'), calc_est_se(group, bills))
  )
est_se

# (c)
n_st <- c(
  ar=sum(p38_df$st == 'ar'),
  tx=sum(p38_df$st == 'tx')
  )

calc_block_ate <- function(ates, ns){
  return(sum(ates * ns) / sum(ns))
}

block_ate <- calc_block_ate(est_ate, n_st)
block_ate

# (d)
# Why just pooling would be biased?
# Equal numbers of control in each group
# but more treatment in Arkansas. Because AR senators
# typically introduced fewer bills, this would bias the average 
# bills introduced by the treatment group lower and
# exagerate the negative treatment effect.

#(e)
total_ate_se <- sqrt(
  est_se['ar']^2 * n_st['ar'] / nrow(p38_df) + 
    est_se['tx']^2 * n_st['tx'] / nrow(p38_df)
  )
unname(total_ate_se)

# (f)
# Randomization Inference w/ blocking?
# Need to randomize by block!

# function to cacluate random inference with blocks
# n_bg is just a table of blocks and groups
# it's ncessary in the function but faster to calculate once before hand
# instead of every time

run_block_sim <- function(block, group, outcome, n_bg = table(block, group)){
  ates <- c()
  ns <- c()
  for(blocki in unique(block)){
    assign <- assign_treat(n_bg[blocki,'0'], n_bg[blocki,'1'])
    ates[blocki] <- calc_est_ate(assign, outcome[block == blocki])
    ns[blocki] <- sum(n_bg[blocki,])
  }
  
  return(calc_block_ate(ates, ns))
}

block <- p38_df$st
group <- p38_df$group
outcome <- p38_df$bills
n_bg <- table(block, group)

iter = 10000
rand_ate <- replicate(iter, run_block_sim(block, group, outcome, n_bg))
sum(rand_ate > abs(block_ate)) / iter

#ggplot(data.frame(ATE = rand_ate), aes(x=ATE)) + geom_density() + geom_vline(xint=block_ate, col='blue')

# Histograms
ggplot(p38_df, aes(x=bills)) + geom_histogram(binwidth=10) + facet_wrap(st~group)


#############################
### (4) Problem 3.11

# calculate cluster standard errors

calc_cluster_se <-function(y0,y1,m,cluster){
  k <- length(unique(cluster))
  N <- length(y0)
    
  cluster_means <- aggregate(data.frame(y0=y0, y1=y1),list(cluster=cluster),mean)
  
  # R uses n-1 to calculate variance
  # but we are computing actual variance, not estimating
  # so want to adjust the values
  sigma <- cov(cluster_means[,c('y0','y1')]) * k / (k-1)
  
  return(sqrt(1/(k-1)*(m/(N-m)*sigma[1,1] + (N-m)/m*sigma[2,2]) + 2*sigma[1,2]))

}

y0 <- c(0,1,2,4,4,6,6,9,14,15,16,16,17,18)
y1 <- c(0,0,1,2,0,0,2,3,12,9,8,15,5,17)

# (a)
cluster <- rep(1:7,each=2)
calc_cluster_se(y0,y1,m=3*2,cluster=cluster)

# (b)
cluster <- c(1:7,7:1)
calc_cluster_se(y0,y1,m=3*2,cluster=cluster)


#############################
### (5) iPhones

N = 1000000
p1 = .007
p2 = .005

### (b)

calc_ci <- function(p1,p2,n1,n2){
  x1 = p1*n1
  x2 = p2*n2
  xbar = p1-p2
  p = (x1+x2)/(n1+n2)
  se = sqrt(p*(1-p)*(1/n1 + 1/n2))
  ci = c(xbar - se*1.96, xbar + se*1.96)
  return(ci)
}


n1 = N*.5
n2 = N*.5

ci1 <- calc_ci(p1,p2,n1,n2)
ci1

### (d)

n1 = 0.99*N
n2 = 0.01*N
ci2 <- calc_ci(p1,p2,n1,n2)
ci2[2] - ci2[1]


#############################
### (6) Auction

auction_df <- read.csv('list_luckingreiley_auction_data.csv')

# treatment = theoretically lower bids

# (a)

est_ate <- with(auction_df, calc_est_ate(uniform_price_auction, bid))
est_se <- with(auction_df, calc_est_se(uniform_price_auction, bid))
# could also have used regression to calculate these
reg_summary <- summary(lm(bid~uniform_price_auction, auction_df))

ci <- est_ate + 1.96*est_se*c(-1, 1)
ci

# (b)
# p-value with regression
p_reg <- reg_summary$coef[2,4]

# p-value with randomization inference
n0 <- sum(auction_df$uniform_price_auction==0)
n1 <- sum(auction_df$uniform_price_auction==1)

iter <- 10000
rand_ate <- replicate(iter, run_sim(auction_df$bid, n0, n1))

p_rand <- sum(abs(rand_ate) > abs(est_ate)) / iter

p_reg
p_rand

# not much different, (usually the case for samples > 50)
# if the sample size was smaller then we could start to see
# bigger differences