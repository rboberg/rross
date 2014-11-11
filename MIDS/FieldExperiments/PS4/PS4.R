####################2
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 4
####################

library(foreign)

####################
### Functions & Libraries

# set wd
setwd('F:/Docs/Personal/rross/MIDS/FieldExperiments/PS4')
#setwd('C:/Users/Ross/Documents/R/rross/MIDS/FieldExperiments/PS4')
#setwd('C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS4')

#Function to compute clustered standard errors in R
cl <- function(dat, fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

#####################
### Problem 3 - Exercise 5.10

# load data
data3 = read.dta('Guan_Green_CPS_2006.dta')

### a) ITT

turnout = aggregate(turnout ~ treat2, data3, mean)
itt3 = turnout[2,2] - turnout[1,2]
itt3

### b) Test

# Try RI, stop at 5:10
act.clust <- merge(merge(aggregate(turnout ~ dormid, data3, sum), aggregate(treat2 ~ dormid, data3, mean)), aggregate(n ~ dormid, transform(data3, n = 1), sum))

singlesim <- function(act.clust){
  groups <- sample(act.clust$treat2)
  to0 <- sum(act.clust$turnout[groups == 0])
  to1 <- sum(act.clust$turnout[groups == 1])
  n0 <- sum(act.clust$n[groups == 0])
  n1 <- sum(act.clust$n[groups == 1])
  return(to1/n1 - to0/n0)
}

itt3sim <- replicate(100000, singlesim(act.clust))
sum(itt3 < abs(itt3sim))


2lm3b <- lm(turnout ~ treat2, data3)
summary(lm3b)

cl(data3, lm3b, data3$dormid)


