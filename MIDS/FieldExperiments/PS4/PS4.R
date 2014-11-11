####################2
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 4
####################

library(foreign)

####################
### Functions & Libraries

# set wd
#setwd('F:/Docs/Personal/rross/MIDS/FieldExperiments/PS4')
setwd('C:/Users/Ross/Documents/R/rross/MIDS/FieldExperiments/PS4')
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
data3 = data3[complete.cases(data3),]

### a) ITT

turnout = aggregate(turnout ~ treat2, data3, mean)
itt3 = turnout[2,2] - turnout[1,2]
itt3

### b) Test

# Randomization Inference
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

# Regression w/ Clustered SE
lm3b <- lm(turnout ~ treat2, data3)

### c) CACE estimate (ignoring leaflet)

compliance.rate <- with(subset(data3, treat2 == 1), mean(contact))
cace3 <- itt3 / compliance.rate
cace3

### d) turnout without leaflet
# Assuming this is just asking us to subtract 1% from treatment turnout rate
turnout.noleaflet <- turnout
turnout.noleaflet[2,2] <- turnout[2,2] - 0.01
turnout.noleaflet

### e) CACE without leaflet
cace3.noleaflet <- (turnout.noleaflet[2,2]-turnout.noleaflet[1,2])/compliance.rate
cace3.noleaflet

### f) CACE no effect on compliers

turnout.f = aggregate(turnout ~ contact + treat2, data3, function(x){return(c(mean=mean(x),n=length(x)))})
turnout.fadj = turnout.f
turnout.fadj[turnout.fadj$contact==0 & turnout.fadj$treat2==1,3][1] <- turnout.f[turnout.f$contact==0 & turnout.f$treat2==1,3][1] - 0.03

control.turnout.adj <- with(subset(turnout.fadj, treat2==1), sum(turnout[,'mean']*turnout[,'n'])/sum(turnout[,'n']))
treatment.turnout <- with(subset(turnout.fadj, treat2==0), turnout[,'mean'])
itt3f <- control.turnout.adj - treatment.turnout
itt3f
cace3f <- itt3f / compliance.rate
cace3f


##########################
### Problem 4 - FE Exercise 5.11

# set up data from book
data4 <- data.frame(
  assigned = c('baseline', rep('treatment',2), rep('placebo',2)),
  treated = c(0,1,0,1,0),
  n = c(2572, 486, 2086, 470, 2109),
  turnout = c(0.3122, 0.3909, 0.3274, 0.2979, 0.3215),
  stringsAsFactors = F
  )

### a) placebo and treatment compliance rates

# expand data to test stated null hypothesis
data4.expanded <- data.frame(
  assigned = unlist(mapply(rep, data4$assigned, data4$n)),
  treated = unlist(mapply(rep, data4$treated, data4$n))
)
data4.expanded$turnout = unlist(mapply(
  function(n,pct1){
    n1 = round(n*pct1)
    return(c(rep(0,n-n1), rep(1,n1)))
  }, data4$n, data4$turnout))

lm4a <- lm(treated ~ assigned, subset(data4.expanded, assigned != 'baseline'))
summary(lm4a)

# p-value of 0.535 suggests these compliance rates are consistent with the stated null hypothesis

### b) placebo vs treatment never takers

lm4b <- lm(turnout ~ assigned, subset(data4.expanded, assigned != 'baseline' & treated == 0))
summary(lm4b)

# p-value of 0.68 suggests the turnout rates are not distinguishable for the two groups
# this is as we expected, so it informative in that it is evidence of a succesful
# experimental design (proper randomization).

### c) CACE of placebo

# ATE of placebo
lm4c <- lm(turnout ~ assigned, subset(data4.expanded, assigned != 'treatment'))
summary(lm4c)
placebo.compliance <- with(subset(data4, assigned == 'placebo'), n[treated==1]/sum(n))

# CACE of placebo
cace4c <- lm4c$coef[2]/placebo.compliance
cace4c
# the CACE of 2.7% is not statistically significant (CHECK ANSWER FROM PROF)
# with standard error 0.012943 / 0.1822 = 7.1%

### d) Treatment CACE

# Method 1 : Divide by compliance rate

# ATE of treatment
lm4d1 <- lm(turnout ~ assigned, subset(data4.expanded, assigned != 'placebo'))
summary(lm4d1)
treatment.compliance <- with(subset(data4, assigned == 'treatment'), n[treated==1]/sum(n))

# CACE Calc 1
cace4d1 <- summary(lm4d1)$coef[2,1:2]/treatment.compliance
cace4d1

# Method 2 : Compare turnout rates of placebo and treatment

# placebo vs treatment regression
lm4d2 <- lm(turnout ~ assigned, subset(data4.expanded, assigned != 'baseline' & treated == 1))
cace4d1 <- summary(lm4d2)$coef[2,1:2]
cace4d1

# Method 2 is more accurate than method 1 with tighter standard errors and thus more statistical significance


