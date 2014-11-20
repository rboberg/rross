####################2
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 4
####################
library(ggplot2)
library(foreign)

####################
### Functions & Libraries

# set wd
#setwd('F:/Docs/Personal/rross/MIDS/FieldExperiments/PS4')
#setwd('C:/Users/Ross/Documents/R/rross/MIDS/FieldExperiments/PS4')
setwd('C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS4')

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
lm3b.clse <- cl(data3, lm3b, data3$dormid)
lm3b.clse

### c) CACE estimate (ignoring leaflet)

compliance.rate <- with(subset(data3, treat2 == 1), mean(contact))
cace3 <- itt3 / compliance.rate
cace3

### e) turnout without leaflet
# adjust turnout rate of noncompliers in the treatment group down by 1%
# but leave the compliers alone since we want to include the effect of the leaflet
turnout.e = aggregate(turnout ~ contact + treat2, data3, function(x){return(c(mean=mean(x),n=length(x)))})
turnout.e.adj = turnout.e
turnout.e.adj[turnout.e.adj$contact==0 & turnout.e.adj$treat2==1,3][1] <- turnout.e[turnout.e$contact==0 & turnout.e$treat2==1,3][1] - 0.01

# calculate new treatment turnout as weigthed average of complier and
# non complier turnout
treatment.turnout.adj.e <- with(subset(turnout.e.adj, treat2==1), sum(turnout[,'mean']*turnout[,'n'])/sum(turnout[,'n']))
control.turnout.e <- with(subset(turnout.e.adj, treat2==0), turnout[,'mean'])

# calculate new ITT
itt3e <- treatment.turnout.adj.e - control.turnout.e
itt3e

# calculate new CACE
cace3e <- itt3e / compliance.rate
cace3e

# ### e) CACE without leaflet
# This is how I originally did it because I thought we wanted to 
# exclude the effect of the leaflet
# # Assuming this is just asking us to subtract 1% from treatment turnout rate
# turnout.noleaflet <- turnout
# turnout.noleaflet[2,2] <- turnout[2,2] - 0.01
# turnout.noleaflet
# 
# cace3.noleaflet <- (turnout.noleaflet[2,2]-turnout.noleaflet[1,2])/compliance.rate
# cace3.noleaflet


### f) CACE no effect on compliers

# adjust turnout rate of noncompliers in the treatment group down by 3%
turnout.f = aggregate(turnout ~ contact + treat2, data3, function(x){return(c(mean=mean(x),n=length(x)))})
turnout.f.adj = turnout.f
turnout.f.adj[turnout.f.adj$contact==0 & turnout.f.adj$treat2==1,3][1] <- turnout.f[turnout.f$contact==0 & turnout.f$treat2==1,3][1] - 0.03


# calculate new treatment turnout as weigthed average of complier and
# non complier turnout
treatment.turnout.adj.f <- with(subset(turnout.f.adj, treat2==1), sum(turnout[,'mean']*turnout[,'n'])/sum(turnout[,'n']))
control.turnout.f <- with(subset(turnout.f.adj, treat2==0), turnout[,'mean'])

# calculate new ITT
itt3f <- treatment.turnout.adj.f - control.turnout.f
itt3f

# calculate new CACE
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

# test whther there is a different in treatment rates (compliance rates) for treatment vs placebo
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

######### EXTRA: CHECKING STANDARD ERROR ASSUMPTION #####

data4csub <- subset(data4.expanded, assigned != 'treatment')
lm4c.step1 <- lm(treated ~ assigned,data4csub)
lm4c.step2 <- lm(turnout ~ fitted.treated, transform(data4csub, fitted.treated=lm4c.step1$fitted.values))
summary(lm4c.step2)

data4dsub <- subset(data4.expanded, assigned != 'placebo')
lm4d.step1 <- lm(treated ~ assigned, data4dsub)
lm4d.step2 <- lm(turnout ~ fitted.treated, transform(data4dsub, fitted.treated=lm4d.step1$fitted.values))
summary(lm4d.step2)
        
###### END EXTRA ########################################


#######################
### Problem 8: FE 8.9

# load data
data8 <- read.csv("GerberGreenBook_Chapter8_Table_8_4_8_5.csv", stringsAsFactors = F)

### a) No spillover hot spots
data8a <- subset(data8, hotwitin500 == 0)

# calculate y01 - y00
# this calculation is overkill in this case b/c all probabilities are the same
# but figured I would do it anyway.
y01_y00_8a <- with(data8a, sum(y01 * 1/prob01)/sum(1/prob01) - sum(y00 * 1/prob00)/sum(1/prob00))
y01_y00_8a


### b) spillover hot spots
data8b <- subset(data8, hotwitin500 > 0)
y01_y00_8b <- with(data8b, sum(y01 * 1/prob01)/sum(1/prob01) - sum(y00 * 1/prob00)/sum(1/prob00))
y10_y00_8b <- with(data8b, sum(y10 * 1/prob10)/sum(1/prob10) - sum(y00 * 1/prob00)/sum(1/prob00))
y11_y00_8b <- with(data8b, sum(y11 * 1/prob11)/sum(1/prob11) - sum(y00 * 1/prob00)/sum(1/prob00))

y01_y00_8b
y10_y00_8b
y11_y00_8b


### c) spillover non-experimental
data8c <- read.csv("non_experimental_hotspots.csv", stringsAsFactors = F)
y10_y00_8c <- with(
  subset(data8c, prob10 > 0),
  sum(y10 * 1/prob10)/sum(1/prob10) - sum(y00 * 1/prob00)/sum(1/prob00)
  )
y10_y00_8c


################
### Problem 9: FE 8.10

data9 = read.dta('Hough_WorkingPaper_2010.dta')

### b) effect of running on Tetris
data9$run_yest = c(NA, data9$run[1:(nrow(data9)-1)])

lm9b0 = lm(tetris ~ run, data9)
summary(lm9b0)

lm9b1 = lm(tetris ~ run_yest, data9)
summary(lm9b1)

lm9b2 = lm(tetris ~ run + run_yest, data9)
summary(lm9b2)

# Something else interesting...
# I noticed that the scores were better at the end of the time
# and figured that she was probably improving at tetris as she played.
# So I thought "day" made sense to include as a covariate.
# A regression with day indeed has higher R^2 (0.26 to 0.49) and increases the precision
# of the "run" coefficient, lowering the standard error (4856 to 4290).
# But the t-value acutally goes down a bit (2.80 to 2.39)
# because the estimate dropped (she radnomly ran more at
# the end of the study when her scores were better)

lm9b0_day = lm(tetris ~ run + day, data9)
summary(lm9b0_day)


### c) effect of running tomorrow on Tetris
data9$run_tom = c(data9$run[-1], NA)

lm9c = lm(tetris ~ run_tom, data9)
summary(lm9c)

### d) effect of running on energy level & GRE

lm9d_energy = lm(energy ~ run, data9)
summary(lm9d_energy)

lm9d_gre = lm(gre ~ run, data9)
summary(lm9d_gre)


### e) Tetris scores not normal....
ggplot(data9, aes(x=tetris)) + geom_density()
