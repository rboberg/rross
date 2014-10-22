####################
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 3
####################

####################
### Functions & Libraries

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
##################################################


# set wd
#setwd('F:/Docs/Personal/rross/MIDS/FieldExperiments/PS3')
setwd('C:/Users/Ross/Documents/R/rross/MIDS/FieldExperiments/PS3')

#####################
### Problem 2

# load data
fb_df = read.csv('broockman_green_anon_pooled_fb_users_only.csv')

### a) CI w/ no cluster

fb_sub1 = subset(fb_df, studyno == 1)
fb_sub2 = subset(fb_df, studyno == 2)

# run model
fb_lm1 = lm(name_recall ~ treat_ad, fb_sub1)
fb_t1= summary(fb_lm1)$coef[2,]

# calculate confidence interval
fb_ci1 = rep(fb_t1[1], 2) + 1.96 * fb_t1[2] * c(-1, 1)
fb_ci1
# ci: -0.0510, 0.0314

### c)  Cluster sanity check?
cl_minmax = aggregate(treat_ad ~ cluster, fb_df, function(x){return(c(min = min(x), max = max(x)))})
cl_dif_treat = cl_minmax[cl_minmax$treat_ad[,'min'] != cl_minmax$treat_ad[,'max'],]

# there seem to be 48 clusters that were not consistently treated / untreated
nrow(cl_dif_treat)

### d) 

cl(fb_sub1, fb_lm1, fb_sub1$cluster)
dat = fb_sub1
fm = fb_lm1
cluster = fb_sub1$cluster

require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)
M <- length(unique(cluster))
N <- length(cluster)
K <- fm$rank
dfc <- (M/(M-1))*((N-1)/(N-K))
uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum))
vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
coeftest(fm, vcovCL)

