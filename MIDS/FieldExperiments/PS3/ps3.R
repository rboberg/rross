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
#setwd('C:/Users/Ross/Documents/GitHub/rross/MIDS/FieldExperiments/PS3')

#####################
### Problem 2

# load data
fb_df = read.csv('broockman_green_anon_pooled_fb_users_only.csv', stringsAsFactors=F)

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

### d) Confidence interval with clusters

fb_lm1_cl = cl(fb_sub1, fb_lm1, fb_sub1$cluster)
fb_ci1_cl = rep(fb_lm1_cl[2,1], 2) + 1.96 * fb_lm1_cl[2,2] * c(-1, 1)
fb_ci1_cl
# ci: -0.0564, 0.0368

### e) Cluster CI for Study 2
fb_sub2 = fb_sub2[complete.cases(fb_sub2$name_recall, fb_sub2$treat_ad, fb_sub2$cluster),]

fb_lm2 = lm(name_recall ~ treat_ad, fb_sub2)
fb_lm2_cl = cl(fb_sub2, fb_lm2, fb_sub2$cluster)
fb_ci2_cl = rep(fb_lm2_cl[2,1], 2) + 1.96 * fb_lm2_cl[2,2] * c(-1, 1)
fb_ci2_cl
# ci: -0.0724, 0.0668

### f) Cluster CI for Both

fb_df = fb_df[complete.cases(fb_df$name_recall, fb_df$treat_ad, fb_df$cluster),]

fb_lm0 = lm(name_recall ~ treat_ad, fb_df)
fb_lm0_cl = cl(fb_df, fb_lm0, fb_df$cluster)
fb_lm0_cl
# est: -0.1551, p-value 7*10^-9

fb_ci0_cl = rep(fb_lm0_cl[2,1], 2) + 1.96 * fb_lm0_cl[2,2] * c(-1, 1)
fb_ci0_cl
# ci: -0.2075, -0.1027

### g) Model with study dummy

fb_lm00 = lm(name_recall ~ treat_ad + studyno, fb_df)
fb_lm00_cl = cl(fb_df, fb_lm00, fb_df$cluster)
fb_lm00_cl
# est: -0.0068, p-value 0.74

fb_ci00_cl = rep(fb_lm00_cl[2,1], 2) + 1.96 * fb_lm00_cl[2,2] * c(-1, 1)
fb_ci00_cl
# ci: -0.0468, 0.0332

### h) 

# Check out recall and treatment by study
aggregate(treat_ad ~ studyno, fb_df, mean)
aggregate(name_recall ~ studyno, fb_df, mean)


##########################
# Problem 4 Ebola

# load data
ebola_df = read.csv('ebola_rct2.csv', stringsAsFactors=F)

### a) No Covariates

lma = lm(vomiting_day14 ~ treat_zmapp, ebola_df)
summary(lma)

### b) Initial Vomiting and Temperature

lmb = lm(vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0, ebola_df)
summary(lmb)


### d) Same as b) + control for day 14 temperature (bad control)

lmd = lm(vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0 + temperature_day14, ebola_df)
summary(lmd)

### f) Treatment by sex

lmf = lm(vomiting_day14 ~ treat_zmapp * male + vomiting_day0 + temperature_day0 , ebola_df)
summary(lmf)
