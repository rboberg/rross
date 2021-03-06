####################2
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 5
####################

####################
### Functions & Libraries
library(ggplot2)

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

################################################
### Problem 1 - Yahoo Ad Natural Experiment

# Load data
data1 = read.csv('ps5_no1.csv')

# A) Crosstab total_exposures & treatment_ad_exposures

crosstab = table(data1$total_exposures, data1$treatment_ad_exposures)
print(crosstab)

# The occurences seem close to what you would expect with random selection.
# With 0 total exposures, you would expect 0 treatment exposures.
# With 1 total exposure, you would expect a 50/50 split of treatment/control.
# With 2 total exposures, you would expect a 25/50/25 split of see 0, 1 or 2 treatments.
# From 2 total you expect to see twice as many 1 treatments as 0s and 2s because
# there are 2 ways you can draw a 1, but only one way to draw a 0 or a 2.

# B) Placebo test

# My colleagues plan
summary(lm(week1 ~ treatment_ad_exposures, data1))

# Placebo test
summary(lm(week0 ~ treatment_ad_exposures, data1))

# Although my colleagues "planned " analysis shows highly statistically
# significant results (p-value ~0), so does the placebo test. This suggests
# the treatment effect is at least overstated by just looking at week1 sales
# as an outcome better - so the placebo test fails in that it shows
# the original statistical analysis was flawed.

# C) Why does the Placebo test look like this & what do you need to change?

# The placebo test turns out to be highly significant because sales seem to
# be high for users who go online alot. If you look at the average purchasees
# BEFORE the campaing started grouped by the total exposures of each subject
# you see that people who had more total exposures also had more purchases.
# On average people with no exposures bought 1.5, those with 1 bought 2.2, and
# those with 2 bought 3. It is this selection bias that the regression picks up.
# We can control for the selection bias by including fixed effects for the 
# total number of exposures. This way the regression can tease out differences
# in purchases correlated with more total exposures versus more treatment exposures.

# look at purchases by total exposures
cond_week0 = aggregate(week0 ~ total_exposures, data1, mean)
print(cond_week0)

# D) Adding total exposure fixed effects

# Placebo test
summary(lm(week0 ~ treatment_ad_exposures + total_exposures, data1))

# After adding total exposure fixed effects, the placebo test passes in that
# the treatment coefficient of -.001 (.005) when week0 is used is not significant.

# E) Estimated causal effect on week1

# It appears that seeing an ad increases estimated revenue by $0.133 (0.005).

# Treatment exposure with total exposure fixed effects
summary(lm(week1 ~ treatment_ad_exposures + total_exposures, data1))

# G) Effect on week 1 through 10

# add variable that sums week 1 through 10
data1$week1_10 = apply(data1[,paste0('week',1:10)], 1, sum)

# run model
summary(lm(week1_10 ~ treatment_ad_exposures + total_exposures, data1))

# H) Effect on week 2 through 10

# add variable that sums week 2 through 10
data1$week2_10 = apply(data1[,paste0('week',2:10)], 1, sum)

# run model
summary(lm(week2_10 ~ treatment_ad_exposures + total_exposures, data1))

# J) Compare product A vs B

# run model
summary(lm(week1 ~ treatment_ad_exposures*product_a + total_exposures, data1))


##################################################
### Problem 2 - Vietnam Draft

data2 = read.csv('ps5_no2.csv')

# A) Naive Regression
lm2a = lm(income ~ years_education, data2)
summary(lm2a)

# C) Education on Draft Rank

# make high_draft variable
data2$high_draft = data2$draft_number <= 80

# regression
lm2c = lm(years_education ~ high_draft, data2)

# clustered standard errors
cl(data2, lm2c, data2$draft_number)



# D) Income on Draft Rank

# regression
lm2d = lm(income ~ high_draft, data2)

# clustered standard errors
cl(data2, lm2d, data2$draft_number)


# E) Effect of Education on Income

lm2d$coef[2] / lm2c$coef[2]


# G) Differential Attrition

# count observations per draft number
data2agg = as.data.frame(table(data2$draft_number))

# reformat dataframe
colnames(data2agg) = c('draft_number', 'count')
data2agg$draft_number = as.numeric(data2agg$draft_number)

# make high_draft bool
data2agg$high_draft = data2agg$draft_number <= 80

# run regression
lm2g = lm(count ~ high_draft, data2agg)
summary(lm2g)


##################################################
### Problem 3 - FE 11.6

# copy info from book
data3 = data.frame(
  study = c('m2006', 'm2007', 'i2009'),
  n_c = c(26481, 348277, 15676),
  n_c_v = c(8755, 88960, 2600),
  n_t = c(5310, 12391, 9326),
  n_t_v = c(2123, 3791, 1936)
  )

# A & B) Estimate ATE, SE, and Precision of the studies
data3 = transform(data3, p_c = n_c_v/n_c, p_t = n_t_v/n_t)
data3 = transform(data3,
               ate = p_t - p_c,
               se = sqrt(p_t*(1-p_t)/n_t + p_c*(1-p_c)/n_c)
               )
data3$pre = (data3$se)^-2

# The ATEs
data3$ate

# The SEs
data3$se

# The Precision
data3$pre

# C) Precision Weighted Average ATE

ate3c = with(data3, sum(ate * pre) / sum(pre))
ate3c

# E) Pooled SE & CI

# SE
se3c = sqrt(1/sum(data3$pre))
se3c

# CI
ci3c = ate3c + se3c * 1.96 * c(-1,1)
ci3c

