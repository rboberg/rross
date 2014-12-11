####################2
# ROSS BOBERG
# FIELD EXPERIMENTS
# PROBLEM SET 5
####################

####################
### Functions & Libraries
library(ggplot2)

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

The placebo test turns out to be highly significant because sales seem to
be high for users who go online alot. If you look at the average purchasees
BEFORE the campaing started grouped by the total exposures of each subject
you see that people who had more total exposures also had more purchases.
On average people with no exposures bought 1.5, those with 1 bought 2.2, and
those with 2 bought 3. It is this selection bias that the regression picks up.
We can control for the selection bias by including fixed effects for the 
total number of exposures. This way the regression can tease out differences
in purchases correlated with more total exposures versus more treatment exposures.

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

# E) Effect on week 2 through 10

# add variable that sums week 2 through 10
data1$week2_10 = apply(data1[,paste0('week',2:10)], 1, sum)

# run model
summary(lm(week2_10 ~ treatment_ad_exposures + total_exposures, data1))

# J) Compare product A vs B

# run model
summary(lm(week1 ~ treatment_ad_exposures*product_a + total_exposures, data1))
