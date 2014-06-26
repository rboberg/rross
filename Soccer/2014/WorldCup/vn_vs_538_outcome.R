##########
## This script analyzes world cup predictions vs outcomes
library(MASS)
library(ggplot2)

###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/GitHub/rross"
setwd(paste0(HOMEDIR,"/Soccer/2014/WorldCup/"))

team.df <- read.csv('vn_vs_538_predictions.csv',stringsAsFactors=F)

advance <- c(
  "Algeria",
  "Argentina",
  "Belgium",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "France",
  "Germany",
  "Greece",#"Cote d`Ivoire"#"Greece"
  "Mexico",
  "Netherlands",
  "Nigeria",
  "Ecuador",#"Switzerland"#"Ecuador"
  "Uruguay",
  "United States"#"United States"#"Ghana"
  )

team.df$adv = team.df$team %in% advance

glm538 <- glm(adv~prob_538,data=team.df,family=binomial)
glmvn <- glm(adv~prob_vn,data=team.df,family=binomial)

summary(glm538)
summary(glmvn)

ggplot(team.df) + geom_boxplot(aes(x=adv,y=prob_vn),col='blue',fill=NA) + geom_boxplot(aes(x=adv,y=prob_538),col='red',fill=NA)


with(team.df, mean(abs(adv - prob_538)))
with(team.df, mean(abs(adv - prob_vn)))

ggplot(team.df) +
  geom_histogram(aes(abs(adv - prob_538)),binwidth=.1,alpha=0.25,fill='red') +
  geom_histogram(aes(abs(adv - prob_vn)),binwidth=.1,alpha=0.25,fill='blue') +
  xlab("Absolute Miss")


