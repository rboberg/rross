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
  "Algeria",#"Algeria"#"Russia"
  "Argentina",
  "Belgium",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "France",
  "Germany",
  "Greece",#"Greece"#"Cote d`Ivoire"
  "Mexico",
  "Netherlands",
  "Nigeria",
  "Switzerland",#"Switzerland"#"Ecuador"
  "Uruguay",#Uruguay#"Italy"
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


plot.df <- data.frame(
  team = c(team.df$team,team.df$team),
  site = rep(c("VN","538"),each=nrow(team.df)),
  miss = with(team.df,c(abs(adv-prob_vn),abs(adv-prob_538)))
  )

plot.df$team <- factor(plot.df$team,levels=with(team.df,team[order((abs(adv - prob_538) + abs(adv - prob_vn))/2)]))
plot.df$bigger <- sapply(1:nrow(plot.df),function(i){subset(plot.df,team==plot.df$team[i])$site[order(-subset(plot.df,team==plot.df$team[i])$miss)[1]]})

ggplot(plot.df,aes(x=team,y=miss,fill=site)) + geom_bar(stat='identity',position="dodge") + coord_flip()

ggplot(plot.df,aes(x=team,y=miss)) +
  geom_line(aes(group=team,color=bigger),size=1) +
  geom_point(aes(color=site),size=4) +
  coord_flip()


with(plot.df,apply(1)
