##########
## This script analyzes world cup predictions vs outcomes
library(MASS)
library(ggplot2)
library(grid)

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


#clean up some team names
team.df$team[team.df$team=='Bosnia and Herzegovina'] <- 'Bosnia'
team.df$team[team.df$team=='Korea Republic'] <- 'Korea'
team.df$team[team.df$team=='United States'] <- 'USA'

plot.df <- data.frame(
  team = c(team.df$team,team.df$team),
  site = rep(c("VN","538"),each=nrow(team.df)),
  miss = with(team.df,c(abs(adv-prob_vn),abs(adv-prob_538)))
  )

plot.df$team <- factor(plot.df$team,levels=with(team.df,team[order((abs(adv - prob_538) + abs(adv - prob_vn))/2)]))
plot.df$bigger <- sapply(1:nrow(plot.df),function(i){subset(plot.df,team==plot.df$team[i])$site[order(-subset(plot.df,team==plot.df$team[i])$miss)[1]]})
plot.df$better <- sapply(1:nrow(plot.df),function(i){subset(plot.df,team==plot.df$team[i])$site[order(subset(plot.df,team==plot.df$team[i])$miss)[1]]})



#ggplot(plot.df,aes(x=team,y=miss,fill=site)) + geom_bar(stat='identity',position="dodge") + coord_flip()

gp <- ggplot(plot.df,aes(x=team,y=miss)) +
        geom_line(aes(group=team,color=better),size=1) +
        geom_point(aes(color=site),size=4) +
        scale_color_manual(values=c(rgb(1,0,0),rgb(0,120/256,240/256)),name = "") +
        coord_flip() +
        ylab("") + xlab('') +
        theme(legend.position = 'top')
gp

png(file="images/group_stage_outcome.png",bg="transparent",family="helvetica",width=400,height=1200,res=120)
gp + theme(plot.margin=unit(c(0,1,0,0),"mm"))
dev.off()

ggsave(file="images/group_stage_outcome_ggsave.png",plot=gp,width=4,height=9)


##################

bar.df <- data.frame(
            miss =
              c(
                with(team.df, mean(abs(adv - prob_538))),
                with(team.df, mean(abs(adv - prob_vn))),
                0.5
                ),
            root.mse =
              c(
                with(team.df, mean((adv - prob_538)^2)^0.5),
                with(team.df, mean((adv - prob_vn)^2)^0.5),
                0.5
                ),
            se =
              c(
                with(team.df, sd((adv - prob_538)^2)^0.5),
                with(team.df, sd((adv - prob_vn)^2)^0.5),
                0.5
                ),
            site=c('FiveThirtyEight','VividNumeral', 'EqualChance')
            )

barT <- ggplot(bar.df) +
  geom_bar(aes(x=site,y=miss,fill=site),stat='identity') +
  scale_fill_manual(values=c(FiveThirtyEight=rgb(1,0,0),VividNumeral=rgb(0,120/256,240/256),EqualChance=rgb(.5,.5,.5)),name = "") + 
  ylab("Average Prediction Error")+xlab("") + 
  theme(legend.position='none') + coord_cartesian(ylim=c(-.025,0.525))
  
barF <- ggplot(subset(bar.df,site!='EqualChance')) +
  geom_bar(aes(x=site,y=miss,fill=site),stat='identity') +
  scale_fill_manual(values=c(FiveThirtyEight=rgb(1,0,0),VividNumeral=rgb(0,120/256,240/256),EqualChance=rgb(.5,.5,.5)),name = "") + 
  ylab("Average Prediction Error")+xlab("") + 
  theme(legend.position='none') + 
  coord_cartesian(ylim=c(0.416125,0.41626)) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())

png(file="images/wc_group_bar_false.png",bg="transparent",family="helvetica",width=240,height=320,res=80)
barF
dev.off()
png(file="images/wc_group_bar_true.png",bg="transparent",family="helvetica",width=304,height=320,res=80)
barT
dev.off()

bar.df$miss[2] - bar.df$miss[1]