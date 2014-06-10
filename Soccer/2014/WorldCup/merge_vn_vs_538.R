##########
## This script combines my probabilities and Nate Silver's in to a single CSV

library(plyr)

###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/GitHub/rross"
setwd(paste0(HOMEDIR,"/Soccer/2014/WorldCup/"))


vndf <- read.csv('20140610_through_prob.csv',stringsAsFactors=F,row.names=1)
nsdf <- read.csv('predictions538.csv',stringsAsFactors=F)

#check where team names differ
#my names
vndf$team[is.na(match(vndf$team,nsdf$TEAM))]
#538 names
nsdf$TEAM[is.na(match(nsdf$TEAM,vndf$team))]

nsdf$TEAM[nsdf$TEAM=='Bosnia'] <- "Bosnia and Herzegovina"
nsdf$TEAM[nsdf$TEAM=='USA'] <- "United States"
nsdf$TEAM[nsdf$TEAM=='South Korea'] <- "Korea Republic"
nsdf$TEAM[nsdf$TEAM=='Ivory Coast'] <- "Cote d`Ivoire"

#merge
combodf <- merge(vndf,nsdf,by.x=c('team','group'),by.y=c('TEAM','GROUP'))

#rename
combodf <- rename(combodf,c(prob='prob_vn',SIXTEEN='prob_538'))

#write to file
combo.json <- toJSON(lapply(1:nrow(combodf),function(x){combodf[x,]}))
writeLines(combo.json,"vn_vs_538.json")
write.csv(combodf,'vn_vs_538.csv',row.names=F)

