##########
## This script takes a csv manually created in Excel from www.worldcup-history.com
## The script turns it in to a structured history of World Cup results
## Saves as csv


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Soccer/2014/WorldCup/"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org", library(data.table)))

##################
rawwc <- read.csv("wc history raw.csv",stringsAsFactors=F)

#string form is:
#(/s?)team1(//*?)(/s*)-(/s*)team2(/s*)score1-score2(/s*)Date&VenueInfo

rawwc$result <- gsub("^\\s*","",rawwc$result,perl=T)
temp1 <- t(sapply(strsplit(rawwc$result,"\\s+-\\s+"),function(x){c(x[1],x[2])}))
pk1 <- grepl("\\*",temp1[,1])
team1 <- gsub("\\*","",temp1[,1])

temp2 <- t(sapply(strsplit(temp1[,2],"\\s\\s+"),function(x){c(x[1],x[2],x[3])}))
pk2 <- grepl("\\*",temp2[,1])
team2 <- gsub("\\*","",temp2[,1])

temp3 <- t(sapply(strsplit(temp2[,2],"[- ]"),function(x){c(x[1],x[2],length(x)==3)}))
score1 <- as.numeric(temp3[,1])
score2 <- as.numeric(temp3[,2])
et <- as.logical(temp3[,3])

wcdf <- cbind(rawwc,
              data.frame(
                team1,
                team2,
                score1,
                score2,
                pk1,
                pk2,
                et,
                stringsAsFactors=F
                )
              )

write.csv(wcdf,"clean_wc_results.csv",row.names=F)
