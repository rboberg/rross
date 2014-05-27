##########
## This script combines fifa world team ranking data and
## world cup result data


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

################
wcdf <- read.csv("clean_wc_results.csv",stringsAsFactors=F)
rankdf <- read.csv("fifa_rank_history.csv",stringsAsFactors=F)


################
## Make corrections if team names do not match
# Names in WC data that cannot be readily matched
c(wcdf$team1,wcdf$team2)[is.na(match(c(wcdf$team1,wcdf$team2),rankdf$name))]
# Unique list of fifa rank data
sort(unique(rankdf$name))

# Manual changes to FIFA rank data where it doesnot match
rankdf$name[grepl("Ivoire",rankdf$name)] <- "Cote d`Ivoire"
rankdf$name[grepl("USA",rankdf$name)] <- "United States"
rankdf$name[grepl("Trinidad",rankdf$name)] <- "Trinidad And Tobago"
rankdf$name[grepl("Republic of Ireland",rankdf$name)] <- "Ireland"
rankdf$name[grepl("China PR",rankdf$name)] <- "China"

################
#create datatable and remove any duplicates
rankdt <- unique(data.table(rankdf,key=c('name','date')))

wcdf$raw1 <- rankdt[J(wcdf$team1,paste0(wcdf$year,"-05-01"))]$points
wcdf$raw2 <- rankdt[J(wcdf$team2,paste0(wcdf$year,"-05-01"))]$points


###############
# Calculate z-score params for each world cup
# needed to normalize across changes in point systems over time
# Actually... doesn't seem to work b/c distribution changed

wcUnq <- unique(data.table(
  team=c(wcdf$team1,wcdf$team2),
  year=c(wcdf$year,wcdf$year),
  raw=c(wcdf$raw1,wcdf$raw2),
  key=c("year","team")))

zcalc <- wcUnq[,list(mean=mean(.SD$raw),sd=sd(.SD$raw)),by="year"]

transform(wcdf,strength1=(raw1 - zcalc[year==year]$mean )/zcalc[year==year]$sd)

# What about plotting change in score the month FIFA switched systems?
# Switch was from 2006-05-01 to 2006-07-01
# There will be noise because of the world cup in the interim

changes <- acast(subset(rankdt,date %in% c("2006-07-01","2006-05-01")),name~date)
chgdf <- data.frame(changes)
names(chgdf) <- c("old","new")
ggplot(chgdf,aes(x=log(old),y=log(new))) + geom_point()

###############
