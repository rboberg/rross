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
wcdf$conf1 <- setkey(unique(rankdt[,list(name,confid)],name))[wcdf$team1]$confid
wcdf$conf2 <-  setkey(unique(rankdt[,list(name,confid)],name))[wcdf$team2]$confid

###############
# needed to normalize across changes in point systems over time
wcUnq <- unique(data.table(
  team=c(wcdf$team1,wcdf$team2),
  year=c(wcdf$year,wcdf$year),
  raw=c(wcdf$raw1,wcdf$raw2),
  key=c("year","team")))

# Calculate z-score params for each world cup
# Actually... doesn't seem to work b/c distribution changed
# significantly when systme changed

zcalc <- wcUnq[,list(mean=mean(.SD$raw),sd=sd(.SD$raw)),by="year"]

transform(wcdf,strength1=(raw1 - zcalc[year==year]$mean )/zcalc[year==year]$sd)

# What about plotting change in score the month FIFA switched systems?
# Switch was from 2006-05-01 to 2006-07-01
# There will be noise because of the world cup in the interim

# Compare dates between point calculation change
changes <- acast(subset(rankdt,date %in% c("2006-07-01","2006-05-01")),name~date)
chgdf <- data.frame(changes)
names(chgdf) <- c("old","new")
chgdf <- chgdf[!is.na(chgdf$old) & !is.na(chgdf$new),]

# Get rid of some of the teams we don't care about (bad ones that don't make the WC)
chgdf <- subset(chgdf,new>=quantile(new,0.4))
chgdf$team <- row.names(chgdf)
chgdf$conf <- rankdt[J(chgdf$team,"2006-05-01")]$confid

# Does the change appear to be linear or log-linear?
ggplot(chgdf,aes(x=old,y=new,col=factor(conf))) + geom_point() + geom_text(aes(label=team))
ggplot(chgdf,aes(x=log(old),y=log(new),col=factor(conf))) + geom_point() + geom_text(aes(label=team))

# Looks like it's log-linear
# Compute the log-linear model
unqConf <- unique(chgdf$conf)
chgdf.lm <- cbind(chgdf,data.frame(sapply(unqConf,function(x){chgdf$conf==x})))

chglm <- lm(log(new)~log(old) + X1 + X2 + X3 + X4,chgdf.lm)
summary(chglm)

# Apply the model to the old points to creat a newPoints series
confWt <- rep(0,length(unique(rankdt$confid)))
names(confWt) <- rep(unique(rankdt$confid))
confWt[match(unqConf[1:(length(coef(chglm))-2)],names(confWt))] <- coef(chglm)[3:(length(coef(chglm)))]
 
rankdt[,newPoints:=round(exp(
                  log(points)*coef(chglm)[2] +
                    coef(chglm)[1] +
                    confWt[match(confid,names(confWt))]
                  ),0)
             ]

rankdt[date > "2006-05-01"]$newPoints <- rankdt[date > "2006-05-01"]$points

###############

wcdf$strength1 <- rankdt[J(wcdf$team1,paste0(wcdf$year,"-05-01"))]$newPoints
wcdf$strength2 <- rankdt[J(wcdf$team2,paste0(wcdf$year,"-05-01"))]$newPoints
wcdt <- data.table(wcdf)
wcdt$res <- ifelse(wcdt$score1 > wcdt$score2, 1, ifelse(wcdt$score1 < wcdt$score2,0,-1))

ptie <- sum(wcdt$score1==wcdt$score2)/nrow(wcdt)
winbase <- (1 - ptie)/2
winlm <- lm((res>0)-winbase~0+log(strength1/strength2),wcdt)
winbeta <- coef(winlm)[1]

tielm <- lm((res==0)-ptie~0+abs(log(strength1/strength2)),wcdt)
tiebeta <- coef(tielm)[1]

## Point Diff ##
revwcdt <- wcdt
revwcdt$score1 <- wcdt$score2
revwcdt$score2 <- wcdt$score1
revwcdt$strength1 <- wcdt$strength2
revwcdt$strength2 <- wcdt$strength1

ggplot(rbind(wcdt,revwcdt),aes(x=log(strength1/strength2),y=(score1-score2))) + geom_point() 
pdlm <- lm((score1-score2)*sign(strength1-strength2)~0+abs(log(strength1/strength2)),wcdt)
summary(pdlm)

pdiffFromStrength <- function(s1,s2,data,bandwidth=0.5,maxdiff=4,useabs=F){
  if(useabs){
    si <- abs(log(data$strength1/data$strength2))
    s <- abs(log(s1/s2))
    wi <- 1 - pnorm(abs(si-s)/bandwidth)
    di <- with(data,(score1-score2)*sign(strength1-strength2))
    di[di>maxdiff] <- maxdiff
  } else{
    si <- log(data$strength1/data$strength2)
    s <- log(s1/s2)
    wi <- 1 - pnorm(abs(si-s)/bandwidth)
    di <- with(data,(score1-score2))
    di[di>maxdiff] <- maxdiff
    di[di<(-maxdiff)] <- -maxdiff
  }
  
  ds <- min(di):max(di)
  rets <- sapply(ds,function(d){sum(wi*(di==d))})/sum(wi)
  names(rets) <- ds
  rets
}

logs <- (-70:70)/100
probs <- sapply(logs,function(s){pdiffFromStrength(exp(s),1,rbind(wcdt,revwcdt),bandwidth=0.2)})

colnames(probs) <- logs

probdf <- melt(probs,varnames=c("pdiff","logPower"),value.name="prob")
ggplot(probdf,aes(x=pdiff,y=prob,color=factor(logPower))) + geom_line()
ggplot(probdf,aes(x=logPower,y=prob,color=factor(pdiff))) + geom_line()

wlprob <- data.table(probdf)[,.SD[,list(
              pwin=sum(prob*(pdiff>0)),
              ptie=sum(prob*(pdiff==0)),
              ploss=sum(prob*(pdiff<0))
            )],by="logPower"]
ggplot(melt(wlprob,id.vars="logPower"),aes(x=logPower,y=value,color=variable)) + geom_line()



###############




###############


