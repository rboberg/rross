##########
## This script combines fifa world team ranking data and
## world cup result data


###########################
##TO RUN: Set working Directory to the Location of the Project
#HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
HOMEDIR <- "C:/Users/Ross/Documents/GitHub/rross"
setwd(paste0(HOMEDIR,"/Soccer/2014/WorldCup/"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org"))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org"))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org"))
tryCatch(library(quantreg), error = function(e) install.packages("quantreg", repos = "http://cran.r-project.org"))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org"))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org"))


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
wcdf$conf1 <- setkey(unique(rankdt[,list(name,confid)]),name)[wcdf$team1]$confid
wcdf$conf2 <-  setkey(unique(rankdt[,list(name,confid)]),name)[wcdf$team2]$confid

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

transform(wcdf,power1=(raw1 - zcalc[year==year]$mean )/zcalc[year==year]$sd)

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

wcdf$power1 <- rankdt[J(wcdf$team1,paste0(wcdf$year,"-05-01"))]$newPoints
wcdf$power2 <- rankdt[J(wcdf$team2,paste0(wcdf$year,"-05-01"))]$newPoints
wcdt <- data.table(wcdf)
wcdt$res <- ifelse(wcdt$score1 > wcdt$score2, 1, ifelse(wcdt$score1 < wcdt$score2,0,-1))

ptie <- sum(wcdt$score1==wcdt$score2)/nrow(wcdt)
winbase <- (1 - ptie)/2
winlm <- lm((res>0)-winbase~0+log(power1/power2),wcdt)
winbeta <- coef(winlm)[1]

tielm <- lm((res==0)-ptie~0+abs(log(power1/power2)),wcdt)
tiebeta <- coef(tielm)[1]

## Point Diff ##
revwcdt <- wcdt
revwcdt$score1 <- wcdt$score2
revwcdt$score2 <- wcdt$score1
revwcdt$power1 <- wcdt$power2
revwcdt$power2 <- wcdt$power1

ggplot(rbind(wcdt,revwcdt),aes(x=log(power1/power2),y=(score1-score2))) + geom_point() 
pdlm <- lm((score1-score2)*sign(power1-power2)~0+abs(log(power1/power2)),wcdt)
pdlm <- lm((score1-score2)~0+log(power1)+log(power2),rbind(wcdt,revwcdt))
summary(pdlm)

#how many points are usually scored given a given point differential
totalOnPdiff <- lm(score1~pdiff,transform(rbind(wcdt,revwcdt),pdiff=score1-score2))
summary(totalOnPdiff)

#Use a kernel function to estimate point differential probabilities
#
pdiffFrompower <- function(s1,s2,data,bandwidth=0.5,maxdiff=4,useabs=F){
  if(useabs){
    si <- abs(log(data$power1/data$power2))
    s <- abs(log(s1/s2))
    wi <- 1 - pnorm(abs(si-s)/bandwidth)
    di <- with(data,(score1-score2)*sign(power1-power2))
    di[di>maxdiff] <- maxdiff
  } else{
    si <- log(data$power1/data$power2)
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

logs <- (-65:65)/100
probs <- sapply(logs,function(s){pdiffFrompower(exp(s),1,rbind(wcdt,revwcdt),bandwidth=0.2)})

colnames(probs) <- logs

probdf <- melt(probs,varnames=c("pdiff","logPower"),value.name="prob")
ggplot(probdf,aes(x=pdiff,y=prob,color=factor(logPower))) + geom_line()
ggplot(probdf,aes(x=logPower,y=prob,color=factor(pdiff))) + geom_line()

 
windf <- data.table(probdf)[,outcome:=ifelse(pdiff < 0, "LOSS", ifelse(pdiff>0, "WIN", "TIE"))][,.SD[,list(prob=sum(prob))],by=c('logPower','outcome')]
ggplot(windf,aes(x=logPower,y=prob,col=factor(outcome))) + geom_line()

logPower = 0.111
logPower = -0.111
logPower = 0.8
logPower = -0.8


probfunc <- function(logPower, data){
  powers <- unique(data$logPower)
  pow2 <- c(ifelse(logPower<min(powers),min(powers),max(powers[logPower>=powers])),ifelse(logPower>max(powers),max(powers),min(powers[logPower<=powers])))
  if(pow2[1] == pow2[2]){
    w1 <- 1 
  }else{
    w1 <- abs(logPower-pow2[1])/(abs(logPower-pow2[2]) + abs(logPower-pow2[1]))
  }
  probs <- subset(data,logPower==pow2[1])$prob * w1 +  subset(data,logPower==pow2[1])$prob *(1 - w1)
  names(probs) <- subset(data,logPower==pow2[1])$pdiff
  return(probs)
}

sampleScore <- function(logPowers,data,rseed=runif(length(c(logPowers)))){
  minwhich <- function(x){min(which(x))}
  allprobs <- apply(sapply(logPowers,function(x){probfunc(x,data)}),2,cumsum)
  allpdiff <- as.numeric(rownames(allprobs))[
                  apply(matrix(rseed,ncol=length(c(logPowers)),nrow=nrow(allprobs),byrow=T) <= allprobs,2,minwhich)
                  ]
  #This is definitley a slightly biased, suboptimal way of
  #of translating point differentials in to scores
  #but for the sake of parsimony I'm gong to do ti
  s1Int <- 1.2
  s1Slp <- 0.5
  s1RSE <- 0.75
  score1 <- pmax(pmax(round(s1Int + allpdiff*s1Slp + s1RSE * rnorm(length(logPowers))),0),allpdiff)
  score2 <- score1-allpdiff
  return(data.frame(score1,score2))
}
probfunc(0,probdf)
logPowers <- c(0.11,-0.11,0.8,-0.8,0)
temp <- sampleScore(rep(0.5,10000),probdf)

#US probs vs Germany
usgerm <- sampleScore(rep(log(1015/1340),10000),probdf)
with(usgerm,c(pwin = mean(score1-score2>0), ploss = mean(score1-score2<0), ptie = mean(score1-score2==0)))
apply(usgerm,2,mean)

#function that takes team names, powers, and iterations
#and simulates the group stage among those teams

groupSim <- function(teams,powers,iter=100){
  #set up games
  n <- length(teams)
  gs <- factorial(n)/(factorial(n-2)*2)
  
  combos <- matrix(rep(combn(teams,2),iter),ncol=2,byrow=T)
  simdt <- data.table(
    sim=c(rep(1:iter,each=gs)),
    t1=c(combos[,1]),
    t2=c(combos[,2])
    )
  simdt$p1 <- powers[match(simdt$t1,teams)]
  simdt$p2 <- powers[match(simdt$t2,teams)]
  
  #simulate games
  scores <- sampleScore(log(simdt$p1/simdt$p2),probdf)
  simdt <- cbind(simdt,scores)
  
  #duplicate games with teams swapped
  revdt <- simdt
  revdt <- transform(revdt,t1=simdt$t2,t2=simdt$t1,p1=simdt$p2,p2=simdt$p1,score1=simdt$score2,score2=simdt$score1)
  simdt <- rbind(simdt,revdt)
  
  #form arrays out of game results
  #points for each team
  pfA <- acast(simdt, sim~t1~t2, value.var="score1")
  #points against each team
  paA <- acast(simdt, sim~t2~t1, value.var="score1")
  #point differential for each team
  pdA <- pfA-paA
  
  #function to assign points to pt differentials
  ptfunc <- function(x){ifelse(x>0,3,ifelse(x==0,1,0))}
  
  #point differentials
  ptA <- ptfunc(pdA)
  
  #aggregate statistics for each team in each iteration
  pts <- apply(ptA,c(1,2),sum,na.rm=T)
  pds <- apply(pdA,c(1,2),sum,na.rm=T)
  pfs <- apply(pfA,c(1,2),sum,na.rm=T)
  
  #preliminary ranking algorithm
  #rank on points, then point differential, then points scored
  rnksMix <- t(apply(pts,1,rank)) + t(apply(pds,1,rank))/n +  t(apply(pfs,1,rank))/n/n
  rnksF <- t(apply(rnksMix,1,rank))
  rnknames <- colnames(rnksF)
  
  #are there still any ties?
  checkTie <- function(x){length(x)!=length(unique(x))}
  ties <- which(apply(rnksF,1,checkTie))
  
  #additionall tie breaking
  if(length(ties) > 0){
    for(i in ties){
      rnki <- rnksF[i,rnknames]
      tieM <- (matrix(rnki,n,n,dimnames=list(rnknames,rnknames)) - t(matrix(rnki,n,n)))
      diag(tieM) <- NA
      tiebreak <- subset(melt(tieM,varnames=c('t1','t2'),stringsAsFactors=F),value==0)
      
      #tie break based on games within teams
      tiebreak$value <-   sapply(1:nrow(tiebreak),function(x){
          pdA[i,as.character(tiebreak[x,]$t1),as.character(tiebreak[x,]$t2)]
        })
      
      #draw lots
      if(length(which(tiebreak$value==0))>0){
        drawLots <- sample(1:4) 
        tiebreak$value <- drawLots[match(as.character(tiebreak$t1),rnknames)] - drawLots[match(as.character(tiebreak$t2),rnknames)]
      }
      for(x in 1:nrow(tiebreak)){
        tieM[as.character(tiebreak[x,]$t1),as.character(tiebreak[x,]$t2)] <- tiebreak[x,]$value
      }
      rnksF[i,rnknames] <- apply(tieM>0,1,sum,na.rm=T)[rnknames]+1
    }
  }
  
  return(
    list(
      groupRank = rnksF, #high is good
      throughProb = apply(rnksF>2,2,mean),
      gameDT = simdt, #data table of game results
      pointsFor = pfA, #array of points for
      pointsAgainst = paA, #array of points against
      pointDiff = pdA #array of point differential
      )
    )
}

niter = 10000

allThroughProb <- data.frame()
teams <- c("Brazil","Croatia","Mexico","Cameroon")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="A"))

teams <- c("Spain","Netherlands","Chile","Australia")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="B"))

teams <- c("Colombia","Greece","Cote d`Ivoire","Japan")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="C"))

teams <- c("Uruguay","Costa Rica","England","Italy")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="D"))

teams <- c("Switzerland","Ecuador","France","Honduras")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="E"))

teams <- c("Argentina","Bosnia and Herzegovina","Iran","Nigeria")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="F"))

teams <- c("United States","Germany","Ghana","Portugal")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="G"))

teams <- c("Belgium","Algeria","Russia","Korea Republic")
powers <- rankdt[J(teams,max(date))]$points
simResults <- groupSim(teams,powers,iter=10000)
simResults$throughProb
allThroughProb <- rbind(allThroughProb,data.frame(prob=simResults$throughProb,group="H"))

allThroughProb$power <- rankdt[J(row.names(allThroughProb),max(date))]$points
allThroughProb$team <- row.names(allThroughProb)

allThroughProb[order(-allThroughProb$prob),]

ggplot(allThroughProb,aes(x=power,y=prob,col=factor(group))) + geom_point() + geom_text(aes(label=team))
ggplot(allThroughProb,aes(x=log(power),y=prob,col=factor(group))) + geom_point() + geom_text(aes(label=team))

#####################
#### make JSON of "through" probabilities for scatterplot
prob.json <- toJSON(lapply(1:nrow(allThroughProb),function(x){allThroughProb[x,]}))
writeLines(prob.json,"through_prob.json")
#####################

###############
#### CHECK "DEATHIEST" GROUPS FROM LAST 3 WC

rddt <- unique(
  wcdt[,.SD[,list(team=c(team1,team2),power=c(power1,power2))],by=c("year","rd")]
  )
thiswc <- with(allThroughProb,data.table(year=2014,rd=as.character(group),team=team,power))
rddt <- rbind(rddt,thiswc)

groups <- c('A','B','C','D','E','F','G','H')
groupdt <- rddt[rd %in% groups,list(power=sum(.SD$power)),by=c("year","rd")]

groupdt[,wcPower:=sum(.SD$power),by="year"]
groupdt[,pctPower:=power/wcPower]

groupdt[order(-groupdt$pctPower)]

rddt[year==2002]

##################
## Get teams in each group and year
grpteams <- data.frame()
for(i in 1:nrow(groupdt)){
  yi = groupdt$year[i]
  grpi = groupdt$rd[i]
  infoi <- unique(rddt[year==yi & rd==grpi])
  #get teams, sorted by descending power
  tmsi <- infoi$team[order(-infoi$power)]
  if(yi < 2014){
    r16i <- tmsi %in% unique(unlist(c(wcdt[year==yi & rd=='R16',list(team1,team2)])))
    tmsi <- paste0(tmsi,ifelse(r16i,"*",""))
  }
  names(tmsi) <- c('team1','team2','team3','team4')
  grpteams <- rbind(grpteams,data.frame(year=yi,rd=grpi,t(tmsi),stringsAsFactors=F))
}

groupDetail <- merge(groupdt,grpteams,by=c('year','rd'))

unique(rddt[year==yi & rd==grpi])

########################
#### make csv for HTML table
write.csv(groupDetail[order(-groupDetail$pctPower),list(year,rd,pct=pctPower,team1,team2,team3,team4)][1:8],'god_top.csv',row.names=F)
write.csv(groupDetail[order(groupDetail$pctPower),list(year,rd,pct=pctPower,team1,team2,team3,team4)][1:8],'god_bottom.csv',row.names=F)
###############


