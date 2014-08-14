###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2014/Fantasy/FantasyPros"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org", library(data.table)))

#################
#### FUNCTIONS

# Function to calculate the needs of each team
# result factors in to draft decisions
calc.need <- function(teams, pos.start, pos.bench, start.mult, bench.mult){
  
  max.need <- (pos.start * start.mult + pos.bench * bench.mult)
  need.start <- pmax((teams - pos.bench) * start.mult,0)
  need.bench <- teams - need.start
  need <- pmax((need.start * start.mult + need.bench * bench.mult),0)
  return(need/max.need)

}


###
# Function to run simulations of the draft baesd
# with some draft picking rules.
# May need to recreate this in python or js to
# make it web accesible

#test for Team1
#start.team <- 1
#n.picks <- 144
#n.iter <- 1000
#initial.teams <- teams
#snake <- T
#fwd <- T
#adp.source <- 'ESPN.ADP'
#adp.wt = 0.8

draft.sim <- function(
  allDF,
  initial.teams,
  pos.start,
  pos.bench,
  start.mult,
  bench.mult,
  n.picks = 144,
  n.iter = 1000,
  start.team = 2,
  snake = T,
  fwd = T,
  adp.wt = 0.8,
  adp.source = 'ESPN.ADP'
  ){
  
  iterDF <- data.frame(c())
  
  for(i in 1:n.iter){
    
    loopDF <- allDF[,c('Player','POS',adp.source,'FPTS')]
    draftDF <- data.frame(c())
    
    team.j <- start.team
    teams.j <- initial.teams
    
    for(j in 1:n.picks){
      pos.need <- calc.need(teams.j, pos.start, pos.bench, start.mult, bench.mult)
      wt <- pos.need[match(loopDF$POS, pos.name),team.j] * adp.wt^(loopDF[,adp.source])
      loopDF$prob <- wt/sum(wt,na.rm=T)
      draft <- sample(1:nrow(loopDF),1,prob=loopDF$prob,replace=T)
      
      teams.j[loopDF[draft,'POS'],team.j] <- teams.j[loopDF[draft,'POS'],team.j] - 1
      
      draftDF <- rbind(draftDF,data.frame(loopDF[draft,],team=team.j,pick=j))
      loopDF <- loopDF[-draft,]
      
      if(snake & (!fwd | team.j == ncol(teams.j))){
        if((team.j == ncol(teams.j) & fwd) | (team.j == 1 & !fwd)){
          team.j <- team.j
          fwd <- !fwd
        } else{
          team.j <- team.j - 1
        }
      }else {
        team.j <- (team.j %% ncol(teams.j)) + 1
      }
    }
    #draftDF[order(draftDF$team,draftDF$POS),]
    
    iterDF <- rbind(iterDF,transform(draftDF,iter=i))
  }
  return(iterDF)
}


# This function takes a data.frame of several simulated drafts
# and returns the expected fantasy points of the best available
# player at each position for each pick in the draft
expectedMax <- function(iterDF){
  
  maxDF <- data.frame(c())
  picks <- max(iterDF$pick)
  
  for(pos in pos.name){
    posAr <- acast(subset(iterDF,POS==pos),Player~iter, value.var='pick')
    posPts <- allDF$FPTS[match(rownames(posAr),allDF$Player)]
    names(posPts) <- rownames(posAr)
    
    suppressWarnings(
      posMax <-  sapply(
                  1:ncol(posAr),
                  function(i){
                    sapply(
                      1:picks,
                      function(j){
                        max(posPts[names(posAr[posAr[,i]>=j,i])],na.rm=T)
                      }
                      )
                  }
                  )
    )
    
    posMax[is.infinite(posMax)] <- NA
           
    maxDF <- rbind(
      maxDF,
      data.frame(
        pick = 1:picks,
        position = pos,
        maxPts = apply(posMax,1,mean,na.rm=T)
        )
      )
  }
  
  maxAr <- acast(maxDF,pick~position, value.var='maxPts')

  return(list(maxAr = maxAr, maxDF = maxDF))
}



######################
#####################

# Load CSV
allDF <- read.csv('fpro2014.csv', row.names = 1, stringsAsFactors = F)

# Number of Teams in Simulation
nteam <- 12

# Positions in simulations
pos.name <- c('QB','RB','WR','TE')

# Roster specifications
pos.start <- c(1,2.5,2.5,1)
pos.bench <- c(.5,2.5,2.5,.5)

# Coefficients for weighting starters vs bench in draft decisions
start.mult <- 1
bench.mult <- 0.05

# set up rosters for simulations
teams <- matrix(
  pos.start + pos.bench,
  nrow=4,
  ncol=nteam,
  dimnames=list(
    pos.name,
    paste0('Team',1:nteam)
    )
  )

# remove any that weren't drafted on ESPN
allDF <- allDF[!is.na(allDF$ESPN.ADP),]

# remove positions I'm not using
allDF <- allDF[allDF$POS %in% pos.name,]

n.picks <- 144

# run draft simulation
iterDF <-  draft.sim(
              allDF=allDF,
              initial.teams=teams,
              pos.start=pos.start,
              pos.bench=pos.bench,
              start.mult=start.mult,
              bench.mult=bench.mult,
              n.picks = n.picks,
              n.iter = 1000,
              start.team = 1,
              snake = T,
              fwd = T,
              adp.wt = 0.8,
              adp.source = 'ESPN.ADP'
              )

# check what percent of drafts each player was taken in
# helps to highlight any major problems w/ draft model
taken <- table(iterDF$Player)/max(iterDF$iter)

### Try to show chart of expected change in point for each position
# at each point in the draft

# calculate the expected max points available
maxDataStruct <- expectedMax(iterDF)

# pull the data out of the struct
maxAr <- maxDataStruct$maxAr
maxDF <- maxDataStruct$maxDF

# calculate 1, 2, 3, 4 round lags
changeDF <- data.frame(c())
for(lag in c(12,24,36,48)){
  changeAr <- array(NA,dim=dim(maxAr),dimnames=dimnames(maxAr))
  changeAr[1:(n.picks-lag),] <- maxAr[1:(n.picks-lag),] - maxAr[(1+lag):n.picks,]
  changeDF <- rbind(
    changeDF,
    data.frame(melt(changeAr,varnames=c('pick','position')),lag=lag)
    )
}

# plot lags
ggplot(changeDF,aes(x=pick,y=value,color=position)) + geom_line() + facet_wrap(~lag)

# calculate expected change in points for each pick in a snake draft
draft.seq <- ((12:1)*2)-1

pickDF <- maxDF
pickDF$wait <- draft.seq
pickDF$team <- c(1:12,12:1)
pickDF$value <- NA

for(i in 1:nrow(pickDF)){
   rowi <- pickDF[i,]
   nexti <- subset(pickDF,position == rowi$position & pick == (rowi$pick + rowi$wait))
   if(nrow(nexti) > 0){
     pickDF$value[i] <- rowi$maxPts - nexti$maxPts
   }
}

# Reorganize data
valueDF <- dcast(pickDF, pick+team~position, value.var='value')

# Calculate value of each pick with weightings for each position
# 75% for highest value position, 25% for second, 0% for the rest
valueDF$value <- apply(valueDF[,pos.name],1,function(x){sum(x[order(-x)]*c(0.75,0.25,0,0),na.rm=T)})

# Calculate average value for each draft position
pickValue <- aggregate(value ~ team, data = valueDF, FUN = sum)
pickValue$plusMinus <- pickValue$value-mean(pickValue$value)


################
## Look at simulated rosters

rosters <- data.frame(c())
points <- data.frame(c())

for(i in 1:max(iterDF$iter)){
  for(j in 1:max(iterDF$team)){
    allpicks <- subset(iterDF, iter==i & team==j)
    allpicks.order <- allpicks[order(-allpicks$FPTS),]
    roster.ij <- data.frame(
                    QB = allpicks.order$Player[allpicks.order$POS=='QB'][1],
                    RB1 = allpicks.order$Player[allpicks.order$POS=='RB'][1],
                    RB2 = allpicks.order$Player[allpicks.order$POS=='RB'][2],
                    WR1 = allpicks.order$Player[allpicks.order$POS=='WR'][1],
                    WR2 = allpicks.order$Player[allpicks.order$POS=='WR'][2],
                    TE = allpicks.order$Player[allpicks.order$POS=='TE'][1],
                    stringsAsFactors = F
                    )
    roster.ij$FLEX <- allpicks.order$Player[
      allpicks.order$POS %in% c('WR','TE','RB') &
        !(allpicks.order$Player %in% roster.ij[1,])
      ][1]
    
    roster.ij$BENCH1 <- allpicks.order$Player[
      allpicks.order$POS %in% c('WR','TE','RB') &
        !(allpicks.order$Player %in% roster.ij[1,])
      ][1]
    
    roster.ij$BENCH2 <- allpicks.order$Player[
      allpicks.order$POS %in% c('WR','TE','RB') &
        !(allpicks.order$Player %in% roster.ij[1,])
      ][1]
    
    points.ij <- roster.ij
    points.ij[1,] <- allpicks$FPTS[match(roster.ij,allpicks$Player)]
    
    rosters <- rbind(rosters, transform(roster.ij,iter=i,team=j))
    points <- rbind(points, transform(points.ij,iter=i,team=j))
    
  }
  
}

pointsDT <- data.table(apply(points,2,as.numeric),key=c('iter','team'))
pointsDT[,lapply(.SD,mean,na.rm=T),by=team]

pmDT <- pointsDT[,lapply(.SD[,list(QB,RB1,RB2,WR1,WR2,FLEX,BENCH1,BENCH2)],function(x){x-mean(x,na.rm=T)})]
pmDT$Total <- apply(pmDT[,list(QB,RB1,RB2,WR1,WR2,FLEX,BENCH1,BENCH2)],1,sum)
pmDT$iter <- pointsDT$iter
pmDT$team <- pointsDT$team
pmSummary <- pmDT[,lapply(.SD,mean,na.rm=T),by=team]

pmSummary$Total <- apply(pmSummary[,list(QB,RB1,RB2,WR1,WR2,FLEX,BENCH1,BENCH2)],1,sum)

pmSummary

#Get the most probably players at each position
teamList <- list()
for(i in 1:max(rosters$team)){
  subros <- subset(rosters,team==i)
  plist <- sapply(
            c('QB','RB1','RB2','WR1','WR2','TE','FLEX','BENCH1','BENCH2'),
            function(p){
              posTable <- table(subros[,p])
              return(sort(posTable,dec=T)[1:5]/nrow(subros))
            },
            simplify=F
            )
  teamList[[i]] <- plist
}

teamList[1]

#Look at what successful teams did
teami <- 6
top <- 200
subDT <- pmDT[1:20]
temp <- pmDT[,list(Total,rank(-Total),rank(-Total)<100),by='team']

topTeams <- pmDT[,.SD[rank(-Total)<top],by='team']
bottomTeams <- pmDT[,.SD[rank(Total)<top],by='team']

iterDT <- data.table(iterDF,key=c('iter','team','pick'))
topDrafts <- iterDT[J(topTeams$iter,topTeams$team)]
bottomDrafts <- iterDT[J(bottomTeams$iter,bottomTeams$team)]

table(topDrafts[team==1][pick %in% c(1)]$POS)/200
table(bottomDrafts[team==1][pick %in% c(1)]$POS)/200

table(topDrafts[team==1][pick %in% c(24,25)]$POS)/400
table(bottomDrafts[team==1][pick %in% c(24,25)]$POS)/400

table(topDrafts[team==1][pick %in% c(48,49)]$POS)/400
table(bottomDrafts[team==1][pick %in% c(48,49)]$POS)/400

table(topDrafts[team==1][pick %in% c(72,73)]$POS)/400
table(bottomDrafts[team==1][pick %in% c(72,73)]$POS)/400

pickv <- 1:max(iterDT$pick)
best <- data.frame(t(sapply(
  pickv,
  function(x){
      retv <- table(topDrafts[pick == x]$POS)/200
      retv <- retv[c('QB','RB','WR','TE')]
      names(retv) <- c('QB','RB','WR','TE')
      retv[is.na(retv)] <- 0
      retv
    }
  )))

worst <- data.frame(t(sapply(
  pickv,
  function(x){
      retv <- table(bottomDrafts[pick == x]$POS)/200
      retv <- retv[c('QB','RB','WR','TE')]
      names(retv) <- c('QB','RB','WR','TE')
      retv[is.na(retv)] <- 0
      retv
    }
  )))
bwDiff <- best-worst

best$pick <- pickv
worst$pick <- pickv
bwDiff$pick <- pickv

bestDF <- melt(best,id.var='pick',variable.name='POS')
ggplot(bestDF,aes(x=pick,y=value,color=POS)) + geom_line()

worstDF <- melt(worst,id.var='pick',variable.name='POS')
ggplot(worstDF,aes(x=pick,y=value,color=POS)) + geom_line()

bwDF <- melt(bwDiff,id.var='pick',variable.name='POS')
ggplot(bwDF,aes(x=pick,y=value,color=POS)) + geom_line(alpha=0.2) + geom_smooth(method="loess",span=0.2,se=F)


aggregate(Total~team,pmDT,FUN=quantile)

#Diff between AP and SJ = 95
#AP drafted top 3, SJ drafted 69 on ESPN ADP,  85 on average
transform(subset(allDF[order(-allDF$FPTS),],POS=='RB'),rank = order(-FPTS))

#Diff between JG and MB = 95
#JG drafted 10 overall, MB drafted 133 ESPN, 131 on average
transform(subset(allDF[order(-allDF$FPTS),],POS=='TE'),rank = order(-FPTS))

#Diff between JG and MB = 95
#JG drafted 10 overall, MB drafted 133 ESPN, 131 on average
transform(subset(allDF[order(-allDF$FPTS),],POS=='WR'),rank = order(-FPTS))

save.image()