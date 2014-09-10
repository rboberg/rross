###########################
## TO RUN: Set working Directory to the Location of the Project
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
  start.team = 1,
  snake = T,
  fwd = T,
  adp.wt = 0.8,
  adp.source = 'ESPN.ADP'
  ){
  
  iterDF <- data.frame(c())
  bestDF <- data.frame(c())
  
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
      #bestDF <- rbind(bestDF,data.frame(iter=i,pick=j,aggregate(FPTS~POS,loopDF,max)))
    }
    #draftDF[order(draftDF$team,draftDF$POS),]

    
    iterDF <- rbind(iterDF,transform(draftDF,iter=i))
  }
  #return(list(iterDF,bestDF))
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

#######################
### This function takes teams, who is picking, and rounds
### returns the picks that that person gets
whichPicks <- function(who, nteam=12, rounds=12){
  picks <- c(who)
  gaps <- rep(c(who*2 - 1, (nteam - who)*2+1), rounds)
  for(i in 2:rounds){
    picks <- c(picks, picks[i-1] + gaps[i]) 
  }
  return(picks)
}

whichPicks(12)

######################
#####################

# Load CSV
allDF <- read.csv('fpro2014.csv', row.names = 1, stringsAsFactors = F)

################
## ADD ANY SPECIAL SCORING FPTS MEASURES YOU WANT TO USE
###############
allDF <- transform(allDF, FPTS.HPPR = FPTS + ifelse(is.na(REC.RECEIVING),0,REC.RECEIVING)*0.5)


###################
## SET UP SIMULATION
#################

# Number of Teams in Simulation
nteam <- 12

# Positions in simulations
pos.name <- c('QB','RB','WR','TE')

# Roster specifications
pos.start <- c(1,2.5,2.5,1)
pos.bench <- c(.5,2.5,2.5,.5)

#Calculate points over replacement
pos.total <- (pos.start+pos.bench)*nteam
point.names <- c('FPTS','FPTS.HPPR')
por.names <- c('POR','POR.HPPR')
pob.names <- c('POB','POB.HPPR')

for(i in 1:length(point.names)){
  point.var <- point.names[i]
  por.var <- por.names[i]
  pob.var <- pob.names[i]
  allDF[,por.var] <- 0
  allDF[,pob.var] <- 0
  
  for(j in 1:length(pos.name)){
    which.row <- allDF$POS == pos.name[j]
    pos.max <- min(pos.total[j] + 2, sum(which.row))
    repval <- -mean(sort(-allDF[which.row,point.var])[(pos.max-4):(pos.max)])
    allDF[which.row, por.var] <- allDF[which.row, point.var] - repval
    
    bench.max <- min(pos.start[j]*nteam + 2, sum(which.row))
    benchval <- -mean(sort(-allDF[which.row,point.var])[(bench.max-4):(bench.max)])
    allDF[which.row, pob.var] <- allDF[which.row, point.var] - benchval
  }
}

allDF$POR.RANK.HPPR <- rank(-allDF$POR.HPPR)
allDF$POB.RANK.HPPR <- rank(-allDF$POB.HPPR)
allDF$POR.RANK <- rank(-allDF$POR)
allDF$POB.RANK <- rank(-allDF$POB)

allDF[order((allDF$POR.RANK.HPPR + allDF$POB.RANK.HPPR)/2),c('ESPN.ADP','FPTS.HPPR','POR.HPPR','POB.HPPR', 'POR.RANK.HPPR', 'POB.RANK.HPPR')]
allDF[order(-(allDF$POR.HPPR)/2),c('ESPN.ADP','FPTS.HPPR','POR.HPPR','POB.HPPR')]

subset(allDF[order(-(allDF$POR.HPPR)/2),], POS=='WR')[,c('ESPN.ADP','FPTS.HPPR','POR.HPPR','POB.HPPR')]

standard.ranks <- allDF[order((allDF$POR.RANK+ allDF$POB.RANK)/2),c('ESPN.ADP','FPTS','POR','POB', 'POR.RANK', 'POB.RANK')]
transform(standard.ranks, rank.order = 1:nrow(standard.ranks))

hppr.ranks <- allDF[order((allDF$POR.RANK+ allDF$POB.RANK)/2),c('ESPN.ADP','FPTS.HPPR','POR.HPPR','POB.HPPR', 'POR.RANK.HPPR', 'POB.RANK.HPPR')]
transform(hppr.ranks, rank.order = 1:nrow(hppr.ranks))
#allDF[order(-(allDF$POR)/2),c('ESPN.ADP','FPTS','POR','POB')]

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

######################
## Look at best players available

#FPT MEASURE
fptsMeasure <- 'FPTS.HPPR'
#make sure fpts measure is in the data frame
iterDF[,fptsMeasure] <- allDF[match(iterDF$Player, allDF$Player),fptsMeasure]

iterDT <- data.table(transform(iterDF,iter=as.character(iter)),key=c('iter','pick','Player'))
tempDF <- allDF
tempDF$rank <- rank(-tempDF[,fptsMeasure])
playerDT <- data.table(tempDF,key=c('POS','rank'))

bestDT <- array(NA, c(4,max(iterDT$pick),max(iterDT$iter)),dimnames=list(c('QB','RB','WR','TE')))
bestDT['QB',1,] <- mean(playerDT['QB'][1:3][,get(fptsMeasure)])
bestDT['RB',1,] <- mean(playerDT['RB'][1:3][,get(fptsMeasure)])
bestDT['WR',1,] <- mean(playerDT['WR'][1:3][,get(fptsMeasure)])
bestDT['TE',1,] <- mean(playerDT['TE'][1:3][,get(fptsMeasure)])

for(i in 1:max(iterDT$iter)){
  system.time({
  loopDraft <- iterDT[as.character(i)]
  loopPlayer <- playerDT
  
  for(j in 2:nrow(loopDraft)){
    bestDT[,j,i] <- bestDT[,j-1,i]
    pj <- loopDraft[j]
    loopPlayer <- loopPlayer[Player != pj$Player]
    bestDT[pj$POS,j,i] <- mean(loopPlayer[pj$POS][1:3][,get(fptsMeasure)])
  }
  })
}

apply(bestDT[,12,] - bestDT[,36,], 1, quantile, probs=c(0.1,0.9), na.rm=T)

temp <- transform(melt(bestDT[,12,] - bestDT[,36,], varnames=c("POS","ITER")),Diff="12-36")
temp <- rbind(temp, transform(melt(bestDT[,36,] - bestDT[,60,], varnames=c("POS","ITER")), Diff="36-60"))
temp <- rbind(temp, transform(melt(bestDT[,60,] - bestDT[,84,], varnames=c("POS","ITER")), Diff="60-84"))
temp <- rbind(temp, transform(melt(bestDT[,84,] - bestDT[,106,], varnames=c("POS","ITER")), Diff="84-106"))
ggplot(temp,aes(x=POS,y=value)) + geom_boxplot() + facet_wrap(~Diff)

################
## Look at simulated rosters

#FPT MEASURE
fptsMeasure <- 'FPTS.HPPR'
#make sure fpts measure is in the data frame
iterDF[,fptsMeasure] <- allDF[match(iterDF$Player, allDF$Player),fptsMeasure]

rosters <- data.frame(c())
points <- data.frame(c())

for(i in 1:max(iterDF$iter)){
  for(j in 1:max(iterDF$team)){
    allpicks <- subset(iterDF, iter==i & team==j)
    allpicks.order <- allpicks[order(-allpicks[,fptsMeasure]),]
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
    points.ij[1,] <- allpicks[match(roster.ij,allpicks$Player),fptsMeasure]
    
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

#Get the most probable players at each position for each drafter
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


subbwDF <- subset(bwDF,pick %in% c(whichPicks(12,12,12), whichPicks(11,12,12)))
subbwDF$Group <- floor(subbwDF$pick/24)
ggplot(subbwDF,aes(x=pick,y=value,color=POS)) + geom_line()
acast(subbwDF,pick~POS)

acast(aggregate(value~Group+POS,subbwDF,mean),Group~POS)


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


################################
################################
# Check Player availability at each pick
niter <- max(iterDT$iter)
pickFreq <- table(iterDT$Player, iterDT$pick)
pickFreq <- cbind(0,pickFreq[,-dim(pickFreq)[2]])
dimnames(pickFreq)[[2]] <- 1:ncol(pickFreq)
pickProb <- 1-(t(apply(pickFreq,1,cumsum)) / niter)
probs <- c(0.1,1)

pointSuffix <- ''
pointSuffix <- '.HPPR'
pointMeasure <- paste0('FPTS',pointSuffix)

pick <- 24
picks <- iterDT[team==1 & iter==1][order(pick)]$pick
#picks <- 1:max(iterDF$pick)
picks <- whichPicks(1, 12, 12)

pickList <- list()
for(i in 1:length(picks)){
    pick <- picks[i]
    subDF <- subset(allDF, Player %in% row.names(pickProb))
    subDF$PROB <- pickProb[subDF$Player,pick]
    if(pick == 1){
      subDF$PROB2 <- pickProb[subDF$Player,pick+1]
      subDF <- subset(subDF, PROB2 > probs[1]  & PROB2 < probs[2])
    } else{
      subDF <- subset(subDF, PROB > probs[1]  & PROB < probs[2])
    }
    pickList[[paste0('Pick:',pick)]] <- by(subDF[,c(pointMeasure,paste0('POR',pointSuffix),paste0('POB',pointSuffix),'PROB','ESPN.ADP')],subDF$POS,function(x){x[order(-x$FPTS),]})
}

pickList  

save.image()