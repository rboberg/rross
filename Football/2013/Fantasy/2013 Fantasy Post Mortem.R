##########
## this script takes CSVs produced by other scripts to
## organize and analyze the performance of a Fantasy Football draft
## strategy used at the beginning of the 2013 season


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
#HOMEDIR <- "F:/Docs/Personal/rross"
setwd(paste0(HOMEDIR,"/Football/2013/Fantasy"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("quantreg", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(grid), error = function(e) install.packages("grid", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(gridExtra), error = function(e) install.packages("gridExtra", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org", library(data.table)))

############################

statyr <- 2013

### Load End of Season Stats from CSV
player.stats <- read.csv(paste0("../Reality/player_stats_",statyr,".csv"),stringsAsFactors=F)
player.info <- read.csv(paste0("../Reality/player_info_",statyr,".csv"),stringsAsFactors=F)

### Load Preseason Stats from CSV
preseason <- read.csv(paste0("preseason_rank_",statyr,".csv"),stringsAsFactors=F)

### Add id to preseason data
preseason$id <- player.info$id[match(preseason$name,player.info$name)]

# Check who's missing an ID
subset(preseason,is.na(id))

# Add id's for the missing ones
subset(player.info, grepl("Robert Griffin",name)| grepl("Ivory",name) | grepl("Hilton",name) | grepl("Manuel",name))
manual.id <- rbind(
  c("Robert Griffin III", "GrifRo01"),
  c("Christopher Ivory","IvorCh00"),
  c("Ty Hilton","HiltT.00"),
  c("E.J. Manuel","ManuEJ00")
  )

preseason[match(manual.id[,1],preseason$name),]
preseason[match(manual.id[,1],preseason$name),"id"] <- manual.id[,2] #preseason[match(manual.id[,1],preseason$name),]

### Set up roster info for fake draft
roster.info <- data.frame(
  pos=c("QB","RB","WR","TE"),
  start=c(1,2.5,2.5,1),
  bench=c(0.5,1.5,1.5,.25),
  stringsAsFactors=F)

mult <- 6

#initialize roster
roster <- data.frame(c(),stringsAsFactors=F)
for(i in 1:nrow(roster.info)){
  roster.add <- data.frame(
    pos=with(roster.info,rep(pos[i],round(start[i]*mult)+round(bench[i]*mult))),
    type=with(roster.info,c(rep("start",round(start[i]*mult)),rep("bench",round(bench[i]*mult)))),
    stringsAsFactors=F
  )
  roster <- rbind(roster,roster.add)
}
roster$id.value <- NA
roster$id.adp <- NA
roster$dp.value <- NA
roster$dp.adp <- NA

temp <- data.frame(c())


##### DRAFT
pickorder <- c('adp',rep(c('value','value','adp','adp'),times=nrow(roster)))

draft.pool <- preseason
for(i in 1:(nrow(roster)*2)){
  if(nrow(draft.pool)!=0){
    #ADP picks first
    if(pickorder[i]=='adp'){
    #if(i%%2==1){}  
      #fill starters first
      needs <- unique(subset(roster,is.na(id.adp) & type=="start")$pos)
      if(length(needs)==0) needs <- unique(subset(roster,is.na(id.adp))$pos)
      
      #pick min adp @ needed position
      idi <- subset(subset(draft.pool,pos %in% needs), (adp == min(adp)))$id[1]
      thisi <- match(idi,draft.pool$id)
      posi <- draft.pool$pos[thisi]
      
      #fill the roster
      roster$id.adp[with(roster, which(pos==posi & is.na(id.adp)))[1]] <- idi
      roster$dp.adp[with(roster, which(pos==posi & is.na(dp.adp)))[1]] <- i
      temp <- rbind(temp,data.frame(i=i,type="adp",pos=posi,id=idi,need=paste(needs,collapse=" ")))
      
      #remove from draft pool
      draft.pool <- draft.pool[-thisi,]
    } else{
      #fill starters first
      needs <- unique(subset(roster,is.na(id.value) & type=="start")$pos)
      if(length(needs)==0) needs <- unique(subset(roster,is.na(id.value))$pos)
      
      #pick max value @ needed position
      idi <- subset(subset(draft.pool,pos %in% needs), (value1 == max(value1)))$id[1]
      thisi <- match(idi,draft.pool$id)
      posi <- draft.pool$pos[thisi]
      
      #fill the roster
      roster$id.value[with(roster, which(pos==posi & is.na(id.value)))[1]] <- idi
      roster$dp.value[with(roster, which(pos==posi & is.na(dp.value)))[1]] <- i
      temp <- rbind(temp,data.frame(i=i,type="value",pos=posi,id=idi,need=paste(needs,collapse=" ")))
      
      #remove from draft pool
      draft.pool <- draft.pool[-thisi,]
    }
  }
}



#######
# Player fantasy points

#head(player.stats)
player.stats$Fmb <- with(player.stats,ifelse(is.na(Fmb.rush),ifelse(is.na(Fmb.rec),0,Fmb.rec),Fmb.rush))
replace.na.list <-c("Yds.pass","TD.pass","Int","Yds.rush","TD.rush","Yds.rec","TD.rec") 
player.stats[,replace.na.list][is.na(player.stats[,replace.na.list])] <- 0
player.stats <- transform(player.stats,
          pass.pts = Yds.pass / 25 + TD.pass * 6 - Int * 2,
          rush.pts = Yds.rush / 10 + TD.rush * 6 - Fmb * 2,
          rec.pts = Yds.rec / 10 + TD.rec * 6
)
player.stats$total.pts <- with(player.stats, pass.pts+rush.pts+rec.pts) 
#player.stats[sample(1:nrow(player.stats),size=10),]

#######
# Calculate Roster Points

roster$points.value <- player.stats$total.pts[match(roster$id.value, player.stats$id)]
roster$points.adp <- player.stats$total.pts[match(roster$id.adp, player.stats$id)]

apply(roster[,c("points.value","points.adp")],2,sum)

#calculate the scores of the top perfrormers of each team
top.n <- data.frame()
# for each position
for(posi in unique(roster$pos)){
  # roster of that position
  subroster <- subset(roster,pos==posi)
  #how many starters at that position
  #n.start <- sum(subroster$type %in% c("start"))
  n.start <- sum(subroster$type %in% c("start","bench"))
  #calculate performance of starters
  top.n <- rbind(top.n,with(subroster,
                data.frame(
                  points.value=sum(points.value[order(points.value,decreasing=T)][1:n.start]),
                  points.adp=sum(points.adp[order(points.adp,decreasing=T)][1:n.start]),
                  sd.both=sd(c(points.value,points.adp))*sqrt(n.start),
                  row.names=posi
                  )
            ))
}

#calculate season long points of each team type
top.n.sum <- apply(top.n,2,sum)
top.n.sum["sd.both"] <- top.n.sum["sd.both"] * sqrt(nrow(top.n))/nrow(top.n)

sum(top.n.sum["points.value"] - top.n.sum["points.adp"])
sqrt(sum(top.n.sum["sd.both"]^2))


#calculate points per game of each team type
top.n/6/16
ppg <- apply(top.n/6/16,2,sum)
ppg["sd.both"] <- ppg["sd.both"]/sqrt(4)

sum(ppg["points.value"] - ppg["points.adp"])
sqrt(sum(ppg["sd.both"]^2))


ggplot(subset(roster,pos=="QB")) + geom_point(aes(x=dp.value,y=points.value),col="red") + geom_smooth(aes(x=dp.value,y=points.value),col="red",se=F) + geom_point(aes(x=dp.adp,y=points.adp),col="blue") + geom_smooth(aes(x=dp.adp,y=points.adp),col="blue",se=F)
ggplot(subset(roster,pos=="RB")) + geom_point(aes(x=dp.value,y=points.value),col="red") + geom_smooth(aes(x=dp.value,y=points.value),col="red",se=F) + geom_point(aes(x=dp.adp,y=points.adp),col="blue") + geom_smooth(aes(x=dp.adp,y=points.adp),col="blue",se=F)
ggplot(subset(roster,pos=="WR")) + geom_point(aes(x=dp.value,y=points.value),col="red") + geom_smooth(aes(x=dp.value,y=points.value),col="red",se=F) + geom_point(aes(x=dp.adp,y=points.adp),col="blue") + geom_smooth(aes(x=dp.adp,y=points.adp),col="blue",se=F)
ggplot(subset(roster,pos=="TE")) + geom_point(aes(x=dp.value,y=points.value),col="red") + geom_smooth(aes(x=dp.value,y=points.value),col="red",se=F) + geom_point(aes(x=dp.adp,y=points.adp),col="blue") + geom_smooth(aes(x=dp.adp,y=points.adp),col="blue",se=F)

### 2 Things to Explain Outperformance
# 1 Where you drafted a postion
ggplot(roster)+geom_histogram(aes(x=dp.value-1),fill="blue",alpha=0.5,binwidth=12)+geom_histogram(aes(x=dp.adp-1),fill="red",alpha=0.5,binwidth=12)+facet_wrap(~pos)
### Could do an interactive plot, where you select a round and it show show much of each position
### each player type took in each round.

# 2 Drafting better players within a position
### Model Value~Draft Position, for each position
rcombo <- rbind(
  data.frame(
    rename(roster[,c("pos","id.value","dp.value","points.value")],c(id.value="id",dp.value="dp",points.value="points")),
    type="value"
  ),
  data.frame(
    rename(roster[,c("pos","id.adp","dp.adp","points.adp")],c(id.adp="id",dp.adp="dp",points.adp="points")),
    type="adp"
  )
)
rcombo <- data.table(rcombo)

rcombo[,dp.rank:=rank(dp),by=pos]

lm.pos <- c("QB","WR","RB","TE")

## can regress on each positions relative draft position, or actual draft position in draft
regon = "dp"

lm.list <- lapply(lm.pos,function(x){lm(paste0('points ~ ',regon),rcombo[pos==x])})
stats <- sapply(lm.list,function(x){c(coef(x),rsq=summary(x)$r.squared,n=length(summary(x)$residuals),meany=mean(x$model[,1]),meanx=mean(x$model[,2]))})
colnames(stats) <- lm.pos
stats

lm.list.noext <- lapply(lm.pos,function(x){lm(paste0('points ~ ',regon),rcombo[pos==x][points<max(points) & points>min(points)])})
stats.noext <- sapply(lm.list.noext,function(x){c(coef(x),rsq=summary(x)$r.squared,n=length(summary(x)$residuals),meany=mean(x$model[,1]),meanx=mean(x$model[,2]))})
colnames(stats.noext) <- lm.pos
stats.noext



x <- lm.list[[1]]
x$model[,1]

rcombo[,residual:=points-(stats[2,pos]*get(regon)+stats[1,pos])]
rcombo[,value.of.dp:=stats[2,pos]*(get(regon)-(stats[4,pos]+1)/2),by=list(type,pos)]
rcombo[,
       list(
        value.added=sum(residual),
        sd.value.added=sd(residual)*length(residual)^0.5,
        value.of.dp=sum(value.of.dp)
        ),
       by=type]

rcombo[,
       list(
         value.added=sum(residual),
         sd.value.added=sd(residual)*length(residual)^0.5,
         value.of.dp=sum(value.of.dp)
       ),
       by=list(type,pos)]

ggplot(rcombo[,points.mean:=median(points),by=pos],aes(x=dp.rank,y=points,col=type))+geom_point()+stat_smooth(se=F)+facet_wrap(~pos,ncol=2,scale="free_x") + geom_line(aes(y=points.mean),col="black")
ggplot(rcombo[,resid.mean:=median(residual),by=pos],aes(x=dp.rank,y=residual,col=type))+geom_point()+stat_smooth(se=F)+facet_wrap(~pos,ncol=2,scale="free_x") + geom_line(aes(y=resid.mean),col="black")

ggplot(rcombo[,points.mean:=median(points),by=list(pos,type)],aes(x=dp.rank,y=points,col=type))+geom_point()+stat_smooth(se=F)+facet_wrap(~pos,ncol=2,scale="free_x")


###For Use: Scatterplot Points~Draft Position, facet by position
ggplot(rcombo,aes(x=dp.rank,y=points,col=type))+geom_point()+stat_smooth(se=F)+facet_wrap(~pos,ncol=2,scale="free_x")


rcombo[,pos.rank:=rank(-.SD[,points]),by="pos"]
rcombo[,start:=.SD[,round(roster.info$start[roster.info$pos==pos]*mult)*2],by=pos]
rcombo[,bench:=.SD[,round(roster.info$bench[roster.info$pos==pos]*mult)*2],by=pos]
rcombo[,teamrank:=.SD[,rank(points,ties='random')],by=c("pos","type")]


###For Use: Box plot of points, facet by position
ggplot(
  rcombo[,
         list(
           MinPoints=min(points),
           Q1Points=quantile(points,0.25),
           MeanPoints=mean(points),
           MedianPoints=median(points),
           Q3Points=quantile(points,0.75),
           MaxPoints=max(points)
           
         ),
         by=list(type,pos)],
  aes(x=type,fill=type))+geom_boxplot(aes(ymin=MinPoints,ymax=MaxPoints,lower=Q1Points,middle=MedianPoints,upper=Q3Points),stat="identity") + geom_text(aes(y=MeanPoints,label=round(MeanPoints)),col="white",fontface="bold") + facet_wrap(~pos,scale="free_y")



ggplot(rcombo[,list(points=sum(points)),by=list(pos,type)],aes(x=factor(type),y=points,fill=pos)) + geom_bar(stat="identity") + scale_fill_hue(h=c(180,360),l=70,c=150)


### Top Pos
rcombo[pos=="RB",.SD[order(-points)]]
rcombo[pos=="RB",mean(dp),by=type]

rcombo[pos=="TE",.SD[order(-points)]]
rcombo[pos=="TE",mean(dp),by=type]

rcombo[pos=="WR",.SD[order(-points)]]
rcombo[pos=="WR",mean(dp),by=type]



### Sketch Table Showing Value Sources Break Down
stats


#Value Generated by Making better positions at particular point
residsmry1 <- rcombo[,.SD[,list(
  added=sum(residual),
  start=round(roster.info$start[roster.info$pos==pos]*mult),
  bench=round(roster.info$bench[roster.info$pos==pos]*mult)
  )],by=c("pos","type")]

residsmry <- merge(residsmry1[type=='value'],residsmry1[type=='adp'],by='pos',suffixes=c(".value",".adp"))[,list(
  Position=pos,
  StartingSpots=start.value,
  RosterSpots=start.value+bench.value,
  ValueSkill=added.value,
  ADPSkill = added.adp
  )]

residsmry[,StartingSkill:=(ValueSkill-ADPSkill)*StartingSpots/RosterSpots]

residsmry[,list(TotalSkill=sum(StartingSkill))]

#Total Point Analys
totsmry <- rcombo[,.SD[,list(
    points.value=mean(points[type=='value']),
    points.adp=mean(points[type=='adp']),
    stdev=sd(points),
    nstart=round(roster.info$start[roster.info$pos==pos]*mult),
    nbench=round(roster.info$bench[roster.info$pos==pos]*mult)
  )],by=c("pos")]
totsmry[,avg.point.diff:=points.value-points.adp]
totsmry[,start.point.diff:=avg.point.diff*nstart]
totsmry[,start.point.sd:=stdev*sqrt(nstart)*sqrt(2)]
totsmry

totsmry[,.SD[,list(points=sum(start.point.diff),stdev=sqrt(sum(start.point.sd^2)))]]/16



### Measure of best picks
## Compare player points to next 

ggplot(rcombo,aes(x=pos.rank,y=points,fill=type)) + geom_histogram(stat="identity",position="dodge") +facet_wrap(~pos,scale="free_x")

rcombo[,replace:=.SD[(floor(pos.rank)==start+1) | (ceiling(pos.rank)==start+1),list(points=mean(points))],by=pos]
rcombo

rcombo[(floor(pos.rank)==start+1) | (ceiling(pos.rank)==start+1)]

rcombo[,over.replace:=points-replace]

ggplot(rcombo,aes(x=pos.rank,y=over.replace,fill=type)) + geom_histogram(stat="identity",position="dodge") +facet_wrap(~pos,scale="free_x")
ggplot(rcombo,aes(x=dp,y=over.replace,fill=type)) + geom_histogram(stat="identity",position="dodge") +facet_wrap(~pos,scale="free_x")



###################
###################
### PRODUCTION ####
###################

#####################
## BROAD DISCUSSION

#How much value added?
#Points per team per game
#Method 1 - pro-rate total points 
totsmry1 <- rcombo[,.SD[,list(
  points.value=mean(points[type=='value']),
  points.adp=mean(points[type=='adp']),
  stdev=sd(points)/sqrt(length(points)),
  sd.value = sd(points[type=='value'])/sqrt(length(points[type=='value'])),
  sd.adp = sd(points[type=='adp'])/sqrt(length(points[type=='adp'])),
  nstart=mean(start)/2,
  nbench=mean(bench)/2
)],by=c("pos")]
totsmry1[,avg.point.diff:=points.value-points.adp]
totsmry1[,start.point.diff:=avg.point.diff*nstart]
totsmry1[,start.point.sd:=(sd.value+sd.adp)*sqrt(nstart)]
totsmry1
#mean and sd of method 1 estimate
tot1 <- totsmry1[,.SD[,list(points=sum(start.point.diff),stdev=sqrt(sum(start.point.sd^2)))]]/16/6
tot1
pnorm(tot1$points/tot1$stdev)

#Method 2 - just choose top n at each position and use as starters
#this will throw out the "worst" observations, like an seasonlong injury in week one
#also more forgiving of high risk/ high reward picks

totsmry2 <- rcombo[,.SD[
  ,
  list(
    points.value=mean(points[type=='value' & teamrank<=start/2]),
    points.adp=mean(points[type=='adp' & teamrank<=start/2]),
    stdev=sd(points[teamrank<=start/2])/sqrt(length(points[teamrank<=start/2])),
    sd.value = sd(points[type=='value' & teamrank<=start/2])/sqrt(length(points[type=='value' & teamrank<=start/2])),
    sd.adp = sd(points[type=='adp' & teamrank<=start/2])/sqrt(length(points[type=='adp' & teamrank<=start/2])),
    nstart=mean(start)/2,
    nbench=mean(bench)/2
  )],by=c("pos")]
totsmry2[,avg.point.diff:=points.value-points.adp]
totsmry2[,start.point.diff:=avg.point.diff*nstart]
totsmry2[,start.point.sd:=(sd.value+sd.adp)*sqrt(nstart)]
totsmry2

#mean and sd of method 2 estimate
tot2 <- totsmry2[,.SD[,list(points=sum(start.point.diff),stdev=sqrt(sum(start.point.sd^2)))]]/16/6
tot2
pnorm(tot2$points/tot2$stdev)

####
combosmry <- data.table(
  Position=totsmry1$pos,
  ValueAddedPerGame1=totsmry1$start.point.diff/16/6,
  ValueAddedPerGame2=totsmry2$start.point.diff/16/6
)
combototal <- rbind(combosmry,data.table(
  Position="Total",
  ValueAddedPerGame1=sum(combosmry[1:4]$ValueAddedPerGame1),
  ValueAddedPerGame2=sum(combosmry[1:4]$ValueAddedPerGame2)
))

#### Discussion
# I'm going to use method one going forward. I think method 2 is a better overall evaluation of success,
# but in evaluating the breakdown of where the value came from i like method 1 because it uses the whole data set
# so it provides more information about what each algorith was doing

dpSmry <- rcombo[,
       .SD[,
       list(
         StartingSpots=median(start)/12,
         ValueMeanDP=mean(dp[type=="value"]),
         ADPMeanDP=mean(dp[type=="adp"]),
         MarginalPointsPerSeason=-stats['dp',pos]
         )],
       by="pos"]
dpSmry[,ValueAddedPerSeason:=StartingSpots*(ValueMeanDP-ADPMeanDP)*MarginalPointsPerSeason*-1]
dpSmry[,ValueAddedPerGame:=ValueAddedPerSeason/16]

# Extreme values can have oversized effects on statistics, especially in small samples.
# I think that's the case for QBs and TEs
# QBs Manning and Josh Freeman
# TEs Jimmy Graham and Zach Sudfeild
# If you take out max and min for all positions, the value of an additional draft position is mroe inline


dpSmry.noext <- rcombo[,
                 .SD[,
                     list(
                       StartingSpots=median(start)/12,
                       ValueMeanDP=mean(dp[type=="value"]),
                       ADPMeanDP=mean(dp[type=="adp"]),
                       MarginalPointsPerSeason=-stats.noext['dp',pos]
                     )],
                 by="pos"]
dpSmry.noext[,ValueAddedPerSeason:=StartingSpots*(ValueMeanDP-ADPMeanDP)*MarginalPointsPerSeason*-1]
dpSmry.noext[,ValueAddedPerGame:=ValueAddedPerSeason/16]

## Try averaging betas
stats.use <- (stats + stats.noext)/2
#And change y intercept so the model still goes through the mean of the whole distribution
stats.use[1,] <- stats["meany",] - stats["meanx",] * stats.use[regon,]

dpSmry.use <- rcombo[,
                       .SD[,
                           list(
                             StartingSpots=median(start)/12,
                             ValueMeanDP=mean(dp[type=="value"]),
                             ADPMeanDP=mean(dp[type=="adp"]),
                             MarginalPointsPerSeason=-((stats.noext+stats)/2)['dp',pos]
                           )],
                       by="pos"]
dpSmry.use[,PositionPointsPerSeason:=StartingSpots*(ValueMeanDP-ADPMeanDP)*MarginalPointsPerSeason*-1]
dpSmry.use[,PositionPointsPerGame:=PositionPointsPerSeason/16]
setnames(dpSmry.use,"pos","Position")

dpSmry.use

#Total Benefit From Positional Strategy
#This year picking the top QBs very early quarterbacks might have landed you Drew Brees or Peyton Manning
#They had huge value this year.
#ADP placed higher value on early QBs than my Value based system
#Picking QBs later turned out to hurt my system around 0.5 points per game

write.csv(dpSmry.use,'dp_summary.csv',row.names=F)

#####
# Residual Skill

skillSmry <- rcombo[,
                    .SD[,
                        list(
                            StartingSpots=median(start)/12,
                            ValueMeanResidual=mean(
                                (points-(dp*stats.use[2,pos]+stats.use[1,pos]))[type=="value"]
                            ),
                            ADPMeanResidual=mean(
                              (points-(dp*stats.use[2,pos]+stats.use[1,pos]))[type=="adp"]
                            )
                          )
                        ],
                    by="pos"
                    ]
skillSmry[,SkillAddedPerSeason:=StartingSpots*(ValueMeanResidual-ADPMeanResidual)]
skillSmry[,SkillAddedPerGame:=SkillAddedPerSeason/16]
setnames(skillSmry,"pos","Position")

skillSmry

#Total Skill
write.csv(skillSmry,'skill_summary.csv',row.names=F)


######
## MAKE CSV FOR INTERACTIVE VISUALIZATION
# REQUIRED COLUMNS:
### Player Name or ID
### Position
### Drafted By
### Points Above Replacemtn
### Where Drafted
### Position Rank
### Overall Rank

draftDT <- data.table(
  Player = player.info$name[match(rcombo$id,player.info$id)],
  Position = rcombo$pos,
  DraftType = rcombo$type,
  PointsOverRep = rcombo$over.replace,
  DraftPosition = rcombo$dp,
  PositionRank = rcombo$pos.rank,
  OverallRank = rank(-rcombo$over.replace)
  )

write.csv(draftDT,"mock_draft.csv",row.names=F)

