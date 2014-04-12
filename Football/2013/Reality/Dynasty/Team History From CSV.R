##########
## This script loads NFL team history from CSV's created by another script
## team_history.csv
## team_info.csv
## It uses that information to analyze NFL franchise history & evaluate "Dynasties"
## The information and graphics produced were used for VividNumeral.com project


###########################
##TO RUN: Set working Directory to the Location of the Script
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2013/Reality/Dynasty"))

###############
#### LOAD AND/OR INSTALL LIBRARIES


tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("quantreg", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(grid), error = function(e) install.packages("grid", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(gridExtra), error = function(e) install.packages("gridExtra", repos = "http://cran.r-project.org", library(ggplot2)))

### Load from CSV
team.hist <- read.csv("team_history.csv",stringsAsFactors=F)
team.info <- read.csv("team_info.csv",stringsAsFactors=F)
team.colors <- read.csv("team_colors.csv",stringsAsFactors=F)

#############################
### Check squared point differential over time
pt.dif.hist <- aggregate(pt.dif.gm^2 ~ year, team.hist, FUN="mean")

ggplot(subset(pt.dif.hist,year>=1970 & year < 2013),aes(x=year,y=`pt.dif.gm^2`)) + geom_line()

pt.dif.time <- lm(sqrt(`pt.dif.gm^2`)~year,pt.dif.hist,subset=(year>=1990 & year<2013))
summary(pt.dif.time)
#The average point differential has been going up since 1980, but only about 0.02 points per game

#############################
### Run lagged regressions

#function for running lagged regressions between different parameters
lag.reg <- function(team.hist,
                    metric.ind="pt.dif.gm",
                    metric.dep="pt.dif.gm",
                    lag.n=1,
                    year.min=1970,
                    year.max=100000,
                    zero.constant=T,
                    filter.on="",
                    filter.lim=c(0,1),
                    return.lm=T
                    ) {
  
  df.ind <- subset(team.hist,year>=year.min & year <= min(c(max(year), year.max))-lag.n)
  if(filter.on %in% names(df.ind)){
    df.ind <- df.ind[df.ind[,filter.on] >= filter.lim[1] & df.ind[,filter.on] <= filter.lim[2],]
  }
  
  df.dep <- team.hist[match(with(df.ind,paste0(year+lag.n,team)), with(team.hist,paste0(year,team))),]

  
  data.subset <- subset(
    data.frame(ind=df.ind[,metric.ind], dep=df.dep[,metric.dep]),
    (!is.na(ind))&(!is.na(dep))
  )
  
  if(return.lm){
    
    if(metric.ind=="w.pct"){data.subset$ind <- data.subset$ind-0.5}
    if(metric.dep=="w.pct"){data.subset$dep <- data.subset$dep-0.5}
    if(metric.ind=="pt.dif.qnt"){data.subset$ind <- data.subset$ind-0.5}
    if(metric.dep=="pt.dif.qnt"){data.subset$dep <- data.subset$dep-0.5}
    
    if(zero.constant){
      lm.n <- lm(dep~0+ind,data.subset)
    } else{
      lm.n <- lm(dep~ind,data.subset)
    }
    
  return(lm.n)
  } else{
    return(data.frame(team=df.ind$team,
                      year=as.numeric(df.ind$year),
                      ind=as.numeric(df.ind[,metric.ind]),
                      dep=as.numeric(df.dep[,metric.dep]),
                      stringsAsFactors=F))
  }
}

# See how point dif predicts wins
pt.predict <- lag.reg(team.hist,"pt.dif.gm","w.pct",1,1970,2013-1)

# See how wins predicts wins
w.predict <- lag.reg(team.hist,"w.pct","w.pct",1,1970,2013-1)

# See how pt.dif quantile predicts wins
qnt.predict <- lag.reg(team.hist,"pt.dif.qnt","w.pct",1,1970,2013-1)

# See how pt.dif z score predicts wins
zs.predict <- lag.reg(team.hist,"pt.dif.zs","w.pct",1,1970,2013-1)

# Compare point diff based prediction with win pct based prediction
summary(pt.predict)
summary(w.predict)
summary(qnt.predict)
summary(zs.predict)



#Conclusion:
#point dif per game has the highest r-squared (18.85%) with next season win percentage
#1 point per game is worth an expected 1.3% wins above 50% the following season

##########
## Calculate general relationship between ppg and wins in same season
wins.per.point <- lag.reg(team.hist,"pt.dif.gm","w.pct",0,1970,2013-1)

#Conclusion:
#1 point per game of point differential is worth 2.75% wins better than 50%
#This relationship has a very high r-squard of 83%
#20 points per game should be a (MORE THAN!) undefeated team

###########################
############################
#CALCULATE SEASON ON SEASON POINT DIFF PER GAME RELATIONSHIP
pt.dif.rev <- lag.reg(team.hist,"pt.dif.gm","pt.dif.gm",1,1970,2013-1)
summary(pt.dif.rev)

### The point per game mean reversion constant since 1970 is .47
### The r-squared between seasons is 22.4%

## Check if mean reversion is consistent across quality  of teams
mr80 <- lag.reg(
  team.hist,
  "pt.dif.gm",
  "pt.dif.gm",
  1,1970,2013-1,
  zero.constant=T,
  filter.on="pt.dif.qnt",
  filter.lim=c(0.8,1)
  )

mr60 <- lag.reg(
  team.hist,
  "pt.dif.gm",
  "pt.dif.gm",
  1,1970,2013-1,
  zero.constant=T,
  filter.on="pt.dif.qnt",
  filter.lim=c(0.6,0.8)
  )

mr40 <- lag.reg(
  team.hist,
  "pt.dif.gm",
  "pt.dif.gm",
  1,1970,2013-1,
  zero.constant=T,
  filter.on="pt.dif.qnt",
  filter.lim=c(0.4,0.6)
)

mr20 <- lag.reg(
  team.hist,
  "pt.dif.gm",
  "pt.dif.gm",
  1,1970,2013-1,
  zero.constant=T,
  filter.on="pt.dif.qnt",
  filter.lim=c(0.2,0.4)
)

mr0 <- lag.reg(
  team.hist,
  "pt.dif.gm",
  "pt.dif.gm",
  1,1970,2013-1,
  zero.constant=T,
  filter.on="pt.dif.qnt",
  filter.lim=c(0,0.2)
)

summary(mr80)
summary(mr60)
summary(mr40)
summary(mr20)
summary(mr0)

#CONCLUSION:
#There are slight differences in the mean reversion constant
#when broken down by bi-decile
#Other than the 40-60 bi-decile, which has already mean reverted
#and so has a statistically insignificant constant, the mean reversion
#constants are all between 44 and 58.
#The worst 20 bi-decile mean reverts the least- this suggests that
#awfulness is more consistent than greatness.
#The second worst mean reverts the most
#All of the mean reversion constants are within
#1 standard deviation of 0.47 (other than the mid bi-decile)

#Also residual standard errors are fairly consistent around 5.5
#This gives the following distibution of expected points per game
#the following season:
# P(T+1) ~ Normal(mean = P(T) * 0.47, sd = 5.5)

mr.m <- 0.47
mr.sd <- 5.5

#We can check how well a team outperformed its mean reversion expectation
#One metric is the probability the team did worse, given how well
#the team performed the previous according to the distribution above


#one approach is to get the probability of each year
#and take a moving average...

perf.df <- lag.reg(team.hist,year.max=2013-1,return.lm=F)
perf.df$zs <- (perf.df$dep - perf.df$ind*mr.m)/mr.sd
perf.df$prob <- pnorm(perf.df$zs)

df.ma.func <- function(x,time,by,n=5){
  this.ma <- function(x.inner,n){
    c(rep(NA,n-1),sapply(n:length(x.inner),function(y) mean(x.inner[(y-n+1):y])))
  }
  the.df <- data.frame(value=x,time=time,by=by)
  the.ar <- acast(the.df,by~time)
  ma.ar <- t(apply(the.ar,1,this.ma,n=n))
  dimnames(ma.ar) <- dimnames(the.ar)
  return(sapply(1:nrow(the.df),function(i) ma.ar[as.character(the.df$by[i]),as.character(the.df$time[i])]))
}

eval.n <- 10

perf.df$prob.ma <- df.ma.func(perf.df$prob,perf.df$year,perf.df$team,n=eval.n)

ggplot(perf.df,aes(x=year,y=prob.ma)) + geom_line()+facet_wrap(~team,ncol=4)

#...BUT now that I think about it
#a better way might be to assume each year is
#an independent normal distributions
#and see what the probablity that you average
#that well over the total span is
#The sum of n independent variables ~N(0,1)
#should have mean zero and variance n*1
#therefore sd = sqrt(n)
#The mean of those n independent variables
#have variance n*1/(n^2) = 1/n
#therefore sd = sqrt(1/n)

#So calculate the moving average of the z score
perf.df$zs.ma <- df.ma.func(perf.df$zs,perf.df$year,perf.df$team,n=eval.n)

#And the joint probablity of a worse outcome is 
perf.df$joint.prob <- pnorm(perf.df$zs.ma,mean=0,sd=(1/eval.n)^0.5)

#And plot the joint probability
ggplot(perf.df,aes(x=year,y=joint.prob)) + geom_line()+facet_wrap(~team,ncol=4)

#Check the worst francises ever:
perf.df[order(perf.df$joint.prob)[1:20],]

#And the best
perf.df[order(perf.df$joint.prob,decreasing=T)[1:20],]

#Write best and worst to a table
html.table <- function(df,header=names(df),digits=4){
  retstr <- ""
  if(header[1]!=""){
    retstr <- paste0(retstr,"<tr>\n",paste0(sapply(header,function(x) paste0(
      "<th>",
      ifelse(is.numeric(x),format(x,digits=digits,nsmall=digits),x),
      "</th>\n"
      )),collapse=""),"</tr>\n")
  }
  
  for(i in 1:nrow(df)){
    retstr <- paste0(retstr,"<tr>\n",paste0(sapply(df[i,],function(x) paste0(
      "<td>",
      ifelse(is.numeric(x),format(x,digits=digits,nsmall=digits),x),
      "</td>\n"
      )),collapse=""),"</tr>\n")
  }
  return(retstr)
}

bottom20 <- perf.df[order(perf.df$joint.prob)[1:20],]
bottom20 <- transform(bottom20,year=paste0(year-8,"-",year+1), team=toupper(team), joint.prob=round(joint.prob,4))
bottom20.html <- html.table(bottom20[,c("team","year","joint.prob")],header=c("Team","Period","Score"))

cat(bottom20.html,file="dynasty_files/bottom20table.html")

top20 <- perf.df[order(perf.df$joint.prob,decreasing=T)[1:20],]
top20 <- transform(top20,year=paste0(year-8,"-",year+1), team=toupper(team), joint.prob=round(joint.prob,4))
top20.html <- html.table(top20[,c("team","year","joint.prob")],header=c("Team","Period","Score"))

cat(top20.html,file="dynasty_files/top20table.html")


  #Find the best x franchises every year:
rank.over.time <- function(data,value="joint.prob",by="team",time="year"){
  data.ar <- acast(data,as.formula(paste0(by,"~",time)),value.var=value)
  rank.df <- melt(apply(data.ar,2,function(x) row.names(data.ar)[order(x,decreasing=T)]),varnames=c("rank",time),value.name=by)
  rank.df[,value] <- NA
  for(i in 1:nrow(rank.df)){
    data.i <- which(rank.df[i,by]==data[,by] & rank.df[i,time]==data[,time])
    if(length(data.i)!=0){
      rank.df[i,value] <- data[data.i,value]
    }
  }
  rank.df <- rank.df[!is.na(rank.df[,value]),]
  return(rank.df)
}

rank.df <- rank.over.time(perf.df)
acast(subset(rank.df,rank<=5),year~rank,value.var="team")

the.best <-with(perf.df[order(perf.df$joint.prob,decreasing=T)[1:20],],
                data.frame(
                  Team=toupper(team),
                  Period=paste0(year-8,"-",year+1),
                  Score=joint.prob,
                  stringsAsFactors=F)
                )


########################
####### Pretty Plots
########################

### Plots of each team over time
dyplot.df <- subset(perf.df, !is.na(perf.df$joint.prob))

dyplot.df$team <- toupper(dyplot.df$team)
dyplot2 <- ggplot(dyplot.df,aes(x=year,y=joint.prob))
dycolors <- c(rgb(105,210,231,max=255),rgb(250,105,0,max=255))
back.color <- rgb(0.95,0.95,0.95)
dyplot2 <- dyplot2 + geom_ribbon(aes(ymin=0.5,ymax=sapply(joint.prob,function(x) max(c(x,0.5)))),fill=dycolors[1])
dyplot2 <- dyplot2 + geom_ribbon(aes(ymax=0.5,ymin=sapply(joint.prob,function(x) min(c(x,0.5)))),fill=dycolors[2])
dyplot2 <- dyplot2 + theme_minimal()
dyplot2 <- dyplot2 + theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank())
dyplot2 <- dyplot2 + facet_grid(team~.)
dyplot2 <- dyplot2 + theme(strip.text.y=element_text(angle=0),axis.title=element_blank())
dyplot2 <- dyplot2 + theme(panel.background=element_rect(fill=back.color,color="white"), panel.margin=unit(0,"cm"))
dyplot2 

png(file="dynasty_files/single_plots.png",width=400,height=2000,res=180,pointsize=1)
dyplot2
dev.off()

dyplot_func <- function(dyplot.df){
  dyplot.df$team <- toupper(dyplot.df$team)
  dyplot2 <- ggplot(dyplot.df,aes(x=year,y=joint.prob))
  dycolors <- c(rgb(105,210,231,max=255),rgb(250,105,0,max=255))
  back.color <- rgb(0.95,0.95,0.95)
  dyplot2 <- dyplot2 + geom_ribbon(aes(ymin=0.5,ymax=sapply(joint.prob,function(x) max(c(x,0.5)))),fill=dycolors[1])
  dyplot2 <- dyplot2 + geom_ribbon(aes(ymax=0.5,ymin=sapply(joint.prob,function(x) min(c(x,0.5)))),fill=dycolors[2])
  dyplot2 <- dyplot2 + theme_minimal()
  dyplot2 <- dyplot2 + theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank())
  dyplot2 <- dyplot2 + facet_grid(team~.)
  dyplot2 <- dyplot2 + theme(strip.text.y=element_text(angle=0),axis.title=element_blank())
  dyplot2 <- dyplot2 + theme(panel.background=element_rect(fill=back.color,color="white"), panel.margin=unit(0,"cm"))
  dyplot2 <- dyplot2 + theme(plot.margin=unit(c(0,0,0,0),"cm"))
  return(dyplot2)
}

dyplot.df <- subset(perf.df, !is.na(perf.df$joint.prob))
dylevels <- levels(factor(dyplot.df$team))
g.every.n <- 6
dylist <- lapply(1:(length(dylevels)/g.every.n),function(i){
  dyplot_func(subset(dyplot.df,team %in% dylevels[(1+(i-1)*g.every.n):(i*g.every.n)])) + theme(axis.ticks.x=element_blank(), axis.text.x=element_text(color="grey40"), plot.margin=unit(c(0,0,0.2,0),"cm"))
})

dylist$ncol=1

png(file="dynasty_files/single_plots.png",width=600,height=3000,res=180,pointsize=1)
do.call(grid.arrange,dylist)
dev.off()




### Bump Plots for Each Decade
gbump.func <- function(bump.df.outer,team.colors, use.team.colors=T,text.size=NA,chart.note=""){
  bump.df.inner <- bump.df.outer
  bump.df.inner$year <- bump.df.inner$year+1
  rank.levels <- as.character(1:max(bump.df.inner$rank))
  bump.df.inner<- rbind(bump.df.inner,data.frame(rank=rank.levels,year=max(bump.df.inner$year),team=NA,joint.prob=NA))
  bump.df.inner$labels <- toupper(bump.df.inner$team)
  bump.df.inner$lwidth <- 0.5
  
  #browser()
  
  bump.df.inner <- rbind(transform(bump.df.inner,year.shift=year-lwidth/2),transform(bump.df.inner,year.shift=year+lwidth/2))
  
  bump.df.inner <- transform(bump.df.inner, rank.factor = factor(as.character(rank),levels=rev(rank.levels),ordered=T))
  
  gbump <- ggplot(bump.df.inner, aes(
    year,
    rank.factor,
    group = team,
    colour = team,
    label = labels
  ))
  gbump <- gbump + geom_line(aes(x=year.shift))
  #gbump <- gbump + geom_text(
  #  data = subset(bump.df, year == max(year)),
    #aes(x = factor(year + 0.25)), size = 3.5, hjust = 0.8
  #  aes(x = year + 0.25), size = 3.5, hjust = 0
  #) + geom_text(
  #  data = subset(bump.df, year == min(year)),
    #aes(x = factor(year - 0.5)), size = 3.5, hjust = 0.8
  #  aes(x = year - 0.25), size = 3.5, hjust = 1
  #)
  gbump <- gbump + theme_minimal()
  #gbump <- gbump + scale_x_discrete( breaks = c("",levels(factor(bump.df$year)),""),labels=c("",levels(factor(bump.df$year)),""))
  gbump <- gbump + scale_x_continuous(
    breaks=as.numeric(levels(factor(bump.df.inner$year))),
    labels=sapply(as.numeric(levels(factor(bump.df.inner$year))),function(yr) paste0(yr-9,"-",yr)),
    limits=c(min(bump.df.inner$year),max(bump.df.inner$year)))
  gbump <- gbump + theme(legend.position="none",axis.ticks=element_blank(),
                         panel.grid=element_blank())
  gbump <- gbump + geom_rect(aes(xmin = year - lwidth/2, xmax = year + lwidth/2), ymin = 0.5,ymax = length(rank.levels)+0.5,fill="white",color="white")
  gbump <- gbump + labs(x="",y="Rank",title="")
  if(is.na(text.size)){gbump <- gbump +  geom_text() } else{gbump <- gbump +  geom_text(size=text.size)}
  if(use.team.colors){
    gbump <- gbump + scale_color_manual(values=team.colors$color.manual[match(levels(factor(bump.df.inner$team)),team.colors$team)])
  } else{
    gbump <- gbump + scale_color_hue(c=100)
  }
  gbump <- gbump  + theme(plot.margin=unit(c(0,0,0,0),"cm"))
  
  if(chart.note!=""){
    gbump <- gbump + labs(x=chart.note) + theme(axis.title.x=element_text(color="grey20",size=rel(0.75),face="italic"))
  }
  
  return(gbump)
}


topn <- 5
top5.label <- "Only Teams That Ranked in Top 5"
#### 2000+
decade <- 2000
bump.df <- subset(rank.df, floor(year/10)*10>=decade)
bump.df <- subset(bump.df, team %in% levels(factor(subset(bump.df, rank <= topn)$team)))
#gbump.func(bump.df, team.colors, text.size=3)

png(file="dynasty_files/rank_2000.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=top5.label)
dev.off()

##### 90s
decade <- 1990
bump.df <- subset(rank.df, floor(year/10)*10==decade)
bump.df <- subset(bump.df, team %in% levels(factor(subset(bump.df, rank <= topn)$team)))

png(file="dynasty_files/rank_1990.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=top5.label)
dev.off()


##### 80s
decade <- 1980
bump.df <- subset(rank.df, floor(year/10)*10<=decade)
bump.df <- subset(bump.df, team %in% levels(factor(subset(bump.df, rank <= topn)$team)))

png(file="dynasty_files/rank_1980.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=top5.label)
dev.off()

### Too many lines? only highlight the ones i'm talking about? only top-x of the decade?

######## CHARTS WITH ALL
all.label <- "All Active Teams"
#### 2000+
decade <- 2000
bump.df <- subset(rank.df, floor(year/10)*10>=decade)
bump.df <- subset(bump.df, !is.na(rank))

png(file="dynasty_files/rank_all_2000.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=all.label)
dev.off()

##### 90s
decade <- 1990
bump.df <- subset(rank.df, floor(year/10)*10==decade)
bump.df <- subset(bump.df, team %in% levels(factor(subset(bump.df)$team)))

png(file="dynasty_files/rank_all_1990.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=all.label)
dev.off()


##### 80s
decade <- 1980
bump.df <- subset(rank.df, floor(year/10)*10<=decade)
bump.df <- subset(bump.df, !is.na(rank))

png(file="dynasty_files/rank_all_1980.png",width=1600,height=800,res=180)
gbump.func(bump.df, team.colors, text.size=3,chart.note=all.label)
dev.off()


###########################################
######## Trying to make probability cones
###########################################

cone.subset <- subset(team.hist,year>=1970 & year < 2013)
cone.ar <- acast(cone.subset,team~year,value.var="pt.dif.gm")

quant.bucket <- function(x, n=5){
  qs <- seq(from=0,to=1,by=1/n)
  q.value <- (rank(x))/(length(x))
  qs[-1][sapply(q.value,function(xi) sum(xi>qs))]
}

n.bucket <- 5
cone.ar.qnt <- apply(cone.ar,2,quant.bucket,n=n.bucket)
dimnames(cone.ar.qnt) <- dimnames(cone.ar)

to.lag.df <- function(x,lag.n=1){
  n.years <- ncol(x)
  x.fwd <- x[,(1+lag.n):n.years]
  x.past <- x[,1:(n.years-lag.n)]
  out.df <- data.frame(
    melt(x.fwd),
    melt(x.past)$value,
    n=lag.n
  )
  names(out.df) <- c("team","year","fwd","past","n")
  out.df <- subset(out.df,!is.na(fwd) & !is.na(past))
  return(out.df)
}

cone.df <- data.frame(c())
for(lag.n in c(0,1,2,3,4,5,10)){
  cone.df <- rbind(cone.df,to.lag.df(cone.ar,lag.n))
}

cone.df$bucket <- sapply(1:nrow(cone.df),function(rowi) cone.ar.qnt[as.character(cone.df$team[rowi]),as.character(cone.df$year[rowi]-cone.df$n[rowi])])

gcone <- ggplot(cone.df,aes(x=n,y=fwd,col=factor(bucket)))
gcone <- gcone + geom_smooth(method="loess",se=F)
gcone <- gcone + geom_hline(yint=0)
gcone


