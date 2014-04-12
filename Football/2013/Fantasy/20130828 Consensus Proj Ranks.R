##########
## This script takes a CSV that takes consensus projections
## of 2013 fantasy football performance from FantasyPros.com
## It uses this information build draft ranks and compare them
## to ESPN's average draft position


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2013/Fantasy"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))

filename <- "consensus_forecasts"
pathname <- paste0(filename,".csv")

rawdata <- read.csv(pathname, stringsAsFactors = F)

players <- rawdata
rec.points <- 0
players$proj <- players$stan.proj + rec.points*players$rec

roster <- data.frame(
  pos=c("QB","RB","WR","TE"),
  start=c(1,2.5,2.5,1),
  bench=c(0.75,3,3,0.25))

teams <- 12

#calculate ADP rank (turns ADP in to 1 through N rank)
max.adp.rank <- 250
players$adp.rank <- rank(players$adp, ties.method="average")
players$adp.rank[players$adp.rank > max.adp.rank] <- max.adp.rank

#first benchmark
#simple average of the total number of each position expected to be taken

roster$simple.bm <- NA
for(i in 1:nrow(roster)){
  pos.n <- round((roster$start[i]+roster$bench[i])*teams)
  pos.players <- subset(players, pos==roster$pos[i])
  pos.players <- pos.players[order(pos.players$proj,decreasing=T),]
  roster$simple.bm[i] <- mean(pos.players$proj[1:pos.n])
}
roster

players$cost1 <- roster$simple.bm[match(players$pos,roster$pos)]
players$value1 <- with(players, proj-cost1)
players$rank1 <- rank(-players$value1)

players[order(players$rank1)[1:40],]



#second benchmark
#average of "second starters". 

roster$second.bm <- NA
for(i in 1:nrow(roster)){
  start.n <- round(roster$start[i]*teams)
  pos.players <- subset(players, pos==roster$pos[i])
  pos.players <- pos.players[order(pos.players$proj,decreasing=T),]
  roster$second.bm[i] <- mean(pos.players$proj[start.n+(1:start.n)])
}
roster

players$cost2 <- roster$second.bm[match(players$pos,roster$pos)]
players$value2 <- with(players, proj-cost2)
players$rank2 <- rank(-players$value2)

players[order(players$rank2)[1:40],]


#third rank
#rank by iterating through all players
#at each stage you consider the best RB, best WR, best TE, best QB and decide btw them
#the "value" of each is their projected minus the average of the remaining projected at their position
#the "value" of the last player is the replacement cost

continue = T
max.rank <- 200
players$rank3 <- max.rank
players$value3 <- 0
roster3 <- roster
roster3$current <- 1
roster3$bm <- ceiling((roster3$start + roster3$bench)*teams) + 1
roster3$best <- 0
roster3$best.index <- 0

count = 1
while(continue & count <= max.rank){
  for(i in 1:nrow(roster3)){
    roster.i <- roster3[i,]
    pos.sort <- subset(players,pos==roster.i$pos)
    pos.sort <- pos.sort[order(pos.sort$proj,decreasing=T),]
    pos.top <- pos.sort$proj[roster.i$current]
    pos.bm <- mean(pos.sort$proj[(roster.i$current+1):roster.i$bm])
    roster3$best[i] <- pos.top - pos.bm
    roster3$best.index[i] <- which(players$name==pos.sort$name[roster.i$current])
  }
  
  which.max <- which(roster3$best == max(roster3$best))
  roster3$current[which.max] <- roster3$current[which.max] + 1
  players$rank3[roster3$best.index[which.max]] <- count
  players$value3[roster3$best.index[which.max]] <- roster3$best[which.max]
  
  continue = !prod(roster3$current >= roster3$bm)
  count = count + 1
}

#put data in to javascript file


players$rank4 <- rank(with(players, (rank2+rank3)/2))
players[order(players$rank4)[1:50],]

####
#Add manual low ranks
low.names <- c("Le'Veon Bell", "Jonathan Stewart","Willis McGahee")
players$rank4[players$name %in% low.names] <- 161

#all data w/ pos info

data_js <- 'var all_data = { '

for(this.pos in c("QB","RB","TE","WR")){
  
  data_js <- paste0(data_js,'\n"', this.pos,'": { \n')
  
  adp.string <- '"ADP": {'
  rank.string <- '"RANK": {'
  data.subset <- subset(players[order(players$rank4)[1:150],],pos==this.pos)
  for(i in 1:nrow(data.subset)){
    player.i <- data.subset[i,]
    adp.string <- paste0(adp.string, "\n",'"',player.i$name,'": ',round(player.i$adp.rank),"," )
    rank.string <- paste0(rank.string, "\n",'"',player.i$name,'": ',round(player.i$rank4),"," )
  }
  
  
  data_js <- paste0(data_js,gsub(",$","",adp.string),"\n},\n",gsub(",$","",rank.string),"\n}\n},")
}

data_js <- gsub(",$","",data_js)

data_js <- paste0(data_js,"\n};")

cat(data_js,file="webfiles/ff_data.js")


players[order(players$rank4)[1:50],]

#####
## Plot of projections adjusted for average
plot.subset <- subset(players, adp < 150 & !(name %in% low.names))[,c("adp","value1","pos")]

plot.fit <- data.frame(adp=c(),value1=c(),pos=c())

for(pos.i in roster$pos){
  fit.subset <- subset(plot.subset, pos==pos.i)
  
  quad.add <- loess(value1~adp^4 + adp^3 + adp^2 + adp , fit.subset)
  #plot.fit <- rbind(plot.fit,data.frame(adp=quad.add$x,value1=quad.add$fitted,pos=pos.i))
  
  kern.add <- ksmooth(x=fit.subset$adp,y=fit.subset$value1,bandwidth=teams)
  plot.fit <- rbind(plot.fit,data.frame(adp=kern.add$x,value1=kern.add$y,pos=pos.i))
}


gscatter <- ggplot(plot.subset, aes(x=adp,y=value1,col=pos)) + geom_point(size=3,alpha=0.4)
gscatter <- gscatter + geom_smooth(aes(x=adp,y=value1,col=pos),data=plot.fit, se=F,size=1)
gscatter <- gscatter + xlab("Average Draft Position") + ylab("[Projected Points] - [Position Average]")
gscatter <- gscatter + scale_color_discrete(guide=guide_legend(title=""))

png(file="webfiles/all_positions.png",width=1600,height=1200,res=180)
gscatter
dev.off()

gscat2 <- gscatter + facet_wrap(~pos,nrow=2)

png(file="webfiles/facet_positions.png",bg="transparent",family="helvetica",width=1600,height=1200,res=180)
gscat2
dev.off()
  
 
#######
## Build html for rank & value table

#save as csv
write.csv(subset(players, !(name %in% low.names))[,c("name","adp","value1","pos")],"preseason_rank_2013.csv",row.names=F)


ptable.sub <- subset(players, adp < 150 & !(name %in% low.names))[,c("name","adp","value1","pos")]
ptable.sub <- ptable.sub[order(ptable.sub$value1,decreasing=T),]

ptable.str <- '<html>
<LINK href="player_table_style.css" rel="stylesheet" type="text/css"></LINK>
<table id="player_table">
<tr><td></td><td>Player</td><td>Value</td><td>ADP</td></tr>\n'

for(i in 1:nrow(ptable.sub)){
  ptable.str <- paste0(ptable.str,
                       '<tr><td>',
                       i,
                       '</td><td>',
                       ptable.sub$name[i],
                       '</td><td>',
                       round(ptable.sub$value1[i],1),
                       '</td><td>',
                       round(ptable.sub$adp[i],1),
                       '</td></tr>\n'
  )
}

ptable.str <- paste0(ptable.str,'</table>\n</html>')

cat(ptable.str,file="webfiles/player_values.html")

