##########
## This script loads NFL team history and meta information from pro-football-reference.com
## It saves them as CSV files:
## team_history.csv
## team_info.csv


###########################
##TO RUN: Set working Directory to the Location of the Script
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2013/Reality/Dynasty"))


###############
#### LOAD AND/OR INSTALL LIBRARIES
tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))

############################
## LOAD DATE FROM HTML
############################

rootURL <- "http://www.pro-football-reference.com"
metaURL <- "http://www.pro-football-reference.com/teams/"
metaParse <- htmlParse(metaURL)

#######################################
### Get the node I want via XPATH query
### using this to get the table of active teams id="teams_active"
tableList <- getNodeSet(metaParse,"//table[@id='teams_active']")
activeTable <- tableList[[1]]

#readHTMLTable(activeTable)
#xmlValue(activeTable[["tbody"]][["tr"]])
#tableCols = sapply(activeTable[["thead"]]["tr"][[2]]["th"], xmlValue)

### Get team names from table
teamNames = sapply(activeTable[["tbody"]]["tr"], function(x) xmlValue(x[["td"]][["a"]]))
### Get links for each team from table
teamLinks = sapply(activeTable[["tbody"]]["tr"], function(x) xmlGetAttr(x[["td"]][["a"]],"href"))
### Make a data.frame out of teams
team.info <- data.frame(name=teamNames,link=teamLinks,stringsAsFactors=F)
### extract team abbreviation from link url
team.info$abr <- sapply(
  team.info$link,
  function(x){
    reg.res <- regexec("/\\w+/$",as.character(x))[[1]]
    return(substr(x,reg.res+1,reg.res+attr(reg.res,"match.length")-2))
  }
)

########################################
### Extract team histories from links

### Empty team history data.frame
team.hist <- data.frame(
  team=c(),
  year=c(),
  league=c(),
  w=c(),
  l=c(),
  t=c(),
  playoffs=c(),
  pts.for=c(),
  pts.ag=c(),
  pt.dif=c(),
  coach=c(),
  o.pt.rnk=c(),
  o.yd.rnk=c(),
  d.pt.rnk=c(),
  d.yd.rnk=c(),
  pt.dif.rnk=c(),
  yd.dif.rnk=c(),
  n.teams=c(),
  stringsAsFactors=F
)


for(i in 1:nrow(team.info)){
  teamURL <- paste0(rootURL,team.info$link[i])
  teamParse <- htmlParse(teamURL)
  teamTableList <- getNodeSet(teamParse,"//table[@id='team_index']")
  teamTableXML <- teamTableList[[1]]
  xmlValue(teamTableXML)
  teamMatrix <- readHTMLTable(teamTableXML,stringsAsFactors=FALSE)
  
  to.bind <- data.frame(
    team=team.info$abr[i],
    year=as.numeric(teamMatrix$Year),
    league=teamMatrix$Lg,
    w=as.numeric(teamMatrix$W),
    l=as.numeric(teamMatrix$L),
    t=as.numeric(teamMatrix$T),
    playoffs=teamMatrix$Playoffs,
    pts.for=as.numeric(teamMatrix$Pts),
    pts.ag=as.numeric(teamMatrix$PtsO),
    pt.dif=as.numeric(teamMatrix$PtDif),
    coach=teamMatrix$Coaches,
    o.pt.rnk=as.numeric(teamMatrix[,16]),
    o.yd.rnk=as.numeric(teamMatrix[,17]),
    d.pt.rnk=as.numeric(teamMatrix[,18]),
    d.yd.rnk=as.numeric(teamMatrix[,19]),
    pt.dif.rnk=as.numeric(teamMatrix[,21]),
    yd.dif.rnk=as.numeric(teamMatrix[,22]),
    n.teams=as.numeric(teamMatrix[,23]),
    stringsAsFactors=F
  )
  
  team.hist <- rbind(team.hist,to.bind)
}
team.hist$w.pct <- with(team.hist,w/(w+l+t))
team.hist$pt.dif.gm <- with(team.hist,pt.dif/(w+l+t))
team.hist$pt.dif.qnt <- with(team.hist,1-(pt.dif.rnk-1)/(n.teams-1))
team.hist$pt.dif.zs <- team.hist$pt.dif/sapply(team.hist$year,function(x){sd(subset(team.hist,year==x)$pt.dif)})
write.csv(team.hist,"team_history.csv",row.names=F)
write.csv(team.info,"team_info.csv",row.names=F)
