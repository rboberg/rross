##########
## This script scrapes and cleans a footballperspective.com html table of historic estimates of DVOA
## It ends with a data.table dvoaDT, data.frame dvoaDF, and saves a csv estimated_dvoa.csv


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2014/Reality/EstimatedDVOA"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org", library(data.table)))



############################
## LOAD DATA FROM HTML
############################

metaURL <-"http://www.footballperspective.com/estimated-dvoa-ratings-from-1950-to-2013/"
metaParse <- htmlParse(metaURL)

#######################################
### Get the node I want via XPATH query
### using this to get the table of active teams id="passing"
tableList <- getNodeSet(metaParse,"//table[@id='tablepress-956']")
dvoahtml <- tableList[[1]]

dvoahtml[['tbody']]['tr'][[1]]['td']

rawtable <- t(sapply(dvoahtml[['tbody']]['tr'],
       function(x){
         c(sapply(x['td'],function(y){xmlValue(y)}),xmlGetAttr(x['td'][[1]][['a']],'href'))
       }))

colname <- c(
  'Team',
  'Year',
  'Lg',
  'Rec',
  'WinPct',
  'PDpG',
  'OffPass',
  'OffRun',
  'DefPass',
  'DefRun',
  'TotOff',
  'TotDef',
  'ST',
  'Total',
  'Link'
  )

coltype <- c(rep('string',4),rep('num',10),'string')

colnames(rawtable) <- colname

dvoaDF <- data.frame(rawtable,stringsAsFactors=F)

for(i in 1:ncol(dvoaDF)){
  if(coltype[i]=='num'){
    dvoaDF[,i] <- as.numeric(dvoaDF[,i]) 
  } else{
    dvoaDF[,i] <- as.character(dvoaDF[,i])
  }
}

#Replace Team Abbreviates with Pro-Football Reference Abrev
dvoaDF$Team <- toupper(gsub('h.*/','',gsub('/(?=[^/]*$).*','',dvoaDF$Link,perl=T),perl=T))

dvoaDT <- data.table(dvoaDF, key="Team,Year")



write.csv(dvoaDT,'estimated_dvoa.csv',row.names=F)

##########################
### GET TEAM / FRANCHISE INFO

teamDT <- data.table(Team=unique(dvoaDT$Team),key="Team")

teamDT[,list(MaxYear=max(dvoaDT[Team]$Year)),by=Team]
teamDT[,MaxYear:=max(dvoaDT[Team]$Year),by=Team]
teamDT[,MinYear:=min(dvoaDT[Team]$Year),by=Team]

#function that returns contiguous ranges as "S1-E1,S2-E2,..."
contRanges <- function(x){
  x <- as.numeric(x)
  xord <- sort(x) 
  xn <- length(xord)
  if(xn > 1){
  stout <- paste0(xord[1],'-')
  lookForEnd = T
  for(i in 2:xn){
    if(lookForEnd){
      if(xord[i]!= xord[i-1]+1){
        stout <- paste0(stout,xord[i-1])
        lookForEnd=F
      }
    } else{
      stout <- paste0(stout,',',xord[i-1],'-')
      lookForEnd=T
    }
  }
  stout <- paste0(stout,xord[xn])
  } else{
    stout = as.character(xord[1])
  }
  return(stout)
}

teamDT[,YearsActive:=contRanges(dvoaDT[Team]$Year),by=Team]

#Link to franchise encyclopedia 
teamDT[,Link:=gsub('/(?=[^/]*$).*','/',dvoaDT[Team][1]$Link,perl=T),by=Team]

#Get team XML // will take time to get HTML content from website
teamXML <- sapply(teamDT$Link,function(x){htmlParse(x)})

#Function to extract Team Name from XML
#xmli <- htmlParse("http://www.pro-football-reference.com/teams/sdg/")
getTeamName <- function(xmli){
  tmText <- xmlValue(getNodeSet(xmli,"//div[@id='you_are_here']/p/strong")[[2]])
  return(tmText)
}

#Add team name to DT
teamDT$TeamName <- sapply(teamXML,function(x){getTeamName(x)})

#Load team colors from CSV
csvin <- data.table(read.csv("team_colors.csv",stringsAsFactors=F))
csvin$Team <- toupper(csvin$Team)
sapply(csvin$ColorSolo,function(x){lighten(x)})

lighten <- function(hexin,pct=0.85){
  rgbin <- strtoi(c(substr(hexin,2,3),substr(hexin,4,5),substr(hexin,6,7)),16L)
  rgbout <- floor(rgbin + (255 - rgbin) * pct)
  hexout <- paste0('#',paste0(toupper(as.hexmode(rgbout)),collapse=''))
  return(hexout)
}
csvin$ColorLight <- sapply(csvin$ColorSolo,function(x){lighten(x)})

teamInfo <- merge(teamDT,csvin,all=T)

write.csv(teamInfo,'team_info.csv',row.names=F)