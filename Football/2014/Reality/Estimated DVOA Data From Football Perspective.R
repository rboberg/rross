##########
## This script scrapes and cleans a footballperspective.com html table of historic estimates of DVOA
## It ends with a data.table dvoaDT, data.frame dvoaDF, and saves a csv estimated_dvoa.csv


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2014/Reality"))


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

dvoaDT <- data.table(dvoaDF)

write.csv(dvoaDT,'estimated_dvoa.csv',row.names=F)
