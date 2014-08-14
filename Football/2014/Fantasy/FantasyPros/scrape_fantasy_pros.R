
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

########################################
# FUNCTIONS

getProjections <- function(pullPos){
  urlString <- paste0(
    'http://www.fantasypros.com/nfl/projections/',
    tolower(pullPos),
    '.php'
    )
  
  # parse the URL
  htmlObject <- htmlParse(urlString)
  
  # pull the data table
  tableList <- getNodeSet(htmlObject, "//table[@id='data']")
  thisTable <- tableList[[1]]
  
  ### GET COLUMN NAMES
  
  # get the column header GROUP information (e.g. Passing, Rushing...) 
  header1XML <- thisTable['thead'][[1]]['tr'][[1]]['td']
  
  varGroups <- sapply(
    header1XML,
    function(x){
      xmlValue(x)
      }
    )
  
  groupCount <- sapply(
    header1XML,
    function(x){
      ifelse(is.null(xmlAttrs(x)), 1, as.numeric(xmlAttrs(x)['colspan']))
      }
    )
  
  groups <- unlist(mapply(rep,x=varGroups,times=groupCount))
  
  # get the column header DETAIL information (e.g. ATT, YDS...)
  header2XML <- thisTable['thead'][[1]]['tr'][[2]]['th']
  varDetail <- sapply(header2XML, function(x){xmlValue(x)})
  
  # combine the GROUP information and DETAIL information into DETAIL.GROUP
  tableVars <- gsub('\\.*$','',make.names(paste(varDetail,groups)))
  
  ### GET TABLE DATA
  
  # get body
  bodyXML <- thisTable['tbody'][[1]]
  
  # loop through rows to get data
  rawDF <- suppressWarnings(
    data.frame(t(
      sapply(
        bodyXML['tr'],
        function(x){
          sapply(
            x['td'],
            function(y){
              xmlValue(y)
              }
            )
          }
        )
      ), stringsAsFactors=F)
    )
  
  # name columns
  names(rawDF) <- tableVars
  
  
  
  # clean data frame and make numeric columns
  cleanDF <- rawDF
  
  cleanDF$Player <- gsub('\\s*$','',rawDF$Player)
  
  for( vari in tableVars[ tableVars != 'Player' ] ){
    cleanDF[ , vari] <- as.numeric(gsub(',','',rawDF[ , vari]))
  }
  
  cleanDF$POS <- pullPos
    
  return(cleanDF)
}


getADP <- function(adpURL = "http://www.fantasypros.com/nfl/adp/overall.php"){
  
  # parse the URL
  htmlObject <- htmlParse(adpURL)
  
  # pull the data table
  tableList <- getNodeSet(htmlObject, "//table[@id='data']")
  thisTable <- tableList[[1]]
  
  # get the column header DETAIL information (e.g. ATT, YDS...)
  headerXML <- thisTable['thead'][[1]]['tr'][[1]]['th']
  varDetail <- sapply(headerXML, function(x){xmlValue(x)})
  tableVars <- gsub('\\s.*$','',varDetail)
  tableVars[!tableVars %in% c('Player','POS')] <- paste0(
    tableVars[!tableVars %in% c('Player','POS')],
    '.ADP'
    )
  
  ### GET TABLE DATA
  
  # get body
  bodyXML <- thisTable['tbody'][[1]]
  
  # loop through rows to get data
  rawDF <- suppressWarnings(
    data.frame(t(
      sapply(
        bodyXML['tr'],
        function(x){
          sapply(
            x['td'],
            function(y){
              ifelse(xmlValue(y) == " ", NA, xmlValue(y))
              }
            )
          }
        )
      ), stringsAsFactors=F)
    )
  
  # name columns
  names(rawDF) <- tableVars
  
  # clean data frame
  cleanDF <- rawDF
  cleanDF$POS <- gsub('\\d*','',rawDF$POS)
  cleanDF$Player <- gsub('\\s*$','',rawDF$Player)
  
  for( vari in tableVars[ !(tableVars %in% c('Player', 'POS')) ] ){
    suppressWarnings(cleanDF[ , vari] <- as.numeric(gsub(',','',rawDF[ , vari])))
  }
  
  return(cleanDF)
}


  
#######################################

# Get projections for each position
rbDF <- getProjections('RB')
qbDF <- getProjections('QB')
wrDF <- getProjections('WR')
teDF <- getProjections('TE')


# Merge predictions in to a single data.frame
allDF <- merge(rbDF,qbDF,all=T)
allDF <- merge(allDF,wrDF,all=T)
allDF <- merge(allDF,teDF,all=T)

# Get ADP data
adpDF <- getADP()

# Merge ADP data in to spreadsheet
allDF <- merge(allDF,adpDF,all=T)

# Rename rows
row.names(allDF) <- paste(allDF$Player,allDF$POS)

# Rename columns
allDF <- rename(
            allDF,
            c('FL.MISC' = 'FL',
              'FPTS.MISC' = 'FPTS'
              )
            )

write.csv(allDF,"fpro2014.csv")
