##########
## This script creates an XML file from pro-football-reference.com HTML of player statistics.
## The script scrapes and cleans 2013 player statistics and stores them in two CSVs
## player_stats_YYYY.csv
## player_info_YYYY.csv


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Football/2013/Reality"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))


############################
## LOAD DATA FROM HTML
############################

rootURL <- "http://www.pro-football-reference.com"
statyr <- 2013


#### PASSING ####
metaURL <- paste0(rootURL,"/years/",statyr,"/passing.htm")
metaParse <- htmlParse(metaURL)

#######################################
### Get the node I want via XPATH query
### using this to get the table of active teams id="passing"
tableList <- getNodeSet(metaParse,"//table[@id='passing']")
passingTable <- tableList[[1]]

#readHTMLTable(passingTable)
#temp <- passingTable[["tbody"]]["tr"][[1]]
#xmlGetAttr(temp["td"][[2]][["a"]],"href")

#Get Player Name and Link from Table
pass.name.info <- data.frame(
  t(sapply(passingTable[["tbody"]]["tr"], function(x){
                  if(length(x["td"]) > 1){
                    c(
                      xmlValue(x["td"][[2]]), #gets name
                      xmlGetAttr(x["td"][[2]][["a"]],"href")) #gets link
                  }else{c(NA,NA)}
                  }
                )),
  stringsAsFactors=F,row.names=NULL)
names(pass.name.info) <- c("dirty_name","link")

#Crete clean player name (takes away pro bowl indicators * and +)
pass.name.info <- transform(pass.name.info,name=gsub("[\\*]|[\\+]","",dirty_name))

### extract player abbreviation from link url
pass.name.info$id <- sapply(
  pass.name.info$link,
  function(x){
    reg.res <- regexec("/(\\w|\\.)+\\.htm",as.character(x))[[1]]
    return(substr(x,reg.res+1,reg.res+attr(reg.res,"match.length")-5))
  }
)

#extract the rest of the player data
pass.info <- merge(pass.name.info,data.frame(readHTMLTable(passingTable)),by.x="dirty_name",by.y="Var.2")
head(pass.info)

player.info <- pass.info[,c("id","name","link","dirty_name","Tm","Age","G","GS")]
pass.stats <- pass.info[,c("id",setdiff(names(pass.info),names(player.info)))]


#### RUSHING ####
metaURL <- paste0(rootURL,"/years/",statyr,"/rushing.htm")
metaParse <- htmlParse(metaURL)

#######################################
### Get the node I want via XPATH query
### using this to get the table of active teams id="passing"
tableList <- getNodeSet(metaParse,"//table[@id='rushing_and_receiving']")
rushingTable <- tableList[[1]]

#readHTMLTable(rushingTable)
#temp <- passingTable[["tbody"]]["tr"][[1]]
#xmlGetAttr(temp["td"][[2]][["a"]],"href")

#Get Player Name and Link from Table
rush.name.info <- data.frame(
  t(sapply(rushingTable[["tbody"]]["tr"], function(x){
    if(length(x["td"]) > 1){
      c(
        xmlValue(x["td"][[2]]), #gets name
        xmlGetAttr(x["td"][[2]][["a"]],"href")) #gets link
    }else{c(NA,NA)}
  }
  )),
  stringsAsFactors=F,row.names=NULL)
names(rush.name.info) <- c("dirty_name","link")

#Crete clean player name (takes away pro bowl indicators * and +)
rush.name.info <- transform(rush.name.info,name=gsub("[\\*]|[\\+]","",dirty_name))

### extract player abbreviation from link url
rush.name.info$id <- sapply(
  rush.name.info$link,
  function(x){
    reg.res <- regexec("/(\\w|\\.)+\\.htm",as.character(x))[[1]]
    return(substr(x,reg.res+1,reg.res+attr(reg.res,"match.length")-5))
  }
)

#extract the rest of the player data
rush.info <- merge(rush.name.info,data.frame(readHTMLTable(rushingTable)),by.x="dirty_name",by.y="Var.2")
head(rush.info)


player.info <- rbind(player.info, rush.info[,c("id","name","link","dirty_name","Tm","Age","G","GS")])
rush.stats <- rush.info[,c("id",setdiff(names(rush.info),names(player.info)))]


#### RECEIVING ####
metaURL <- paste0(rootURL,"/years/",statyr,"/receiving.htm")
metaParse <- htmlParse(metaURL)

#######################################
### Get the node I want via XPATH query
### using this to get the table of active teams id="passing"
tableList <- getNodeSet(metaParse,"//table[@id='receiving']")
receivingTable <- tableList[[1]]

#readHTMLTable(recevingTable)
#temp <- passingTable[["tbody"]]["tr"][[1]]
#xmlGetAttr(temp["td"][[2]][["a"]],"href")

#Get Player Name and Link from Table
rec.name.info <- data.frame(
  t(sapply(receivingTable[["tbody"]]["tr"], function(x){
    if(length(x["td"]) > 1){
      c(
        xmlValue(x["td"][[2]]), #gets name
        xmlGetAttr(x["td"][[2]][["a"]],"href")) #gets link
    }else{c(NA,NA)}
  }
  )),
  stringsAsFactors=F,row.names=NULL)
names(rec.name.info) <- c("dirty_name","link")

#Crete clean player name (takes away pro bowl indicators * and +)
rec.name.info <- transform(rec.name.info,name=gsub("[\\*]|[\\+]","",dirty_name))

### extract player abbreviation from link url
rec.name.info$id <- sapply(
  rec.name.info$link,
  function(x){
    reg.res <- regexec("/(\\w|\\.)+\\.htm",as.character(x))[[1]]
    return(substr(x,reg.res+1,reg.res+attr(reg.res,"match.length")-5))
  }
)

#extract the rest of the player data
rec.info <- merge(rec.name.info,data.frame(readHTMLTable(receivingTable)),by.x="dirty_name",by.y="Var.2")
head(rec.info)

player.info <- rbind(player.info, rec.info[,c("id","name","link","dirty_name","Tm","Age","G","GS")])
rec.stats <- rec.info[,c("id",setdiff(names(rec.info),names(player.info)))]


#remove duplicate players
player.info <- player.info[!duplicated(player.info$id),]

#
pass.name.chg <- union(intersect(names(pass.stats)[-1],names(rec.stats)[-1]),intersect(names(pass.stats)[-1],names(rush.stats)[-1]))
names(pass.name.chg) <- pass.name.chg
pass.name.chg[1:length(pass.name.chg)] <-paste0(names(pass.name.chg),".pass")

rec.name.chg <- union(intersect(names(pass.stats)[-1],names(rec.stats)[-1]),intersect(names(rec.stats)[-1],names(rush.stats)[-1]))
names(rec.name.chg) <- rec.name.chg
rec.name.chg[1:length(rec.name.chg)] <-paste0(names(rec.name.chg),".rec")

rush.name.chg <- union(intersect(names(pass.stats)[-1],names(rush.stats)[-1]),intersect(names(rec.stats)[-1],names(rush.stats)[-1]))
names(rush.name.chg) <- rush.name.chg
rush.name.chg[1:length(rush.name.chg)] <-paste0(names(rush.name.chg),".rush")

all.stats <- merge(merge(rename(pass.stats,pass.name.chg),rename(rush.stats,rush.name.chg),all=T,incomparables=0),rename(rec.stats,rec.name.chg),all=T,incomparables=0)


########################################
### save to csv
write.csv(all.stats,paste0("player_stats_",statyr,".csv"),row.names=F)
write.csv(player.info,paste0("player_info_",statyr,".csv"),row.names=F)
