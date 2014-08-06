
###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/SSA/BabyNames/"))


###############
#### LOAD AND/OR INSTALL LIBRARIES

tryCatch(library(XML), error = function(e) install.packages("XML", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(ggplot2), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(reshape2), error = function(e) install.packages("reshape2", repos = "http://cran.r-project.org", library(reshape2)))
tryCatch(library(quantreg), error = function(e) install.packages("ggplot2", repos = "http://cran.r-project.org", library(ggplot2)))
tryCatch(library(plyr), error = function(e) install.packages("plyr", repos = "http://cran.r-project.org", library(XML)))
tryCatch(library(data.table), error = function(e) install.packages("data.table", repos = "http://cran.r-project.org", library(data.table)))
require('maps')

########################################
# FUNCTIONS

getNames <- function(thisTable){
  retdf <- t(
              sapply(
                thisTable['tr'][-1],
                function(x){
                  c(State = xmlValue(x['td'][[1]]), TopName = xmlValue(x['td'][[2]]))
                }
                )
              )
  return(retdf)
}

#########################################

urlString <- 'http://www.ssa.gov/oact/babynames/state/top5_1986.html'

#parse the URL
htmlObject <- htmlParse(urlString)
tableList <- getNodeSet(htmlObject,"//div[@id='content']//table")


girlNames <- data.frame(getNames(tableList[[1]]))
boyNames <- data.frame(getNames(tableList[[2]]))

states_map <- map_data('state')

boyNames$State <- tolower(boyNames$State)
boyNames$State[which(is.na(match(boyNames$State, states$region)))]

girlNames$State <- tolower(girlNames$State)
girlNames$State[which(is.na(match(girlNames$State, states$region)))]

states_map$BoyName <- boyNames$TopName[match(states$region, boyNames$State)]
states_map$GirlName <- girlNames$TopName[match(states$region, girlNames$State)]

cnamesBoy <- aggregate(cbind(long, lat) ~ region, data=states_map, 
                    FUN=function(x)mean(range(x)))

cnamesBoy$label <- boyNames$TopName[match(cnamesBoy$region, boyNames$State)]

cnamesGirl <- aggregate(cbind(long, lat) ~ region, data=states_map, 
                    FUN=function(x)mean(range(x)))

cnamesGirl$label <- girlNames$TopName[match(cnamesGirl$region, girlNames$State)]



ggplot(boyNames,aes(map_id = State)) +
  geom_map(aes(fill = TopName), map = states_map, color='white') +
  expand_limits(x = states_map$long, y = states_map$lat) +
  geom_text(aes(x=long,y=lat,label=label, map_id = region),data=cnamesBoy)


ggplot(girlNames,aes(map_id = State)) +
  geom_map(aes(fill = TopName), map = states_map,color='white') +
  expand_limits(x = states_map$long, y = states_map$lat) +
  geom_text(aes(x=long,y=lat,label=label, map_id = region),data=cnamesGirl)
