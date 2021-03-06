##########
## This script navigates and scrapes the FIFA website for a history of national men's team rankings
## it creates a data.frame and writes it to file as a csv


###########################
##TO RUN: Set working Directory to the Location of the Project
HOMEDIR <- "F:/Docs/Personal/rross"
#HOMEDIR <- "C:/Users/Ross/Documents/R/rross"
setwd(paste0(HOMEDIR,"/Soccer/2014/WorldCup/"))


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

metaURL <-"http://www.fifa.com/worldranking/rankingtable"
metaParse <- htmlParse(metaURL)

########################################

getRankingTableURL <- function(
  gender="m",
  rank=238, #238 is most recent as of May2014 - counts down to older
  confederation=0, #0 gets all teams
  page=1 #counts up from one for more and more teams
  ){
  base="http://www.fifa.com/worldranking/rankingtable"
  tail="_ranking_table.html"
  return(paste0(base,
               "/gender=",gender,
               "/rank=",rank,
               "/confederation=",confederation,
               "/page=",page,
               "/",
               tail)
         ) 
}
getRankingTableURL(confederation=23913)

#######################################
### Get the nodes I want via XPATH query
#
start_rank = 238 #may want to replace with scraped number
end_rank = 57 #57 corresponds to Jan99, when point method was revised
conf_ids = c(23913,23914,23915,23916,25998,27275)

# start_page = 1
# max_page_glbl = 4 #to reduce time scraping irrelevant teams
# pointDF <- data.frame()
# for(rank in start_rank:end_rank){
#   
#   tableURL <- getRankingTableURL(rank=rank,page=start_page)
#   tableParse <- htmlParse(tableURL)
#   
#   #Get number of rank table pages
#   #pg <- sapply(getNodeSet(tableParse,"//div[@class='ctntH_paging']")[[1]]['a'],function(x){xmlValue(x)})
#   npg <- length(getNodeSet(tableParse,"//div[@class='ctntH_paging']")[[1]]['a'])
#   
#   for(pagei in 1:min(c(max_page_glbl,npg))){
#     if(pagei!=1){
#       tableURL <- getRankingTableURL(rank=rank,page=pagei)
#       tableParse <- htmlParse(tableURL)
#     }
#     
#     #Get date of the table
#     dtString <- xmlValue(getNodeSet(tableParse,"//div[@class='rnkwrap rnkdate']")[[1]][['div']])
#     dt <- as.Date(paste0("1 ",gsub("^\\s*","",dtString)),"%d %B %Y")
#     
#     #Get table data
#     tableList <- getNodeSet(tableParse,"//table[@id='tbl_rankingTable']")
#     tableM <-  t(
#                     sapply(
#                     tableList[[1]][['tbody']]['tr'],
#                     function(x){
#                       tdList <- x['td']
#                       c(
#                         #rank=xmlValue(tdList[[2]]),
#                         name=gsub("^\\s*","",xmlValue(tdList[[3]])),
#                         #abr=gsub("'","",strsplit(xmlGetAttr(x,"onclick"),",")[[1]][2]),
#                         points=gsub("^\\s*","",xmlValue(tdList[[4]]))
#                         )
#                       }
#                     )
#                   )
#     
#     #Turn table matrix in to data.frame
#     pointDF <- rbind(pointDF,data.frame(date=dt,tableM, stringsAsFactors=F))
#   }
# }




pointDF <- data.frame()
for(rank in start_rank:end_rank){
  for(confi in conf_ids){
    tableURL <- getRankingTableURL(rank=rank,confederation=confi)
    tableParse <- htmlParse(tableURL)
    
    #Get date of the table
    dtString <- xmlValue(getNodeSet(tableParse,"//div[@class='rnkwrap rnkdate']")[[1]][['div']])
    dt <- as.Date(paste0("1 ",gsub("^\\s*","",dtString)),"%d %B %Y")
    
    #Get table data
    tableList <- getNodeSet(tableParse,"//table[@id='tbl_rankingTable']")
    tableM <-  t(
                    sapply(
                    tableList[[1]][['tbody']]['tr'],
                    function(x){
                      tdList <- x['td']
                      c(
                        #rank=xmlValue(tdList[[2]]),
                        name=gsub("^\\s*","",xmlValue(tdList[[4]])),
                        #abr=gsub("'","",strsplit(xmlGetAttr(x,"onclick"),",")[[1]][2]),
                        points=gsub("^\\s*","",xmlValue(tdList[[5]]))
                        )
                      }
                    )
                  )
    
    #Turn table matrix in to data.frame
    pointDF <- rbind(pointDF,data.frame(date=dt,confid=confi,tableM, stringsAsFactors=F))
  }
}


write.csv(pointDF,"fifa_rank_history.csv",row.names=F)

########################
# Some cursory analysis
########################

dt <- data.table(transform(read.csv("fifa_rank_history.csv",stringsAsFactors=F),date=as.Date(date)),key=c("name","date"))

top30 <- dt[date==max(date)][rank(-points)<=30]$name

ggplot(dt[top30],aes(x=date,y=points,col=name)) + geom_line()
ggplot(dt[top30][date==max(date)],aes(x=rank(points),y=points,fill=name)) + geom_bar(stat="identity") + geom_text(aes(x=rank(points),y=points,label=name,angle=90))

rank(dt[top30][date==max(date)]$points)
