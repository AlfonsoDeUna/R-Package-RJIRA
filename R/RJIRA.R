##' -----------------------------------------------------------------
##' R interface with JIRA instance
##' @author Alfonso de Uña del Brio <briofons@gmail.com>
##' 
##' Interface with JIRA with the idea
##' to get information that you need to start making
##' studies from your issues.
##' 
##' 
##'
##'


library(RCurl)
library(jsonlite)

##' 
##' It creates the connection without OAUTH authentication
##' 
##' @param URL Base JIRA direction
##' @param port Port JIRA (by default 8080)
##' @param version (Rest API JIRA)
##' 
##' @return Base URL for use REST API
##' 
simpleConnection <- function (URL, port, version=NULL) {
  
  if (is.null(version)|| (version != 2 &  version !=1))
    rapiVersion <- "latatest"
  else
    rapiVersion <- version
  
  simpleURL <- cat("https://",URL,":",port,"/rest/api/", rapiVersion,"/", sep="") 
  return(simpleURL)
  
}

##'
##'
##'
##'
##'
createRow <- function (dat) {
  row <- list( convertNull2NA(dat$key),
            convertNull2NA(dat$fields$summary),
            convertNull2NA(dat$fields$issuetype$name),
            convertNull2NA(dat$fields$issuetype$id),
            convertNull2NA(dat$fields$status$name),
            convertNull2NA(dat$fields$status$id),
            convertNull2NA(dat$fields$reporter$name),
            convertNull2NA(dat$fields$reporter$emailAddress),
            convertNull2NA(dat$reporter$displayName),
            convertNull2NA(dat$assignee$name),
            convertNull2NA(dat$fields$assignee$emailAddress),
            convertNull2NA(dat$fields$assignee$displayName),
            convertNull2NA(dat$fields$project$id),
            convertNull2NA(dat$fields$project$name),
            convertNull2NA(dat$comment$total))
  return(row)
}

##'
##' 
##'   For retrieving the most interesting information from 
##'   request issue to dataframe
##'   
##'   @param conn
##'   @param key
##'   @param type (by default GET)
##'   
getIssue <- function (conn, key, type = "GET") {
  
  con<-cat(conn,"/issue/",key,sep="")
  campaignJSON = getURL(url = paste(conn,'issue/',key,sep="") ,.opts = list(ssl.verifypeer = FALSE))
  jsIssue <- fromJSON(campaignJSON)
  
  #
  dt <- data.frame (createRow (jsIssue), stringsAsFactors=FALSE)
  
  cols <- c(   "key",
               "summary",
               "issueType.name",
               "issueType.IsSubtask",
               "status.name",
               "status.id",
               "reporter.name",
               "reporter.mail",
               "reporter.display.name",
               "assignee.name",
               "assignee.mail",
               "assignee.display.name",
               "project.id",
               "project.name",
               "comments.total"
  )
  
  colnames(dt)<-cols
  
  return(dt)
                    
}

##'
##'    for testing https://jira.atlassian.com/rest/api/latest/search?
##'    
##'
##'    @param conn
##'    @param String with the format of jql (JIRA Query Language)
##'        
##'                
freeQuery <-function (conn, jql = NULL) {
 
  campaignJSON = getURL(url = paste(conn,jql,sep="") ,.opts = list(ssl.verifypeer = FALSE))
  jsIssue <- fromJSON(campaignJSON)
  lista<-jsIssue$issue$key
  cont <-0
  for (i in lista){
    if (cont == 0){
      data<-getIssue (conn, i)
    }
    else{
      print (i)
      campaignJSON = getURL(url = paste(conn,'issue/',i,sep="") ,.opts = list(ssl.verifypeer = FALSE))
      jsIssue <- fromJSON(campaignJSON)
      data<-rbind(data,createRow(jsIssue))
    }
    cont <- cont +1
  }
  return (data)
  
}

convertNull2NA <- function (x) {
  if (is.null(x))
    return(NA)
  else
    return(x)
}