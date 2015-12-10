

## Create the connection without OAUTH authentication
simpleConnection <- function (URL, port, version=NULL) {
  
  if (is.null(version)|| (version != 2 &  version !=1))
    rapiVersion <- "latatest"
  else
    rapiVersion <- version
  
  simpleURL <- cat("https://",URL,":",port,"/rest/api/", rapiVersion,"/", sep="")
  
  return(simpleURL)
}

createRow <- function (dat) {
  row <- c ( convertNull2NA(dat$key),
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

# For retrieving the most interesting information from 
# request issue to dataframe
getIssue <- function (conn, key, type = "GET") {
  
  con<-cat(conn,"/issue/",key,sep="")
  campaignJSON = getURL(url = paste(conn,'issue/',key,sep="") ,.opts = list(ssl.verifypeer = FALSE))
  jsIssue <- fromJSON(campaignJSON)
  
  #create a table
  
 
  dt <- data.frame (createRow (jsIssue))
  
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
  
  #colnames(dt)<-cols
  
  return(dt)
                    
}

## for testing https://jira.atlassian.com/rest/api/latest/search?
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
      campaignJSON = getURL(url = paste(conn,'issue/',i,sep="") ,.opts = list(ssl.verifypeer = FALSE))
      jsIssue <- fromJSON(campaignJSON)
      rbind (data, createRow(jsIssue))
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