

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
  row <- c ( convertNull2NA(jsIssue$key),
            convertNull2NA(jsIssue$fields$summary),
            convertNull2NA(jsIssue$fields$issuetype$name),
            convertNull2NA(jsIssue$fields$issuetype$id),
            convertNull2NA(jsIssue$fields$status$name),
            convertNull2NA(jsIssue$fields$status$id),
            convertNull2NA(jsIssue$fields$reporter$name),
            convertNull2NA(jsIssue$fields$reporter$emailAddress),
            convertNull2NA(jsIssue$fields$reporter$displayName),
            convertNull2NA(jsIssue$fields$assignee$name),
            convertNull2NA(jsIssue$fields$assignee$emailAddress),
            convertNull2NA(jsIssue$fields$assignee$displayName),
            convertNull2NA(jsIssue$fields$project$id),
            convertNull2NA(jsIssue$fields$project$name),
            convertNull2NA(jsIssue$comment$total))
  return(row)
}

# For retrieving the most interesting information from 
# request issue to dataframe
getIssue <- function (conn, key, type = "GET") {
  
  con<-cat(conn,"/issue/",key,sep="")
  jsIssue <- fromJSON(con)
  
  #create a table
  
 
dt <- data.frame ( convertNull2NA(jsIssue$key),
                  convertNull2NA(jsIssue$fields$summary),
                  convertNull2NA(jsIssue$fields$issuetype$name),
                  convertNull2NA(jsIssue$fields$issuetype$id),
                  convertNull2NA(jsIssue$fields$status$name),
                  convertNull2NA(jsIssue$fields$status$id),
                  convertNull2NA(jsIssue$fields$reporter$name),
                  convertNull2NA(jsIssue$fields$reporter$emailAddress),
                  convertNull2NA(jsIssue$fields$reporter$displayName),
                  convertNull2NA(jsIssue$fields$assignee$name),
                  convertNull2NA(jsIssue$fields$assignee$emailAddress),
                  convertNull2NA(jsIssue$fields$assignee$displayName),
                  convertNull2NA(jsIssue$fields$project$id),
                  convertNull2NA(jsIssue$fields$project$name),
                  convertNull2NA(jsIssue$comment$total))
  
  cols <- c("key",
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

## for testing https://jira.atlassian.com/rest/api/latest/search?
freeQuery <-function (conn, query = NULL) {
  jsIssue <- fromJSON(conn)
  lista<-jsIssue$issue$key
  cont <-0
  for (i in lista){
    if (cont == 0){
      data<-getIssue (conn, i)
    }
    else{
      con<-cat(conn,"/issue/",key,sep="")
      jsIssue <- fromJSON(con)
      rbind (data, createRow(jsIssue))
    }
  }
  
  
}

convertNull2NA <- function (x) {
  if (is.null(x))
    return(NA)
  else
    return(x)
}