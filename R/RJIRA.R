

## Create the connection without OAUTH authentication
simpleConnection <- function (URL, port, version=NULL) {
  
  if (is.null(version)|| (version != 2 &  version !=1))
    rapiVersion <- "latatest"
  else
    rapiVersion <- version
  
  simpleURL <- cat("https://",URL,":",port,"/rest/api/", rapiVersion,"/", sep="")
  return(simpleURL)
}

# For retrieving the most interesting information from 
# request issue to dataframe
getIssue <- function (conn, key, type = "GET") {
  jsIssue <- fromJSON(conn)
  
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

convertNull2NA <- function (x) {
  if (is.null(x))
    return(NA)
  else
    return(x)
}