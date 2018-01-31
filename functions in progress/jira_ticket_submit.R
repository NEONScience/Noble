
#Experimental function. Do not use yet.
.ticket.submit=function(username, password){
    #require(base64enc)
    require(httr)
    require(jsonlite)
    #login for a session
    httr::POST(url="https://neoninc.atlassian.net/rest/auth/1/session", body = list(username=username, password=password), encode = "json") #This works to login (status 200)
    httr::POST(url="https://neon.service-now.com/rest/auth/1/session", body = list(username=username, password=password), encode = "json") #This works to login (status 200)


    #Testing how to get info from JIRA
    temp=httr::GET(url = "https://neoninc.atlassian.net/rest/api/2/issue/NEON-12063")
    ticket=jsonlite::fromJSON(base::rawToChar(temp$content))

    test=httr::GET("https://neoninc.atlassian.net/rest/api/2/issue/NEON-10000?expand=names,renderedFields")
    ticket=jsonlite::fromJSON(base::rawToChar(test$content))

    httr::GET("https://instance.service-now.com/api/now/v1/table/incident")
}
