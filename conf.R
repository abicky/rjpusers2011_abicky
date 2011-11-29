useOAuth <- FALSE
# for OAuth authentication
# set your consumerKey and consumerSecret if useOAuth is TRUE
{
    requestURL     <- "https://api.twitter.com/oauth/request_token"
    accessURL      <- "http://api.twitter.com/oauth/access_token"
    authURL        <- "http://api.twitter.com/oauth/authorize"
    consumerKey    <- "Your Consumer key"
    consumerSecret <- "Your Consumer secret"
    # optional
    oauthKey       <- "Your Access token"
    oauthSecret    <- "Your Access token secret"
}

# target user, e.g. a_bicky
targetUser <- "a_bicky"

# save userTimeline result and load it after that if useCache is TRUE
useCache <- TRUE
