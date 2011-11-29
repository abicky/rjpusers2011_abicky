library(twitteR)
library(ROAuth)
Sys.setlocale("LC_TIME", "en_US.utf-8")

if (useOAuth) {
    if (TRUE) {
        twitOauth <- OAuthFactory$new(handshakeComplete = TRUE,
                                      signMethod        = "HMAC",
                                      consumerKey       = consumerKey,
                                      consumerSecret    = consumerSecret,
                                      oauthKey          = oauthKey,
                                      oauthSecret       = oauthSecret)
    } else {
        twitOauth <- OAuthFactory$new(consumerKey    = consumerKey,
                                      consumerSecret = consumerSecret,
                                      requestURL     = requestURL,
                                      accessURL      = accessURL,
                                      authURL        = authURL)
        twitOauth$handshake()
    }
    registerTwitterOAuth(twitOauth)
}
