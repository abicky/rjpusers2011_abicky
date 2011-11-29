source("conf.R")
source("init_twitteR.R")
source("utils.R")
wday.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

if (useCache) {
    rdata <- sprintf("data/statuses-%s.RData", targetUser)
    if (file.exists(rdata)) {
        cat("loading data...\n")
        load(rdata)
    } else {
        cat("get user timeline...\n")
        statuses <- userTimeline(targetUser, n = 3200)
        if (!file.exists("data")) {
            dir.create("data")
        }
        save(statuses, file = rdata)
    }
} else {
    statuses <- userTimeline(targetUser, n = 3200)
}

cat("make data.frame and format it...\n")
statusDF <- twListToDF(statuses)
statusDF <- within(statusDF, {
    attr(created, "tzone") <- "Asia/Tokyo"
    statusSource <- factor(gsub("<a .*?>(.*?)</a>", "\\1", statusSource))
    date <- factor(format(created, "%Y-%m-%d"))
    hour <- NULL; month <- NULL; year <- NULL; wday <- NULL
    with(as.POSIXlt(created), {
        hour <<- factor(hour)
        month <<- factor(mon + 1)
        year <<- factor(year + 1900)
        wday <<- factor((wday + 6) %% 7, labels = wday.abb)
    })
    week <- factor(format(created - 24 * 60 * 60 * (as.integer(wday) - 1), "%Y-%m-%d"))
    textLength <- nchar(text)
    # ユーザ名, URL, ハッシュタグを削除（自作関数。utils.R参照。）
    cleanText <- removeSpecialStr(text)
    cleanTextLength <- nchar(cleanText)
})
topSources <- names(head(sort(table(statusDF$statusSource), decreasing = TRUE), 5))
statusDF <- within(statusDF, {
    statusSource <- as.character(statusSource)
    statusSource[!statusSource %in% topSources] <- "other"
    # 水準は利用頻度の高いものが先になるようにソートする
    statusSource <- factor(statusSource, levels = names(sort(table(statusSource), dec = TRUE)))
})
