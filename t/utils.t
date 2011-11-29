test.extractScreenNames <- function() {
    testCase <- list(list("a@a_bicky", character(0), "a_bicky"),
                     list("あ@a_bicky", "a_bicky", "a_bicky"),
                     list("あ@a_bicky\n", "a_bicky", "a_bicky"),
                     list("あ@a_bicky", "a_bicky", "a_bicky"),
                     list("@a_bicky@a_bicky", character(0), "a_bicky"),
                     list("@ほげ", character(0), character(0)),
                     list("ほげ、@a_bicky ＠a_bicky、 @a_bicky_dev？",
                          c("a_bicky", "a_bicky_dev"),
                          c("a_bicky", "a_bicky_dev")))
    for (test in testCase) {
        checkEquals(extractScreenNames(test[[1]]), test[[2]])
        checkEquals(extractScreenNames(test[[1]], FALSE), test[[3]])
    }
}


test.removeURL <- function() {
    testCase <- list(c("http://hoge.com", "", ""),
                     c("hoge https://handai-junko.com/?a=1&b=2 fuga", "hoge  fuga", "hoge  fuga"),
                     c("https://a.com/?a=%e3%a1+", "", ""),
                     c("http://username:password@example.com/fuga/#tag_test", "", ":password@example.com/fuga/#tag_test"),
                     c("http://twitter.com/#!/a_bicky", "", ""),
                     c("http://twitter.com/~a_bicky/", "", ""),
                     c("http://twitter.com/@a_bicky/", "@a_bicky/", "@a_bicky/"),
                     c("\"http://twitter.com/@a_bicky/", "\"http://twitter.com/@a_bicky/", "\"@a_bicky/"))
    for (test in testCase) {
        checkEquals(removeURL(test[1]), test[2])
        checkEquals(removeURL(test[1], FALSE), test[3])
    }
}


test.removeScreenName <- function() {
    testCase <- list(c("a@a_bicky", "a@a_bicky", "a"),
                     c("あ@a_bicky", "あ", "あ"),
                     c("あ@a_bicky\n", "あ\n", "あ\n"),
                     c("あ＠a_bicky", "あ", "あ"),
                     c("@a_bicky@a_bicky", "@a_bicky@a_bicky", ""),
                     c("@ほげ", "@ほげ", "@ほげ"))
    for (test in testCase) {
        checkEquals(removeScreenName(test[1]), test[2])
        checkEquals(removeScreenName(test[1], FALSE), test[3])
    }
}


test.removeHashTag <- function() {
    testCase <- list(c("#test test", " test", " test"),
                     c("＃てすと", "", ""),
                     c("@#hogehoge", "@#hogehoge", "@"),
                     c("#hoge　fuga", "　fuga", "　fuga"),
                     c("#「 #」 #ー #＾ #％ #〜 #！ #＠ #＃ #＄ #＆ #＊ #（ #） #＿ #＝ #｛ #｝ #｜ #？ #＜ #＞ #、 #。",
                       "#「 #」  #＾ #％ #〜 #！ #＠ #＃ #＄ #＆ #＊ #（ #） #＿ #＝ #｛ #｝ #｜ #？ #＜ #＞ #、 #。",
                       "      #！             #？   #、 #。"),
                     c("。#test", "。", "。"),
                     c(".#test", ".", "."),
                     c("#123", "#123", ""),
                     c("#1a", "", ""),
                     c("あ#test", "あ#test", "あ"))
    for (test in testCase) {
        checkEquals(removeHashTag(test[1]), test[2])
        checkEquals(removeHashTag(test[1], FALSE), test[3])
    }
}
