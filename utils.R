#---------------------------------------------------------------------------------
# utils.R
#
# The MIT License
#
# Copyright (c) 2011 Takeshi Arabiki (@a_bicky)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#---------------------------------------------------------------------------------


extractScreenNames <- function(text, strict = TRUE) {
    if (strict) {
        # Twitter で screen_name と見なされるものを抽出できるはず
        regex <- "(?:(?<!\\w)([@＠])((?>\\w+))(?![@＠])|[\\s\\S])"
    } else {
        # 例えば hoge@example.com などメールアドレスにもマッチする
        regex <- "(?:([@＠])(\\w+)|[\\s\\S])"
    }
    screenNames <- gsub(regex, "\\1\\2", text, perl = TRUE)
    unique(unlist(strsplit(substring(screenNames, 2), "[@＠]")))
}


removeURL <- function(text, strict = TRUE) {
    if (strict) {
        # 手前に英数字とかがなくて、間にbasic認証があるかもしれなくて（ちなみにTwitterだとURLとみなされない）
        # 有効なドメイン名で・・・という文字列を取り除く
        regex <- "(?<![-.\\w#@=!'\"/])https?://(?:[^:]+:.+@)?(?:[0-9A-Za-z][-0-9A-Za-z]*(?<!-)\\.)+[A-za-z]+(?:/[-\\w#%=+,.?!&~]*)*"
    } else {
        regex <- "https?://[-\\w#%=+,.?!&~/]+"
    }
    gsub(regex, "", text, perl = TRUE)
}


removeScreenName <- function(text, strict = TRUE) {
    if (strict) {
        # Twitter で screen_name と見なされるものを抽出できるはず
        regex <- "(?<!\\w)[@＠](?>\\w+)(?![@＠])"
    } else {
        # 例えば hoge@example.com などメールアドレスにもマッチする
        regex <- "[@＠]\\w+"
    }
    gsub(regex, "", text, perl = TRUE)
}


removeHashTag <- function(text, strict = TRUE) {
    delimiters <- "\\s,.\u3000-\u3002\uFF01\uFF1F"
    # cf. http://nobu666.com/2011/07/13/914.html
    validJa <- "\u3041-\u3094\u3099-\u309C\u30A1-\u30FA\u30FC\u3400-\uD7A3\uFF10-\uFF19\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFF9E"
    if (strict) {
        regex <- sprintf("(^|[%s])(?:([#＃](?>[0-9]+)(?!\\w))|[#＃][\\w%s]+)", delimiters, validJa, validJa)
    } else {
        regex <- sprintf("[#＃][^%s]+", delimiters)
    }
    gsub(regex, "\\1\\2", text, perl = TRUE)
}


removeSpecialStr <- function(text) {
    removeURL(removeHashTag(removeScreenName(text)))
}
