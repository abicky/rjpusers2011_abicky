library(reshape2)
library(plyr)
if (!exists("statusDF")) {
    source("init.R")
}

mstatus <- melt(statusDF,
                id.vars = c("statusSource", "wday", "year", "month", "week", "date", "hour"),
                measure.vars = c("textLength", "cleanTextLength"))

#------------------------#
# ツイート数の確認（1軸）#
#------------------------#
# 曜日ごとのツイート数を見てみる
print(acast(mstatus, . ~ wday, length, subset = .(variable == "textLength")))

#------------------------#
# ツイート数の確認（2軸）#
#------------------------#
# 時間軸を加えてみる
print(acast(mstatus, hour ~ wday, length, subset = .(variable == "textLength")))

#------------------------#
# ツイート数の確認（3軸）#
#------------------------#
# さらに月を軸に加えてみる
# 行列で出力
print(acast(mstatus, hour ~ wday + month, length, subset = .(variable == "textLength")))
# 3次元配列で出力
print(acast(mstatus, hour ~ wday ~ month, length, subset = .(variable == "textLength")))


#-----------------------------------------------#
# Twitterクライアントごとの平均文字数と標準偏差 #
#-----------------------------------------------#
print(dcast(mstatus, statusSource ~ .,
      function(x) list(c(mean = mean(x), sd = sd(x))),
      fill = list(c(mean = NaN, sd = NA)),
      subset = .(variable == "textLength")))

#----------------------#
# 2標本t検定による検証 #
#----------------------#
pc <- unlist(subset(statusDF,
                    statusSource %in% c("YoruFukurou", "web"),
                    textLength))
sp <- unlist(subset(statusDF,
                    grepl("(iPhone|Android)", statusSource),
                    textLength))
t.test(sp, pc, var.equal = FALSE)


#---------------------------------------------#
# おまけ 〜よくやり取りする人が誰か知りたい〜 #
#---------------------------------------------#
screenNames <- unlist(lapply(statusDF$text, extractScreenNames))
head(sort(table(screenNames), decreasing = TRUE), 10)  # トップ10
