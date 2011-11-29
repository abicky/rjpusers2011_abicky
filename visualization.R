library(ggplot2)
if (!exists("statusDF")) {
    source("init.R")
}


#----------------------#
# whotwiのグラフを描く #
#----------------------#
# 時間・曜日・Twitterクライアントごとのツイート数
# いちいち melt して cast が面倒なので xtabs を使用（えっ
cnt <- as.data.frame(xtabs(~ hour + wday + statusSource, statusDF))
freqSources <- by(cnt, cnt[c("hour", "wday")], function(df) {
    # 一番頻度の高いTwitterクライアントを抽出
    freqSource <- with(df, statusSource[order(Freq, decreasing = TRUE)[1]])
    cbind(df[1, c("hour", "wday")], freqSource)
})
freqSources <- do.call(rbind, freqSources)
# 時間・曜日ごとのツイート数
cntSum <- as.data.frame(xtabs(Freq ~ hour + wday, cnt))
# 利用頻度の高いクライアントの情報をくっつける
data <- merge(cntSum, freqSources)
# 月曜日がグラフの上に来るように調整
data$wday <- factor(data$wday, levels = rev(levels(data$wday)))
# ツイート数に差がありすぎるので緩和
data$Freq <- log2(data$Freq)
p <- qplot(hour, wday, data = data, xlab = "", ylab = "",
           geom = "point", colour = freqSource, size = Freq)
print(p)


#---------------#
# もう一工夫！！#
#---------------#
# whotwi用theme
theme_whotwi <- function() {
    opts(# バックグラウンドの色をなし（白）にする
         panel.background = theme_rect(fill = NA, colour = NA),
         # 凡例の項目のバックグラウンドの色をなし（白）にする
         legend.key = theme_rect(fill = NA, colour = NA),
         # 目盛りを色をなし（白）にする
         axis.ticks = theme_segment(colour = NA))
}
# 凡例のタイトルを削除 & サイズの凡例を削除
p2 <- p + theme_whotwi() + scale_size(legend = FALSE) + scale_colour_hue(name = "")
print(p2)
