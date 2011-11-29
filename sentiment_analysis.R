library(RMeCab)
library(ggplot2)
if (!exists("statusDF")) {
    source("init.R")
}


#------------------#
# ネガポジ度の算出 #
#------------------#
# 単語感情極性対応表の取得
pndic <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic",
                    sep = ":",
                    col.names = c("term", "kana", "pos", "value"),
                    colClasses = c("character", "character", "factor", "numeric"),
                    fileEncoding = "Shift_JIS")
# 名詞＋品詞で複数の候補がある場合は平均値を採用
# 「アイス」の読みとして「アイス」、「アイスホッケー」が割り当てられている例もある
pndic2 <- aggregate(value ~ term + pos, pndic, mean)

# pndic に登録されている品詞のみ抽出
pos <- unique(pndic2$pos)
tweetDF <- docDF(statusDF, column = "cleanText", type = 1, pos = pos)
# pndic に登録されている単語のみ抽出
tweetDF <- subset(tweetDF, TERM %in% pndic2$term)
# 単語極性スコアを付与
tweetDF <- merge(tweetDF, pndic2, by.x = c("TERM", "POS1"), by.y = c("term", "pos"))

# 単語の出現回数にスコアを掛けて総和を取る
score <- colSums(tweetDF[4:(ncol(tweetDF) - 1)] * tweetDF$value)
# ポジティブツイート数
print(sum(score > 0))
# ネガティブツイート数
print(sum(score < 0))
# どちらでもないツイート数
print(sum(score == 0))
# 可視化
scoreType <- factor(ifelse(score > 0, "positive",
                           ifelse(score == 0, "neutral", "negative")),
                    levels = c("positive", "neutral", "negative"))
print(qplot(x = factor(1), geom = "bar", fill = scoreType) + coord_polar(theta = "y"))



#--------------#
# ちょっと確認 #
#--------------#
print(table(ifelse(pndic$value > 0, "positive",
             ifelse(pndic$value == 0, "neutral", "negative"))))

#----------------------#
# 相対的にネガポジ判定 #
#----------------------#
m <- mean(score)
# 平均スコアでポジティブとネガティブを分離
tweetType <- factor(ifelse(score > m, "positive",
                    ifelse(score == m, "neutral", "negative")),
                    levels = c("positive", "neutral", "negative"))
print(table(tweetType))

#----------------------------#
# 相対的にネガポジ判定可視化 #
#----------------------------#
statusDF$tweetType <- droplevels(tweetType)
qplot(month, data = statusDF,
      geom = "bar", fill = tweetType, position = "fill")
