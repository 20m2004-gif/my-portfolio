#CSVファイルを読み込む場合
data <- read.csv("./data/tree2.csv", fileEncoding = 'SHIFT-JIS', header = T, stringsAsFactors = FALSE)
head(data)
str(data)

# 最初の列（ナンバーの列）を削除
data <- data[, -1]
head(data)
str(data)

library(rpart)

# sex 列の値を英語に置換
data$sex[data$sex == "男性"] <- "Male"
data$sex[data$sex == "女性"] <- "Female"

# 日本語の列名と対応する英語の列名のベクトルを作成
japanese_names <- c("sex", "有名人のファッションを参考にする", "異性の目を気にする", "奇抜なファッションが好き", "TPOに気をつかう", "体型をカバーできる服を着る", "好きなブランドがある")
english_names <- c("sex", "ref_fashion", "care_opposite_sex", "like_unique_fashion", "consider_TPO", "wear_cover_figure", "has_fav_brand")

# 列名を変更
colnames(data) <- english_names

# 列名が変更されたか確認
head(data)
str(data)

# YesとNoをYes = 1, No = 2になるように変換
cols_to_convert <- c("ref_fashion", "care_opposite_sex", "like_unique_fashion", "consider_TPO", "wear_cover_figure", "has_fav_brand")

for (col in cols_to_convert) {
  data[[col]] <- ifelse(data[[col]] == "Yes", 1, 0)
}

# 変換できているかの確認
head(data)
str(data)

############################################################################
# ここまでが前処理
############################################################################


############################################################################

#ここからの目的について

# どのようなファッションに関する特徴量（「有名人のファッションを参考にするか」
# 「異性の目を気にするか」など）が、男女の性別を分ける上で重要なのか?
# それらの特徴量の値の組み合わせによって、どちらの性別である可能性が高いのか？
# この目的のために決定木分析を行い、
# 将来のデータに基づいて性別を予測するための予測モデルとしても利用できるようにする

############################################################################

# 決定木モデルの作成
data.rp <- rpart(sex ~., data = data)
print(data.rp, digits = 2)

# 基本的なプロット
plot(data.rp, uniform = TRUE, branch = 0.6, margin = 0.05)
text(data.rp, use.n = TRUE, all = TRUE)

# CPテーブルの表示
printcp(data.rp)

# 基本的にここで終了
############################################################################

# plotの解釈（覚書）
# どちらが多いかが枝の根に英語で表示されている

############################################################################

# ここまでの結果の解釈

# ①体型をカバーできる服を着る」傾向（Yes=1）がある人は女性である可能性が高い。
# ②「体型をカバーできる服を着る」傾向がなく（No=0）
# さらに「異性の目を気にする」傾向（Yes=1）がある人も女性である可能性が高い。
# ③「体型をカバーできる服を着る」傾向がなく（No=0）
# 「異性の目を気にする」傾向もない（No=0）人は男性である可能性が高い。

# 結果からの考察
# ①自身の外見を異性に意識する層において、
# 体型カバーよりもファッション性を重視する傾向がある女性の存在を示唆してるかも
# ②自身の外見を異性に意識する層において、
# 体型カバーよりもファッション性を重視する傾向がある女性の存在を示唆しているかも
# ③服装において体型カバーを意識せず、
# 異性の目を特に気にしない層に男性が多い可能性を示唆

# 今回の結果からのビジネスへの影響について
# マーケティング戦略: 
# 特定の性別をターゲットとした商品開発やプロモーション戦略を検討する際に、
# これらの傾向を考慮することで、より効果的なアプローチが可能になるかも
# 例えば、体型カバーを重視する女性層には、機能性とデザイン性を両立させた商品を訴求
# 異性の目を気にする女性層には、トレンド感のあるファッションを提案
# 商品開発: 
# 今後の商品開発において、これらの傾向を踏まえた品揃えを検討することで、
# より顧客ニーズに合致した商品を提供できる可能性
# 顧客理解: 
# 既存顧客のデータと照らし合わせることで、顧客層の理解を深め、
# よりパーソナライズされたサービス提供につながるかも

# 今後について
# ❶他のファッションに関する質問項目や顧客属性との関連性を分析する
# このことにより、より多角的な視点から顧客理解を深める
# ❷定性的な調査を併用する
# 顧客へのインタビューやアンケート調査を通じて、
# 定量分析で得られた傾向の背景にある理由や心理を探る
# ❸これらの洞察を実際のマーケティング施策に試験的に導入し、効果を検証する
# データに基づいた意思決定のサイクルを回す

############################################################################


############################################################################
# より複雑な決定木を作成する
# cpを決定するためにグラフで可視化
dev.off()
plotcp(data.rp)

# minsplitの場合
data.rp <- rpart(sex ~., data = data, minsplit = 10)
print(data.rp, digits = 2)

# 基本的なプロット
plot(data.rp, uniform = TRUE, branch = 0.6, margin = 0.05)
text(data.rp, use.n = TRUE, all = TRUE)

# CPテーブルの表示
printcp(data.rp)

# cpの場合
data.rp <- rpart(sex ~., data = data, cp = 0.02)
print(data.rp, digits = 2)

# 基本的なプロット
plot(data.rp, uniform = TRUE, branch = 0.6, margin = 0.05)
text(data.rp, use.n = TRUE, all = TRUE)

# CPテーブルの表示
printcp(data.rp)

# rpart関数の利用可能なすべてのパラメータと詳細の表示
?rpart



############################################################################
# 以下からは個人的なメモ


set.seed(0)
data.rp2 <- rpart(sex ~., data = data, minsplit = 3, cp = 0)
printcp(data.rp2)
# xerrorが0.07になり始めたタイミングが剪定タイミング

plotcp(data.rp2)
# 深くない過ぎないように目安の点線が引かれている

data.rp3 <- prune(data.rp2, cp = 0.090)





# 行と列の確認
data
print(data)

head(data)      # 最初の6行を表示
head(data, n = 10) # 最初の10行を表示

tail(data)      # 最後の6行を表示
tail(data, n = 8)  # 最後の8行を表示

str(data)

summary(data)

dim(data)
# 出力例: [1] 100 5  (100行、5列)

even.n <- 2*(1:1559)-1
data.train <- data[even.n, ]
data.test <- data[-even.n, ]

set.seed(20)
data.rp2 <- rpart(sex ~., data = data.train)
plotcp(data.rp2)


data.rp3 <- predict(data.rp2, data.test[, -1], type = "class")
table(data.test[, 1], data.rp3)





# 結果の表示
print(model_sex, digits = 2)

# プロット
plot(model_sex, uniform = TRUE, branch = 0.6, margin = 0.05)
text(model_sex, use.n = TRUE, all = TRUE)

# CPテーブル
printcp(model_sex)

# 目的変数を ref_fashion に変更
model_ref_fashion <- rpart(ref_fashion ~ sex + care_opposite_sex + like_unique_fashion + consider_TPO + wear_cover_figure + has_fav_brand,
                   data = data,
                   method = "class")

# 結果の表示
print(model_ref_fashion, digits = 2)

# プロット
plot(model_ref_fashion, uniform = TRUE, branch = 0.6, margin = 0.05)
text(model_ref_fashion, use.n = TRUE, all = TRUE)

# CPテーブル
printcp(model_sex)























