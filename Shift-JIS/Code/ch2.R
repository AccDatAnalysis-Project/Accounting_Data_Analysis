# 作業環境のリセット
remove(list = ls(all = TRUE))


# 基本計算の実施 -----------------------------------------------------------------

# お試し計算
1 + 2
sqrt(2)

# ベクトル
c(1, 2, 3) + c(2, 3, 4)
c(1, 2, 3) * c(2, 3, 4)
sqrt(c(1, 2, 3))

# 代入
vec_1 <- c(1, 2, 3)
vec_2 <- c(2, 3, 4)

vec_1 + vec_2
vec_1 * vec_2
sqrt(vec1)

# 型
charvec <- c("a", "b", "c")
charvec

vec1 + charvec

class(vec1)
class(charvec)


# データを読み込んで処理 -------------------------------------------------------------

# 作業環境のリセット
remove(list = ls(all = TRUE))

# パッケージのインストール
install.packages("tidyverse")

# パッケージの読み込み
library(tidyverse)

# データの読み込み
df_0 <- read_csv("ch2.csv",
  locale = locale(encoding = "cp932")
)

# データの表示
df_0

# 下記4つはすべて同じ操作
df_0[, c(1:4, 6)]
df_0[, c("銘柄コード", "会社名", "優先市場", "業種", "売上高")]
select(df_0, 銘柄コード, 会社名, 優先市場, 業種, 売上高)
df_0 %>% 
  select(銘柄コード, 会社名, 優先市場, 業種, 売上高)

# 必要な列を残して、列名を英語表記にする
df_1 <- df_0 %>%
  select(
    銘柄コード,
    会社名,
    業種,
    決算月,
    売上高,
    売上原価,
    販管費
  ) %>%
  rename(
    id    = 銘柄コード,
    name  = 会社名,
    ind   = 業種,
    fm    = 決算月,
    sales = 売上高,
    cogs  = 売上原価,
    sga   = 販管費
  )

# 上から6行目まで確認
head(df_1)

# データの書き出し
write_csv(df_1, "ch2_new.csv")

