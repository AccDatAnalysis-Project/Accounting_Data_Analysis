# 作業環境のリセット
remove(list = ls(all = TRUE))


# 高低点法 -------------------------------------------------------------------------

# 変動費率の計算
vr <- (23.2 - 16.0) / (700 - 300)
vr

# 最高の営業時の変動費
vc <- vr * 700
vc

# 固定費の計算
fc <- 23.2 - vc
fc


# CVP分析 -------------------------------------------------------------------

# 作業環境のリセット
remove(list = ls(all = TRUE))

# パッケージの読み込み
library(tidyverse)

# 小売業データの読み込み
df_retail <- read_csv("ch6_retail.csv",
  na = c("-"),
  locale = locale(encoding = "cp932")
) %>%
  select(
    売上高,
    販管費
  ) %>%
  rename(
    sales = 売上高,
    cost  = 販管費
  )

# 鉄鋼業データの読み込み
df_steel <- read_csv("ch6_steel.csv",
  na = c("-"),
  locale = locale(encoding = "cp932")
) %>%
  select(
    売上高,
    販管費
  ) %>%
  rename(
    sales = 売上高,
    cost  = 販管費
  )

# NAの除去
df_retail <- df_retail %>%
  drop_na()

# 小売業の散布図
g <- df_retail %>%
  ggplot(aes(x = sales, y = cost)) +
  geom_point() +
  stat_smooth(method = "lm")
g

# 鉄鋼業の散布図
g <- df_steel %>%
  ggplot(aes(x = sales, y = cost)) +
  geom_point() +
  stat_smooth(method = "lm")
g

# 上限10％の値を特定する
df_retail %>%
  summarise(
    sales_90 = quantile(sales, 0.90),
    cost_90  = quantile(cost, 0.90)
  )

df_steel %>%
  summarise(
    sales_90 = quantile(sales, 0.90),
    cost_90  = quantile(cost, 0.90)
  )

# 上限10％以上の観測値を除外する
df_retail <- df_retail %>%
  filter(
    sales < 318045,
    cost  < 99395
  )

df_steel <- df_steel %>%
  filter(
    sales < 257315,
    cost  < 23983
  )

# 小売業の散布図
g <- df_retail %>%
  ggplot(aes(x = sales, y = cost)) +
  geom_point() +
  stat_smooth(method = "lm")
g

# 鉄鋼業の散布図
g <- df_steel %>%
  ggplot(aes(x = sales, y = cost)) +
  geom_point() +
  stat_smooth(method = "lm")
g

# SHIFT-JISにするとチルダが?になってしまう（井上メモ）
# 小売業のCVP分析
lm_retail <- lm(cost ? sales, data = df_retail)
summary(lm_retail)

# 鉄鋼業のCVP分析
lm_steel <- lm(cost ? sales, data = df_steel)
summary(lm_steel)

