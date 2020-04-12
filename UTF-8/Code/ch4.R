# 作業環境のリセット
remove(list = ls(all = TRUE))


# 1つのグラフ----------------------------

# パッケージの読み込み
library(tidyverse)

# データの読み込み
df_0 <- read_csv("ch4.csv",
  locale = locale(encoding = "cp932")
)

# データの確認
df_0 %>%
  group_by(ind) %>%
  summarise_at(
    vars(roa, margin, turnover),
    median
  )

# ggolot2の設定
library(ggplot2)
margin_roa <- df_0 %>%
  ggplot(aes(x = margin, y = roa))

# 散布図
margin_roa
margin_roa + geom_point()
margin_roa + geom_point() + stat_smooth(method = lm)

# 代入による記入方法
margin_roa + geom_point() + stat_smooth(method = lm)

margin_roa_color_ind <- df_0 %>%
  ggplot(aes(x = margin, y = roa, color = ind))

margin_roa_color_ind_point <- margin_roa_color_ind + geom_point()

margin_roa_color_ind_point_lm <- margin_roa_color_ind_point + stat_smooth(method = lm)

margin_roa_color_ind_point_lm

# gをつかった記入方法
g <- df_0 %>%
  ggplot(aes(x = margin, y = roa, color = ind)) +
  geom_point() +
  stat_smooth(method = lm)
g

# ヒストグラム
margin_hist <- df_0 %>%
  ggplot(aes(x = margin, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.01)
margin_hist

# 箱ひげ図
margin_box <- df_0 %>%
  ggplot(aes(x = ind, y = margin)) +
  geom_boxplot()
margin_box


# 2つのグラフ----------------------------------------------------

# 変数作成
df_0 <- df_0 %>%
  mutate(
    wc  = inventory + receivable - payable,
    wcr = wc / asset,
    ccc = receivable / sales + inventory / cogs - payable / cogs
  )

# 運転資本のグラフ
gwc <- df_0 %>%
  ggplot(aes(x = wcr, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.05)

# CCCのグラフ
gccc <- df_0 %>%
  ggplot(aes(x = ccc, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.05)

# 2つのグラフを並べて表示
install.packages("gridExtra")
library(gridExtra)
grid.arrange(gwc, gccc, ncol = 1)


# 4つのグラフ----------------------------------------------------

# 変数作成
df_0 <- df_0 %>%
  mutate(
    roa       = ope_inc / asset,
    roa_nopat = (ope_inc - tax) / asset,
    roic_ope  = ope_inc / (debt + equity),
    roic      = (ope_inc - tax) / (debt + equity)
  )

# グラフ作成用の整然データを作成
df_1 <- df_0 %>%
  select(
    id,
    roa,
    roa_nopat,
    roic_ope,
    roic,
    ind
  ) %>%
  gather(
    measure,
    value,
    -id,
    -ind
  )

# 4つのグラフを2*2で表示(gをつかった記入方法)
g <- df_1 %>%
  ggplot(aes(x = value, fill = ind)) +
  geom_histogram(
    position = "identity",
    alpha = 0.8,
    binwidth = 0.01
  ) +
  facet_wrap(~measure, nrow = 2) +
  xlab("[Operating Profit] VS [Operating Profit minus Taxes]") +
  ylab("[ROIC] VS [ROA]")
g

