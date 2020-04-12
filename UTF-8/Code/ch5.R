# 作業環境のリセット
remove(list = ls(all = TRUE))


# t検定（一群）  ----------------------------------------------------------------

# パッケージの読み込み
library(tidyverse)

# データの読み込み
df_0 <- read_csv("ch5_1.csv",
  locale = locale(encoding = "cp932")
)

head(df_0)

# 平均値の算出
df_0 %>%
  summarize(mean(weight))

# 一群のt検定
t.test(df_0 %>% pull(weight), mu = 50)


# t検定（二群 対応なし） ------------------------------------------------------------
# 作業環境のリセット
remove(list = ls(all = TRUE))

# データの読み込み
df_0 <- read_csv("ch5_2.csv",
  locale = locale(encoding = "cp932")
)

head(df_0)

# 平均値の算出
df_0 %>%
  group_by(ind) %>%
  summarise(roa_2019_mean = mean(roa_2019))

# 二群のt検定（対応なし)
t.test(
  df_0 %>% filter(ind == "卸売業") %>% pull(roa_2019),
  df_0 %>% filter(ind == "小売業") %>% pull(roa_2019)
)

# # 二群のt検定（対応あり） -------------------------------------------------------------------------

# 平均値の算出
df_0 %>%
  summarise(
    roa_2019_mean = mean(roa_2019),
    roa_2016_mean = mean(roa_2016)
  )

# 二群のt検定（対応あり）
t.test(
  df_0 %>% pull(roa_2019),
  df_0 %>% pull(roa_2016),
  paired = TRUE
)

# 標本差の算出
df_0 <- df_0 %>%
  mutate(
    d_roa = roa_2019 - roa_2016
  )

# 一群のt検定
t.test(df_0 %>% pull(d_roa))


# 相関の検定 -------------------------------------------------------------------
# 散布図の描写
g <- df_0 %>% 
  ggplot(aes(x = roa_2016, y = roa_2019)) +
  geom_point()
g

# 相関係数の算出
cor(
  df_0 %>% pull(roa_2016),
  df_0 %>% pull(roa_2019)
)

# 相関の分析
cor.test(
  df_0 %>% pull(roa_2016),
  df_0 %>% pull(roa_2019)
)

