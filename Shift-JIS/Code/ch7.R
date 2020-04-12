# 作業環境のリセット
remove(list = ls(all = TRUE))


# ダミー変数 -------------------------------------------------------------------------

# ライブラリの読み込み
library(tidyverse)

# データの入力 (https://www.softbank.jp/corp/irinfo/financials/results/business_results/)
# リンク切れしている（井上メモ）
fy       <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
sales    <- c(3971, 4053, 4068, 5173, 8370, 11086, 25442, 27716, 26730, 27634, 30046, 32024, 32025)
earnings <- c(164, -239, -919, -548, -253, 622, 2710, 3242, 3591, 4658, 6291, 6752, 7994)

df_0 <- tibble(
  fy,
  sales,
  cost = sales - earnings
)

# SHIFT-JISにするとチルダが?になってしまう（井上メモ）
# 単回帰分析
lm_1 <- lm(cost ? sales, df_0)
summary(lm_1)

g <- ggplot(df_0, aes(x = sales, y = cost)) +
  geom_point() +
  geom_abline(
    slope     = coef(lmresult)[2],
    intercept = coef(lm_1)[1]
  )
g

# ダミー変数の作成
df_0 <- df_0 %>%
  mutate(
    fy_dummy = if_else(fy >= 2006, 1, 0)
  )

# 固定費のみダミー
lm_2 <- lm(cost ? sales + fy_dummy, df_0)
summary(lm_2)

cc <- tibble(
  sl         = c(coef(lm_2)[2], coef(lm_2)[2]),
  int        = c(coef(lm_2)[1], coef(lm_2)[1] + coef(lm_2)[3]),
  time_dummy = c("0", "1")
)

g <- df_0 %>%
  ggplot(
    aes(
      x      = sales,
      y      = cost,
      colour = as.character(fy_dummy)
    )
  ) +
  geom_point() +
  geom_abline(
    data = cc,
    aes(
      slope     = sl,
      intercept = int,
      colour    = time_dummy
    )
  )
g

# 固定費と変動費率の両方をダミー
lm_3 <- lm(cost ? sales + fy_dummy + sales:fy_dummy, df_0)
summary(lm_3)

cc <- tibble(
  sl     = c(coef(lm_3)[2], coef(lm_3)[2] + coef(lm_3)[4]),
  int    = c(coef(lm_3)[1], coef(lm_3)[1] + coef(lm_3)[3]),
  time_dummy = c("0", "1")
)

g <- df_0 %>%
  ggplot(
    aes(
      x = sales,
      y = cost,
      colour = as.character(fy_dummy)
    )
  ) +
  geom_point() +
  geom_abline(
    data = cc,
    aes(
      slope     = sl,
      intercept = int,
      colour    = time_dummy
    )
  )
g


# 重回帰分析 -------------------------------------------------------------------

# 作業環境のリセット
remove(list = ls(all = TRUE))

# 重回帰分析
df_0 <- read_csv("ch7_findata.csv",
  locale = locale(encoding = "cp932")
)

lm_simple <- lm(sga ? sales, df_0)
lm_multi  <- lm(sga ? sales + employees, df_0)

# 結果を出力
install.packages("stargazer")
library(stargazer)
stargazer(
  lm_simple,
  lm_multi,
  type = "text"
)

# 残差回帰
# 売上高を従業員数で推定
lm_sales <- lm(sales ? employees, df_0)
summary(lm_sales)

# 残差回帰と重回帰分析の比較
df_0 <- df_0 %>%
  mutate(
    residuals = sales - (coef(lm_sales)[1] + coef(lm_sales)[2] * employees)
  )

lm_residual <- lm(sga ? residuals, df_0)

stargazer(
  lm_simple,
  lm_multi,
  lm_residual,
  type = "text"
)


# コストの下方硬直性（運送業） -------------------------------------------------------------------------

# 作業環境のリセット
remove(list = ls(all = TRUE))

df_0 <- read_csv("ch7_trans.csv",
  locale = locale(encoding = "cp932")
)

# 変数の作成
df_0 <- df_0 %>%
  mutate(
    # 対数
    log_cost     = log(cost),
    log_sales    = log(sales),
    log_p1_cost  = log(p1_cost),
    log_p1_sales = log(p1_sales),
    # 差分
    diff_log_cost  = log_cost - log_p1_cost,
    diff_log_sales = log_sales - log_p1_sales,
    # ダミー変数
    dec_dummy = if_else(sales <= p1_sales, 1, 0)
  )

cvp <- lm(diff_log_cost ? diff_log_sales, df_0)
abj <- lm(diff_log_cost ? diff_log_sales + diff_log_sales:dec_dummy, df_0)


stargazer(cvp, abj, type = "text")

