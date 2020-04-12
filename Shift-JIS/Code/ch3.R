# 作業環境のリセット
remove(list = ls(all = TRUE))


# データの結合・整理--------------------------------

# パッケージの読み込み
library(tidyverse)

# 財務データの読み込み
df_0 <- read_csv("ch3_findata.csv",
  locale = locale(encoding = "cp932")
)

# 変数名の変更
df_1 <- df_0 %>%
  select(
    銘柄コード,
    会社名,
    売上高,
    売上原価,
    販管費,
    総資産
  ) %>%
  rename(
    id    = 銘柄コード,
    name  = 会社名,
    sales = 売上高,
    cogs  = 売上原価,
    sga   = 販管費,
    asset = 総資産
  )

# 業界データの読み込み
df_ind_0 <- read_csv("ch3_inddata.csv",
  locale = locale(encoding = "cp932")
)

# 変数名の変更
df_ind_1 <- df_ind_0 %>%
  select(
    銘柄コード,
    業界
  ) %>%
  rename(
    id  = 銘柄コード,
    ind = 業界
  )

# データの結合
df_2 <- inner_join(df_1, df_ind_1,
  by = "id"
)

# 上から6行目まで確認
head(df_2)

# 業界ごとの度数の確認
df_2 %>%
  select(ind) %>%
  table()

# 業界名を英語に変更
df_2 <- df_2 %>%
  mutate(ind_eng = recode(ind,
    "ドラッグストア" = "DrugStore",
    "家電"           = "ElectronicsRetail"
  ))

head(df_2)


# 収益性指標-------------------------------

# 変数の作成
df_2 <- df_2 %>%
  mutate(
    ope_inc = sales - (cogs + sga),
    roa     = ope_inc / asset
  )

# 変数を指定してデータの確認
df_2 %>%
  select(
    name,
    ope_inc,
    roa,
    ind
  ) %>%
  head()

# ROAの比較
df_2 %>%
  group_by(ind) %>%
  summarise(median(roa))

# 利益率と回転率の比較
df_2 %>%
  mutate(
    margin   = ope_inc / sales,
    turnover = sales / asset
  ) %>%
  group_by(ind) %>%
  summarise_at(
    vars(margin, turnover),
    median
  )


# 整然データの復習------------------------------------
# 以下は付録では？（井上メモ）
# 整然データでないデータ形式
not_tidy <- tibble(
  firm = c("firmA", "firmB", "firmC", "firmD"),
  year1 = c("A1", "B1", "C1", "D1"),
  year2 = c("A2", "B2", "C2", "D2"),
  year3 = c("A3", "B3", "C3", "D3"),
  year4 = c("A4", "B4", "C4", "D4"),
)
not_tidy

# 整然データのデータ形式
tidy <- gather(not_tidy,
  key   = "year",
  value = "value",
  -firm
)

# 企業ごとに並べかえて確認
tidy %>% 
  arrange(firm) %>% 
  head()


# 時系列データ------------------------------------
# 作業環境のリセット
remove(list = ls(all = TRUE))

# 2期分の会計データを読み込む
df_2016_0 <- read_csv("ch3_findata_2016.csv",
  locale = locale(encoding = "cp932")
)
df_2017_0 <- read_csv("ch3_findata.csv",
  locale = locale(encoding = "cp932")
)

# 変数名の変更・年度の追加
# 2016年度
df_2016_1 <- df_2016_0 %>%
  select(
    銘柄コード,
    会社名,
    売上高,
    売上原価,
    販管費,
    総資産
  ) %>%
  rename(
    id    = 銘柄コード,
    name  = 会社名,
    sales = 売上高,
    cogs  = 売上原価,
    sga   = 販管費,
    asset = 総資産
  ) %>%
  mutate(fy = 2016)

# 2017年度
df_2017_1 <- df_2017_0 %>%
  select(
    銘柄コード,
    会社名,
    売上高,
    売上原価,
    販管費,
    総資産
  ) %>%
  rename(
    id    = 銘柄コード,
    name  = 会社名,
    sales = 売上高,
    cogs  = 売上原価,
    sga   = 販管費,
    asset = 総資産
  ) %>%
  mutate(fy = 2017)

# 時系列データの結合
df_0 <- bind_rows(
  df_2016_1,
  df_2017_1
) %>%
  arrange(id, fy)

head(df_0)

# 業界データの読み込みと変数名の変更
df_ind_0 <- read_csv("ch3_inddata.csv",
  locale = locale(encoding = "cp932")
) %>%
  select(
    銘柄コード,
    業界
  ) %>%
  rename(
    id  = 銘柄コード,
    ind = 業界
  )

# 業界名を英語に変更
df_ind_0 <- df_ind_0 %>%
  mutate(ind = recode(ind,
    "ドラッグストア" = "DrugStore",
    "家電"           = "ElectronicsRetail"
  ))

# 業界データの結合
df_1 <- inner_join(df_0, df_ind_0,
  by = "id"
)

head(df_1)

# 前期・次期データの作成
df_1 <- df_1 %>%
  group_by(id) %>%
  mutate(
    p1_asset = lag(asset, 1),
    f1_asset = lead(asset, 1)
  )

# 変数を指定して確認
df_1 %>% 
  select(
    id,
    name,
    fy,
    ind,
    asset,
    p1_asset,
    f1_asset
  ) %>% 
  head(10)

# 様々なパターンのROAを計算
df_1 %>%
  mutate(
    ope_inc  = sales - (cogs + sga),
    m_asset  = (asset + p1_asset) / 2,
    roa_1    = ope_inc / asset,
    roa_2    = ope_inc / m_asset,
    roa_3    = ope_inc / p1_asset
  ) %>%
  select(
    id,
    name,
    fy,
    roa_1,
    roa_2,
    roa_3
  ) %>%
  head(10)

