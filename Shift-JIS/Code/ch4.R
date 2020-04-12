# ��Ɗ��̃��Z�b�g
remove(list = ls(all = TRUE))


# 1�̃O���t----------------------------

# �p�b�P�[�W�̓ǂݍ���
library(tidyverse)

# �f�[�^�̓ǂݍ���
df_0 <- read_csv("ch4.csv",
  locale = locale(encoding = "cp932")
)

# �f�[�^�̊m�F
df_0 %>%
  group_by(ind) %>%
  summarise_at(
    vars(roa, margin, turnover),
    median
  )

# ggolot2�̐ݒ�
library(ggplot2)
margin_roa <- df_0 %>%
  ggplot(aes(x = margin, y = roa))

# �U�z�}
margin_roa
margin_roa + geom_point()
margin_roa + geom_point() + stat_smooth(method = lm)

# ����ɂ��L�����@
margin_roa + geom_point() + stat_smooth(method = lm)

margin_roa_color_ind <- df_0 %>%
  ggplot(aes(x = margin, y = roa, color = ind))

margin_roa_color_ind_point <- margin_roa_color_ind + geom_point()

margin_roa_color_ind_point_lm <- margin_roa_color_ind_point + stat_smooth(method = lm)

margin_roa_color_ind_point_lm

# g���������L�����@
g <- df_0 %>%
  ggplot(aes(x = margin, y = roa, color = ind)) +
  geom_point() +
  stat_smooth(method = lm)
g

# �q�X�g�O����
margin_hist <- df_0 %>%
  ggplot(aes(x = margin, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.01)
margin_hist

# ���Ђ��}
margin_box <- df_0 %>%
  ggplot(aes(x = ind, y = margin)) +
  geom_boxplot()
margin_box


# 2�̃O���t----------------------------------------------------

# �ϐ��쐬
df_0 <- df_0 %>%
  mutate(
    wc  = inventory + receivable - payable,
    wcr = wc / asset,
    ccc = receivable / sales + inventory / cogs - payable / cogs
  )

# �^�]���{�̃O���t
gwc <- df_0 %>%
  ggplot(aes(x = wcr, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.05)

# CCC�̃O���t
gccc <- df_0 %>%
  ggplot(aes(x = ccc, fill = ind)) +
  geom_histogram(position = "dodge", binwidth = 0.05)

# 2�̃O���t����ׂĕ\��
install.packages("gridExtra")
library(gridExtra)
grid.arrange(gwc, gccc, ncol = 1)


# 4�̃O���t----------------------------------------------------

# �ϐ��쐬
df_0 <- df_0 %>%
  mutate(
    roa       = ope_inc / asset,
    roa_nopat = (ope_inc - tax) / asset,
    roic_ope  = ope_inc / (debt + equity),
    roic      = (ope_inc - tax) / (debt + equity)
  )

# �O���t�쐬�p�̐��R�f�[�^���쐬
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

# 4�̃O���t��2*2�ŕ\��(g���������L�����@)
g <- df_1 %>%
  ggplot(aes(x = value, fill = ind)) +
  geom_histogram(
    position = "identity",
    alpha = 0.8,
    binwidth = 0.01
  ) +
  # SHIFT-JIS�ɂ���ƃ`���_��?�ɂȂ��Ă��܂��i��チ���j
  facet_wrap(~measure, nrow = 2) +
  xlab("[Operating Profit] VS [Operating Profit minus Taxes]") +
  ylab("[ROIC] VS [ROA]")
g
