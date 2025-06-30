library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)

# 讀取第一個工作表
data_amount <- read_excel("lng_amount.xlsx")
data_price <- read_excel("lng_price.xlsx")
# 查看前幾列來了解結構
head(data_amount)
head(data_price)


# 整理資料：轉為長格式
data_amount_long <- data_amount %>%
  pivot_longer(cols = `2012`:`2022`,
               names_to = "年份",
               values_to = "數量") %>%
  rename(合約類型 = `現貨類型`) %>%
  mutate(年份 = as.integer(年份))

# 🔍 取得所有可能組合
all_combination_amount <- expand_grid(
  年份 = 2012:2022,
  國家 = unique(data_amount_long$國家),
  合約類型 = unique(data_amount_long$合約類型)
)

# 補齊缺漏的組合，數量為 0
data_complete_amount <- all_combination_amount %>%
  left_join(data_amount_long, by = c("年份", "國家", "合約類型")) %>%
  mutate(數量 = replace_na(數量, 0))

# 加入總量與比例
data_prop_amount <- data_complete_amount %>%
  group_by(國家, 年份) %>%
  mutate(總量 = sum(數量),
         比例 = ifelse(總量 == 0, 0, 數量 / 總量)) %>%
  ungroup()

# 🔁 國家重新排序：讓「其他」排最後
data_prop_amount <- data_prop_amount %>%
  mutate(國家 = factor(國家, levels = c(setdiff(sort(unique(國家)), "其他"), "其他")))

data_prop_amount$國家 <- factor(data_prop_amount$國家,
                              levels = c("卡達", "澳洲", "美國", "巴布亞紐幾內亞",
                                         "俄羅斯", "其他","印尼", "馬來西亞"))
# 畫圖
p <- ggplot(data_prop_amount, aes(x = 年份, y = 比例, fill = 合約類型)) +
  geom_bar(stat = "identity", color = "white", size = 0) +  # ✅ 加邊框
  facet_wrap(~國家) +
  scale_x_continuous(breaks = 2012:2022)+
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("中長期約" = "#4F9D9D", "現貨" = "#AFAF61")) +  # 自訂顏色
  labs(title = "2012-2022 年各國天然氣進口合約比例",
       x = "年份", y = "比例") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#3C3C3C"),
        axis.text.y = element_text(color = "#3C3C3C"),
        axis.title = element_text(color = "#3C3C3C"),
        plot.title = element_text(color = "#3C3C3C", face = "bold"),
        strip.text = element_text(color = "#3C3C3C"),  # facet 標題
        legend.text = element_text(color = "#3C3C3C"),
        legend.title = element_text(color = "#3C3C3C"),
        panel.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.background = element_rect(fill = "#F0F0F0", color = NA))

# 互動圖表
ggplotly(p)
