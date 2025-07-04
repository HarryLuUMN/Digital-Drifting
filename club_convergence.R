# club_convergence.R

# --- 0. 安装并加载必要的包 ——
# 如果尚未安装，请取消注释以下行并执行：
# install.packages("ConvergenceClubs")

library(dplyr)
library(tidyr)
library(readr)
library(ConvergenceClubs)
library(ggplot2)

# --- 1. 读取已清洗数据 ——
data <- read_csv("./data/cleaned_data/data_with_county.csv")

# --- 2. 构建 county-year 面板数据，并计算每县年平均数字指数 ——
county_panel <- data %>%
  filter(YEAR >= 2013, !is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop")

# --- 3. 转换为宽格式 data.frame，每行代表一个县，列名为年份值 ——
club_df <- county_panel %>%
  pivot_wider(names_from = YEAR, values_from = avg_index) %>%
  arrange(COUNTYFIP) %>%
  as.data.frame()  # 确保是 data.frame 而不是 tibble

# --- 4. 定义参数索引 ——
# 第一列是 County 名称，之后的列是各年份数据
dataCols <- 2:ncol(club_df)
refCol <- ncol(club_df)  # 使用最后一年作为排序参考

# --- 5. 运行 Phillips & Sul 的 club convergence 算法 ——
clubs <- findClubs(
  X = club_df,
  dataCols = dataCols,
  unit_names = 1,
  refCol = refCol,
  time_trim = 1/3,
  HACmethod = "FQSB",
  cstar = 0
)

# --- 6. 查看结果 ——
print(clubs)
summary(clubs)

# --- 7. 绘制 transition paths 图 ——
plot(clubs, avgTP = TRUE, legend = TRUE, main = "County-Level Club Convergence (Digital Index)")

# --- 8. （可选）将 county 的 club 分配导出为 CSV ——
assignments <- data.frame(
  COUNTYFIP = clubs@unit_names,
  club = clubs@groups
)
# write.csv(assignments, "./data/processed/county_club_assignments.csv", row.names = FALSE)

nrow(df)

