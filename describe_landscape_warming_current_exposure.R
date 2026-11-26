# =============================================================================
# 周边景观、D_LST、现状热暴露：描述统计及R入门示例
# =============================================================================
# 在RStudio中打开本文件，点击 Source，或者在Console运行：
# source("E:/Codex/C-Drive-Workspaces/2026-08-01/jin-ji/describe_landscape_warming_current_exposure.R",
#        encoding = "UTF-8")
#
# 仅读取原始xls，不修改原文件，不读取任何土地改造或Bootstrap结果。
# 每次运行建立新时间戳子文件夹，不覆盖上次统计。
# 样本单位为社区；均值/中位数是社区之间的描述统计，不是居民级统计。
# =============================================================================

invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8")))
options(encoding = "UTF-8", scipen = 999)


# =============================================================================
# 1. 参数设置：通常只需要修改这一节
# =============================================================================

DATA_PATH <- paste0(
  "E:/博士申请/第二篇论文/data/wuhuanshiliang/",
  "SH_attributes_10000.xls"
)

OUTPUT_PARENT <- paste0(
  "E:/Codex/C-Drive-Workspaces/2026-08-01/jin-ji/",
  "descriptive_statistics_current_conditions"
)

# 正式三个模型使用的周边尺度：两个日期LST用90m，D_LST用300m。
BUFFER_DISTANCES <- c(90, 300)

# 若需扩展到全部敏感性尺度，将上一行改成：
# BUFFER_DISTANCES <- seq(30, 300, by = 30)

EXPECTED_N <- 949L
MINIMUM_AREA_M2 <- 10000
OLD_YEAR_CUTOFF <- 2000

# 原始覆盖比例为0--1。仅在统计展示时乘以100，不修改原始数据。
COVER_TO_PERCENT <- TRUE


# =============================================================================
# 2. 检查并加载R包：不自动安装，不改变现有R环境
# =============================================================================

required_packages <- c("readxl", "dplyr", "tidyr", "readr")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "请先安装缺少的包，再重新运行：install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(readr)


# =============================================================================
# 3. 建立变量字典：字段名、含义、单位及是否换算
# =============================================================================

landscape_names <- c(
  TCC = "External tree canopy coverage",
  GCI = "External grass coverage",
  BCI = "External building coverage",
  WCI = "External water coverage",
  ISF = "External impervious surface coverage",
  BAH = "External mean building height",
  BHSD = "External building-height standard deviation"
)

cover_codes <- c("TCC", "GCI", "BCI", "WCI", "ISF")

landscape_dictionary <- bind_rows(lapply(BUFFER_DISTANCES, function(distance) {
  codes <- names(landscape_names)
  is_cover <- codes %in% cover_codes

  tibble(
    Field = paste0(codes, "_b", distance, "m"),
    Category = "Surrounding landscape",
    Label = paste0(unname(landscape_names), " (", distance, " m)"),
    Buffer_m = distance,
    Weather = NA_character_,
    Resident_group = NA_character_,
    Unit = ifelse(is_cover, ifelse(COVER_TO_PERCENT, "%", "proportion"), "m"),
    Multiplier = ifelse(is_cover & COVER_TO_PERCENT, 100, 1),
    Digits = 2L
  )
}))

temperature_dictionary <- tibble(
  Field = c("lst0603", "lst0619", "D_LST"),
  Category = "Temperature",
  Label = c("Non-heatwave LST", "Heatwave LST", "Heatwave warming"),
  Buffer_m = NA_real_,
  Weather = c("Non-heatwave", "Heatwave", "Difference"),
  Resident_group = NA_character_,
  Unit = "deg C",
  Multiplier = 1,
  Digits = 2L
)

population_fields <- c("pop", "pop15", "pop65", "pop_fra")
resident_labels <- c(
  "All residents", "Children (0-14 years)",
  "Older adults (65+ years)", "Vulnerable population (0-14 and 65+)"
)

exposure_dictionary <- bind_rows(lapply(c("0603", "0619"), function(date_code) {
  tibble(
    Field = paste0("HE_", date_code, "_", population_fields, "_m2"),
    Category = "Current heat exposure",
    Label = paste0(resident_labels, ": ", ifelse(date_code == "0603", "non-heatwave", "heatwave")),
    Buffer_m = NA_real_,
    Weather = ifelse(date_code == "0603", "Non-heatwave", "Heatwave"),
    Resident_group = resident_labels,
    Unit = "deg C persons/m2",
    Multiplier = 1,
    Digits = 3L
  )
}))

context_dictionary <- tibble(
  Field = c("Shape_Area", "jungong", population_fields),
  Category = "Context",
  Label = c("Neighborhood area", "Construction year", resident_labels),
  Buffer_m = NA_real_,
  Weather = NA_character_,
  Resident_group = c(NA_character_, NA_character_, resident_labels),
  Unit = c("m2", "year", rep("persons (WorldPop estimates)", 4)),
  Multiplier = 1,
  Digits = c(2L, 1L, rep(2L, 4))
)

variable_dictionary <- bind_rows(
  landscape_dictionary,
  temperature_dictionary,
  exposure_dictionary,
  context_dictionary
)

if (anyDuplicated(variable_dictionary$Field)) {
  stop("变量字典有重复字段。请检查BUFFER_DISTANCES是否重复。")
}


# =============================================================================
# 4. 读取xls，并核查样本和数值字段
# =============================================================================

if (!file.exists(DATA_PATH)) {
  stop("找不到数据文件，请修改DATA_PATH：", DATA_PATH)
}

source_md5_before <- unname(tools::md5sum(DATA_PATH))

raw_data <- read_excel(
  path = DATA_PATH,
  sheet = 1,
  .name_repair = "check_unique"
)

required_fields <- unique(c("FID", variable_dictionary$Field))
missing_fields <- setdiff(required_fields, names(raw_data))

if (length(missing_fields) > 0L) {
  stop("原表缺少字段：", paste(missing_fields, collapse = ", "))
}

for (field in variable_dictionary$Field) {
  if (!is.numeric(raw_data[[field]])) {
    stop("字段不是数值型，请核对原表，不能直接当0处理：", field)
  }
}

dat <- raw_data |>
  filter(is.finite(Shape_Area), Shape_Area > MINIMUM_AREA_M2)

if (nrow(dat) != EXPECTED_N) {
  stop("样本数不是预期的", EXPECTED_N, "，实际为", nrow(dat), "。请先核对样本范围。")
}

if (anyNA(dat$FID) || anyDuplicated(dat$FID)) {
  stop("FID缺失或重复。不能通过删除同名社区解决重复问题。")
}

dat <- dat |>
  mutate(
    Construction_Group = case_when(
      is.na(jungong) ~ "Unknown",
      jungong <= OLD_YEAR_CUTOFF ~ "Old",
      jungong > OLD_YEAR_CUTOFF ~ "New"
    ),
    Construction_Group = factor(
      Construction_Group,
      levels = c("Old", "New", "Unknown")
    )
  )

# 不使用drop_na()删除整行。统计时逐变量报告有效样本数。
# D_LST为0或负数时仍保留；覆盖率为0同样是有效数值。
for (distance in BUFFER_DISTANCES) {
  for (code in cover_codes) {
    values <- dat[[paste0(code, "_b", distance, "m")]]
    if (any(values < -1e-8 | values > 1 + 1e-8, na.rm = TRUE)) {
      stop("覆盖比例不在0--1范围内，请检查：", code, "_b", distance, "m")
    }
  }
}

for (field in population_fields) {
  if (any(dat[[field]] < 0, na.rm = TRUE)) {
    stop("人口数量不能为负：", field)
  }
}


# =============================================================================
# 5. 核对现状指标公式：只检查，不覆盖已有字段
# =============================================================================

check_formula <- function(label, stored, calculated, tolerance) {
  valid <- is.finite(stored) & is.finite(calculated)
  error <- abs(stored[valid] - calculated[valid])

  tibble(
    Check = label,
    N_comparable = sum(valid),
    N_unavailable = length(valid) - sum(valid),
    Max_absolute_difference = if (length(error)) max(error) else NA_real_,
    Tolerance = tolerance,
    Passed = if (length(error)) all(error <= tolerance) else NA
  )
}

formula_audit <- bind_rows(
  check_formula("D_LST = lst0619 - lst0603", dat$D_LST,
                dat$lst0619 - dat$lst0603, 1e-6),
  check_formula("pop_fra = pop15 + pop65", dat$pop_fra,
                dat$pop15 + dat$pop65, 1e-5)
)

for (date_code in c("0603", "0619")) {
  for (population in population_fields) {
    exposure_field <- paste0("HE_", date_code, "_", population, "_m2")
    calculated <- dat[[paste0("lst", date_code)]] *
      dat[[population]] / dat$Shape_Area

    formula_audit <- bind_rows(
      formula_audit,
      check_formula(exposure_field, dat[[exposure_field]], calculated, 1e-8)
    )
  }
}

if (any(formula_audit$Passed %in% FALSE)) {
  print(formula_audit)
  stop("已有指标与公式不一致。请先核对，不自动覆盖原表。")
}


# =============================================================================
# 6. 描述统计函数：可用于任何一个数值变量
# =============================================================================

describe_numeric <- function(x) {
  valid <- x[is.finite(x)]
  n_valid <- length(valid)

  # type=7为R默认的样本分位数算法。
  q <- if (n_valid > 0L) {
    unname(quantile(valid, probs = c(0.25, 0.50, 0.75), type = 7))
  } else {
    rep(NA_real_, 3)
  }

  tibble(
    n_total = length(x),
    n_valid = n_valid,
    n_missing = sum(is.na(x)),
    n_nonfinite = sum(!is.na(x) & !is.finite(x)),
    Mean = if (n_valid > 0L) mean(valid) else NA_real_,
    SD = if (n_valid > 1L) sd(valid) else NA_real_,
    Minimum = if (n_valid > 0L) min(valid) else NA_real_,
    Q1 = q[1],
    Median = q[2],
    Q3 = q[3],
    IQR = q[3] - q[1],
    Maximum = if (n_valid > 0L) max(valid) else NA_real_
  )
}


# =============================================================================
# 7. 把多个变量整理成长表，再统计全体与新旧社区
# =============================================================================

analysis_long <- dat |>
  select(FID, Construction_Group, all_of(variable_dictionary$Field)) |>
  pivot_longer(
    cols = all_of(variable_dictionary$Field),
    names_to = "Field",
    values_to = "Raw_value"
  ) |>
  left_join(variable_dictionary, by = "Field") |>
  mutate(Value = Raw_value * Multiplier)

overall_stats <- analysis_long |>
  group_by(Field) |>
  group_modify(~ describe_numeric(.x$Value)) |>
  ungroup() |>
  mutate(Construction_Group = "All", .before = 1) |>
  left_join(variable_dictionary, by = "Field")

by_construction_stats <- analysis_long |>
  group_by(Construction_Group, Field, .drop = TRUE) |>
  group_modify(~ describe_numeric(.x$Value)) |>
  ungroup() |>
  mutate(Construction_Group = as.character(Construction_Group)) |>
  left_join(variable_dictionary, by = "Field")

all_statistics <- bind_rows(overall_stats, by_construction_stats) |>
  mutate(Field_order = match(Field, variable_dictionary$Field)) |>
  arrange(match(Construction_Group, c("All", "Old", "New", "Unknown")), Field_order) |>
  select(-Field_order) |>
  relocate(Construction_Group, Category, Field, Label, Unit,
           Buffer_m, Weather, Resident_group)

# 数值精度保留在all_statistics中；下面的文本列只用于论文排表。
paper_summary <- all_statistics |>
  mutate(
    Mean_SD = ifelse(
      n_valid > 0,
      sprintf("%.*f ± %.*f", Digits, Mean, Digits, SD),
      NA_character_
    ),
    Median_Q1_Q3 = ifelse(
      n_valid > 0,
      sprintf("%.*f [%.*f, %.*f]", Digits, Median, Digits, Q1, Digits, Q3),
      NA_character_
    )
  ) |>
  select(Category, Field, Label, Unit, Buffer_m, Weather, Resident_group,
         Construction_Group, n_valid, Mean_SD, Median_Q1_Q3)

paper_summary_wide <- paper_summary |>
  pivot_wider(
    names_from = Construction_Group,
    values_from = c(n_valid, Mean_SD, Median_Q1_Q3),
    names_glue = "{Construction_Group}_{.value}"
  )

sample_counts <- dat |>
  count(Construction_Group, .drop = FALSE, name = "N_communities") |>
  mutate(
    Percent = N_communities / nrow(dat) * 100,
    Construction_Group = as.character(Construction_Group)
  )

sample_counts <- bind_rows(
  tibble(Construction_Group = "All", N_communities = nrow(dat), Percent = 100),
  sample_counts
)


# =============================================================================
# 8. 保存为Excel可打开的UTF-8 BOM CSV，原始xls不变
# =============================================================================

run_name <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%OS6"))
OUTPUT_DIR <- file.path(OUTPUT_PARENT, gsub("[.]", "_", run_name))
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

write_result <- function(table, filename) {
  readr::write_excel_csv(table, file.path(OUTPUT_DIR, filename), na = "")
}

write_result(all_statistics, "01_all_descriptive_statistics.csv")
write_result(filter(all_statistics, Category == "Surrounding landscape"),
             "02_surrounding_landscape.csv")
write_result(filter(all_statistics, Category == "Temperature"),
             "03_LST_and_D_LST.csv")
write_result(filter(all_statistics, Category == "Current heat exposure"),
             "04_current_heat_exposure.csv")
write_result(paper_summary_wide, "05_paper_summary_All_Old_New.csv")
write_result(sample_counts, "06_community_counts.csv")
write_result(variable_dictionary, "07_variable_dictionary.csv")
write_result(formula_audit, "08_formula_consistency_checks.csv")

run_settings <- tibble(
  Setting = c("Source_file", "Source_MD5", "N_raw", "N_selected",
              "Minimum_area_exclusive_m2", "Old_construction_year_maximum",
              "Buffer_distances_m", "Coverage_display", "Quantile_algorithm",
              "SD_definition", "Heat_exposure_definition", "Mean_weighting",
              "Missing_value_policy", "Scenario", "R_version"),
  Value = c(DATA_PATH, source_md5_before, as.character(nrow(raw_data)),
            as.character(nrow(dat)), as.character(MINIMUM_AREA_M2),
            as.character(OLD_YEAR_CUTOFF), paste(BUFFER_DISTANCES, collapse = ","),
            ifelse(COVER_TO_PERCENT, "0-1 fractions multiplied by 100 for display", "0-1 fractions"),
            "R type=7", "Sample SD, denominator n-1", "Observed LST * population / area_m2",
            "Equal weight for each community; not population-weighted LST",
            "Variable-wise available values; counts reported; no whole-row deletion",
            "Current observed conditions only; no intervention predictions",
            R.version.string)
)

write_result(run_settings, "09_run_settings.csv")
capture.output(sessionInfo(), file = file.path(OUTPUT_DIR, "R_session_info.txt"))

source_md5_after <- unname(tools::md5sum(DATA_PATH))
if (!identical(source_md5_before, source_md5_after)) {
  stop("运行期间原始数据文件发生变化。请核对后重新统计。")
}


# =============================================================================
# 9. 在Console显示简要结果，其他对象可在Environment中继续查看
# =============================================================================

cat("\n社区分组数量：\n")
print(sample_counts)

cat("\n全体社区的LST与D_LST：\n")
print(
  paper_summary |>
    filter(Category == "Temperature", Construction_Group == "All") |>
    select(Label, n_valid, Mean_SD, Median_Q1_Q3)
)

cat("\n现状热暴露：全体社区的描述统计\n")
print(
  paper_summary |>
    filter(Category == "Current heat exposure", Construction_Group == "All") |>
    select(Weather, Resident_group, n_valid, Mean_SD, Median_Q1_Q3),
  width = Inf
)

cat("\n输出目录：\n", OUTPUT_DIR, "\n", sep = "")
cat("原始xls未修改；覆盖率单位为%时已乘100；SD和IQR不是置信区间。\n")


# =============================================================================
# 10. R入门练习：取消相应行前的 #，可逐行运行
# =============================================================================

# names(dat)                          # 所有列名
# nrow(dat)                           # 社区数量
# head(dat)                           # 前6行
# summary(dat$D_LST)                   # 一个变量的快速统计
# mean(dat$D_LST, na.rm = TRUE)         # 均值
# sd(dat$D_LST, na.rm = TRUE)           # 样本标准差
# median(dat$D_LST, na.rm = TRUE)       # 中位数
# quantile(dat$D_LST, c(.25, .5, .75), na.rm = TRUE)
# describe_numeric(dat$D_LST)          # 本脚本的一变量完整描述统计
# View(overall_stats)                  # RStudio中查看全体社区统计
# View(by_construction_stats)          # 查看新旧社区分组统计
# View(paper_summary_wide)             # 查看论文排表版本
