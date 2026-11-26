# -*- coding: UTF-8 -*-
# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.

# Original data definitions and statistical procedures are retained.
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "ragg")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("缺少 R 包：", paste(missing_packages, collapse = ", "),
       "。请先在自己的 R 环境中安装这些包。", call. = FALSE)
}

input_file <- "E:/Codex/2026-10-14/q-w/outputs/社区热暴露缓解/社区热暴露缓解汇总.xlsx"
output_dir <- "E:/博士申请/第二篇论文/new/多日期的LST预测尺度/6shp/Box_points_EN_20261016"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(input_file)) stop("找不到源表：", input_file, call. = FALSE)

raw <- readxl::read_excel(input_file, sheet = "社区明细")
needed <- c("日期", "名义增量_pp", "name", "社区组别", "RCGS_AREA_m2",
            "c_full_C", "pop_人", "pop15_人", "pop65_人", "pop_fra_人",
            "he_all_C人每m2", "he_15_C人每m2", "he_65_C人每m2", "he_fra_C人每m2")
if (!all(needed %in% names(raw))) {
  stop("社区明细缺少列：", paste(setdiff(needed, names(raw)), collapse = ", "),
       call. = FALSE)
}

numeric_cols <- c("名义增量_pp", "RCGS_AREA_m2", "c_full_C",
                  "pop_人", "pop15_人", "pop65_人", "pop_fra_人")
if (!all(vapply(raw[numeric_cols], is.numeric, logical(1)))) {
  stop("强度、面积、降温值或人口列不是数值类型。", call. = FALSE)
}
if (anyNA(raw[needed]) || any(!is.finite(as.matrix(raw[numeric_cols])))) {
  stop("必要数据有缺失或非有限值，请先核对社区明细。", call. = FALSE)
}
if (any(raw$RCGS_AREA_m2 <= 0) || any(raw[numeric_cols[4:7]] < 0)) {
  stop("发现非正面积或负人口。", call. = FALSE)
}

day <- sprintf("%04d", as.integer(raw$日期))
dose <- as.integer(raw$名义增量_pp)
type <- as.character(raw$社区组别)
if (!setequal(unique(day), c("0603", "0619")) ||
    !setequal(unique(dose), c(2L, 5L, 10L)) ||
    !setequal(unique(type), c("Old", "New"))) {
  stop("日期、改造强度或 Old/New 类别与预期不符。", call. = FALSE)
}
if (anyDuplicated(data.frame(day, dose, name = raw$name))) {
  stop("同一日期、强度、社区 name 出现重复行。", call. = FALSE)
}

# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.
population_columns <- c(All = "pop_人", Child = "pop15_人",
                        Elder = "pop65_人", Frag = "pop_fra_人")
he_columns <- c(All = "he_all_C人每m2", Child = "he_15_C人每m2",
                Elder = "he_65_C人每m2", Frag = "he_fra_C人每m2")
for (group in names(population_columns)) {
  recomputed <- raw[[population_columns[[group]]]] /
    raw$RCGS_AREA_m2 * raw$c_full_C
  stored <- raw[[he_columns[[group]]]]
  if (is.numeric(stored)) {
    comparable <- is.finite(stored)
    if (any(comparable) &&
        max(abs(stored[comparable] - recomputed[comparable])) > 1e-9) {
      stop("表内 ", he_columns[[group]], " 与人数/面积×降温值不一致。",
           call. = FALSE)
    }
  }
  raw[[group]] <- recomputed
}

plot_data <- raw |>
  dplyr::transmute(
    day = day,
    dose = dose,
    name = as.character(name),
    type = factor(type, levels = c("Old", "New")),
    All = All, Child = Child, Elder = Elder, Frag = Frag
  ) |>
  tidyr::pivot_longer(cols = c("All", "Child", "Elder", "Frag"),
                      names_to = "group", values_to = "relief") |>
  dplyr::mutate(
    group = factor(group, levels = c("All", "Child", "Elder", "Frag")),
    panel = factor(
      paste(day, dose),
      levels = c("0603 2", "0603 5", "0603 10",
                 "0619 2", "0619 5", "0619 10"),
      labels = c("Non-heatwave  +2 pp", "Non-heatwave  +5 pp", "Non-heatwave  +10 pp",
                 "Heatwave  +2 pp", "Heatwave  +5 pp", "Heatwave  +10 pp")
    )
  )

counts <- plot_data |>
  dplyr::count(panel, group, type)
if (nrow(counts) != 48L || any(counts$n < 2L)) {
  stop("有情景-人群缺少足够的 Old/New 社区样本。", call. = FALSE)
}

# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.
wilcox_p_stable <- function(old, new) {
  # 与 wilcox.test(exact=FALSE, correct=TRUE) 相同的正态近似、
  # 并列秩方差修正和连续性校正；用尾概率避免极小 p 值被舍入成 0。
  n_old <- length(old)
  n_new <- length(new)
  pooled <- c(old, new)
  n <- length(pooled)
  W <- sum(rank(pooled)[seq_len(n_old)]) - n_old * (n_old + 1) / 2
  expected <- n_old * n_new / 2
  tie_sizes <- as.numeric(table(pooled))
  variance <- n_old * n_new / 12 *
    ((n + 1) - sum(tie_sizes^3 - tie_sizes) / (n * (n - 1)))
  if (!is.finite(variance) || variance <= 0) {
    stop("Wilcoxon 方差无法计算。", call. = FALSE)
  }
  z <- (W - expected - 0.5 * sign(W - expected)) / sqrt(variance)
  stable_p <- 2 * stats::pnorm(-abs(z))
  builtin_p <- stats::wilcox.test(old, new, alternative = "two.sided",
                                   paired = FALSE, exact = FALSE,
                                   correct = TRUE)$p.value
  if (builtin_p > 1e-10 &&
      abs(log(stable_p) - log(builtin_p)) > 1e-6) {
    stop(sprintf("稳定尾概率与 wilcox.test 不一致：W=%.6f，z=%.6f，稳定 p=%.12g，R p=%.12g。",
                 W, z, stable_p, builtin_p), call. = FALSE)
  }
  stable_p
}

test_one <- function(x) {
  old <- x$relief[x$type == "Old"]
  new <- x$relief[x$type == "New"]
  data.frame(
    panel = as.character(x$panel[1]),
    day = x$day[1], dose = x$dose[1],
    group = as.character(x$group[1]),
    n_old = length(old), n_new = length(new),
    mean_old = mean(old), mean_new = mean(new),
    median_old = median(old), median_new = median(new),
    p_raw = wilcox_p_stable(old, new)
  )
}
split_data <- split(plot_data, list(plot_data$panel, plot_data$group),
                    drop = TRUE)
tests <- do.call(rbind, lapply(split_data, test_one))
tests$p_holm_24 <- stats::p.adjust(tests$p_raw, method = "holm")
tests$significance <- ifelse(tests$p_holm_24 < 0.001, "***",
                      ifelse(tests$p_holm_24 < 0.01, "**",
                      ifelse(tests$p_holm_24 < 0.05, "*", "ns")))
tests <- tests[order(match(tests$panel, levels(plot_data$panel)),
                     match(tests$group, levels(plot_data$group))), ]
row.names(tests) <- NULL

# Original data definitions and statistical procedures are retained.
# Original data definitions and statistical procedures are retained.
sigma <- 0.001
panel_high <- max(plot_data$relief)
bracket_y <- sigma * sinh(asinh(panel_high / sigma) + 0.22)
tick_y <- sigma * sinh(asinh(panel_high / sigma) + 0.12)
label_y <- sigma * sinh(asinh(panel_high / sigma) + 0.34)
tests$x <- match(tests$group, levels(plot_data$group))
tests$x_left <- tests$x - 0.17
tests$x_right <- tests$x + 0.17
tests$bracket_y <- bracket_y
tests$tick_y <- tick_y
tests$label_y <- label_y
tests$panel <- factor(tests$panel, levels = levels(plot_data$panel))

font_family <- "Arial"
palette <- c(Old = "#269BD5", New = "#F29B21")
dodge <- ggplot2::position_dodge(width = 0.68)

p <- ggplot2::ggplot(plot_data,
                     ggplot2::aes(x = group, y = relief, colour = type,
                                  group = interaction(group, type))) +
  ggplot2::geom_hline(yintercept = 0, colour = "#909090",
                      linewidth = 0.30) +
  # Draw unfilled, group-coloured box outlines first.
  ggplot2::geom_boxplot(
    position = dodge, width = 0.56, fill = NA,
    linewidth = 0.45, linetype = "solid", outlier.shape = NA
  ) +
  # Overlay every community; jitter changes x only, never the measured relief.
  ggplot2::geom_point(
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.30, jitter.height = 0,
      dodge.width = 0.68, seed = 123
    ),
    alpha = 0.16, size = 0.42, shape = 16
  ) +
  ggplot2::geom_segment(data = tests,
                        ggplot2::aes(x = x_left, xend = x_right,
                                     y = bracket_y, yend = bracket_y),
                        inherit.aes = FALSE, linewidth = 0.34) +
  ggplot2::geom_segment(data = tests,
                        ggplot2::aes(x = x_left, xend = x_left,
                                     y = tick_y, yend = bracket_y),
                        inherit.aes = FALSE, linewidth = 0.34) +
  ggplot2::geom_segment(data = tests,
                        ggplot2::aes(x = x_right, xend = x_right,
                                     y = tick_y, yend = bracket_y),
                        inherit.aes = FALSE, linewidth = 0.34) +
  ggplot2::geom_text(data = tests,
                     ggplot2::aes(x = x, y = label_y, label = significance),
                     inherit.aes = FALSE, size = 3.0, family = font_family) +
  ggplot2::facet_wrap(~panel, ncol = 3, nrow = 2, drop = FALSE,
                       axes = "all", axis.labels = "all") +
  ggplot2::scale_colour_manual(values = palette, breaks = c("Old", "New")) +
  ggplot2::scale_x_discrete(labels = c(All = "All residents", Child = "Children", Elder = "Older adults", Frag = "Vulnerable")) +
  ggplot2::scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = sigma, base = 10),
    breaks = c(-0.01, -0.003, -0.001, 0, 0.001, 0.003,
               0.01, 0.03, 0.06),
    labels = function(v) sprintf("%.3f", v),
    expand = ggplot2::expansion(mult = c(0.035, 0.05))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Heat-exposure relief (°C·persons/m²)",
    colour = NULL,
    caption = paste(
      "Open boxes show medians and interquartile ranges; whiskers extend to 1.5 × IQR. Transparent points show all communities.",
      "Brackets compare Old and New communities: two-sided Wilcoxon rank-sum tests, Holm-adjusted across 24 comparisons.",
      "*** p < 0.001; ** p < 0.01; * p < 0.05; ns, not significant. The y-axis uses a signed pseudo-log scale.",
      sep = "\n"
    )
  ) +
  ggplot2::theme_classic(base_family = font_family, base_size = 9) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "#202020", linewidth = 0.4),
    axis.ticks = ggplot2::element_line(colour = "#202020", linewidth = 0.35),
    panel.spacing = grid::unit(3.0, "mm"),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(colour = "#111111", face = "bold", size = 10,
                                       margin = ggplot2::margin(5, 0, 5, 0)),
    axis.text.x = ggplot2::element_text(size = 8.5, colour = "#111111", angle = 30, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 7.7, colour = "#111111"),
    axis.title.y = ggplot2::element_text(size = 9.0, margin = ggplot2::margin(r = 6)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 9),
    legend.key.width = grid::unit(6, "mm"),
    plot.caption = ggplot2::element_text(hjust = 0, size = 7.3,
                                         lineheight = 1.25,
                                         margin = ggplot2::margin(t = 6)),
    plot.margin = ggplot2::margin(5, 7, 6, 6)
  )

png_file <- file.path(output_dir, "heat_exposure_relief_old_new_box_points.png")
pdf_file <- file.path(output_dir, "heat_exposure_relief_old_new_box_points.pdf")
csv_file <- file.path(output_dir, "heat_exposure_relief_old_new_tests_EN.csv")

# Preserve the existing 240 x 170 mm canvas; this is a style-only revision.
# Use preview mode: the installed environment does not meet all formal export locks.
source("C:/Users/Y/.codex/skills/paperplot-skills/scripts/paperplot_helpers.R")
render_spec <- pp_render_spec(
  n_panels = 6L, width_mm = 240, height_mm = 170, mode = "preview",
  panel_tags = FALSE, ocr = "off",
  text_pt = list(body = 9, panel_title = 10, axis_title = 9,
                 tick = 8, legend = 9, caption = 7.3, annotation = 8.5)
)
render_spec$dpi <- 320
attr(p, "pp_panel_evidence") <- list(
  observations = plot_data, tests = tests,
  all_community_points = TRUE, point_alpha = 0.16, box_fill = "none",
  unit = "degrees C * persons / m2",
  mean_points = FALSE, multiplicity = "Holm across 24 comparisons"
)
invisible(pp_save_all_with_qa_loop(
  p, output_stem = tools::file_path_sans_ext(png_file),
  formats = c("pdf", "png"), render_spec = render_spec,
  max_iterations = 0L, overwrite = TRUE
))
saveRDS(p, file.path(output_dir, "editable_plot_box_points.rds"))
utils::write.csv(tests[c("panel", "day", "dose", "group", "n_old", "n_new",
                         "mean_old", "mean_new", "median_old", "median_new",
                         "p_raw", "p_holm_24", "significance")],
                 csv_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Generated:\n", png_file, "\n", pdf_file, "\n", csv_file)




