###############################
## 0) 依赖
###############################
needed <- c("dplyr","mgcv","lme4","ggplot2","patchwork")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
library(dplyr); library(mgcv); library(lme4); library(ggplot2); library(patchwork)

###############################
## 1) 读取/指定两个数据框（已在环境中）
###############################
virgin_df   <- AllData_PreEDM_Virgin_RowColIDs_correcteddates
recycled_df <- AllData_PreEDM_Recycled_RowColIDs_correcteddates

###############################
## 2) 标准化列名 & 合并（recycled 追加在 virgin 下面）
###############################
standardize_df <- function(x, powder_label){
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"Nonconformity" %in% names(x) && "Nonconfirmity" %in% names(x)) {
    x$Nonconformity <- x$Nonconfirmity
  }
  x %>%
    mutate(
      PowderType = powder_label,
      PlateID    = toupper(as.character(PlateID)),
      Layout     = toupper(as.character(Layout))
    )
}
virgin_df   <- standardize_df(virgin_df,   "Virgin")
recycled_df <- standardize_df(recycled_df, "Recycled")

df <- bind_rows(virgin_df, recycled_df)

# *** 重要：彻底移除 build plate R ***
df <- df %>% filter(PlateID != "R")

###############################
## 3) 行列坐标 & is_bad（若没有则计算）
###############################
to_row_num <- function(x){
  xi <- suppressWarnings(as.integer(x))
  if (all(is.na(xi))) match(tolower(as.character(x)), letters[1:11]) else xi
}
to_col_num <- function(x) suppressWarnings(as.integer(x))

df <- df %>%
  mutate(
    row_num = if (!"row_num" %in% names(.)) to_row_num(RowID) else row_num,
    col_num = if (!"col_num" %in% names(.)) to_col_num(ColID)  else col_num
  )

if (!("is_bad" %in% names(df))) {
  in_range <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi
  to_bool <- function(x){
    if (is.logical(x)) return(x)
    x <- trimws(tolower(as.character(x)))
    out <- rep(NA, length(x))
    out[x %in% c("true","t","1","yes","y")]  <- TRUE
    out[x %in% c("false","f","0","no","n","")] <- FALSE
    out
  }
  nc_name <- intersect(c("Nonconformity","Nonconfirmity"), names(df))[1]
  
  ok_ID    <- in_range(df$B3_DATUM_B_LOC,    0.415, 0.435)
  ok_OD    <- in_range(df$B3_REF_OD,         0.445, 0.469)
  ok_floor <- in_range(df$C1_LOC_INSIDE_PLN, 0.049, 0.069)
  ok_lipH  <- in_range(df$C4_LOC_TOP_PLN,    0.261, 0.281)
  ok_t1    <- in_range(df$B3_THICK1_WALL,    0.010, 0.017)
  ok_t2    <- in_range(df$B3_THICK2_WALL,    0.010, 0.017)
  ok_t3    <- in_range(df$B3_THICK3_WALL,    0.010, 0.017)
  ok_t4    <- in_range(df$B3_THICK4_WALL,    0.010, 0.017)
  ok_all   <- ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4
  
  nc_true <- if (!is.na(nc_name)) to_bool(df[[nc_name]]) else rep(FALSE, nrow(df))
  df$is_bad <- (is.na(ok_all) | !ok_all) | (nc_true %in% TRUE)
}

###############################
## 4) 拟合用数据：类型清洗 + 构造指标
###############################
# TA 规则（按板号）：Virgin 的 B,C,D,F,G,H,J,K 有 TA；Recycled 去掉 R → L..Q
ta_virgin    <- c("B","C","D","F","G","H","J","K")
ta_recycled  <- c("L","M","N","O","P","Q")  # 不含 R
plates_with_TA <- unique(c(ta_virgin, ta_recycled))

df_fit <- df %>%
  mutate(
    PlateID    = factor(PlateID),
    PowderType = toupper(PowderType),
    Layout     = toupper(Layout),
    row_num    = suppressWarnings(as.numeric(row_num)),
    col_num    = suppressWarnings(as.numeric(col_num)),
    is_bad     = as.integer(is_bad),
    TA         = as.integer(PlateID %in% plates_with_TA),
    layout_6x6 = as.integer(grepl("^6X6", Layout)),
    powder_rec = as.integer(PowderType == "RECYCLED")
  ) %>%
  filter(!is.na(row_num), !is.na(col_num), !is.na(is_bad))

###############################
## 5) 避免秩亏：三水平 layout_ta（主分析）
###############################
df_fit <- df_fit %>%
  mutate(
    layout_ta = case_when(
      layout_6x6 == 1 & TA == 0 ~ "6x6_noTA",
      layout_6x6 == 1 & TA == 1 ~ "6x6_TA",
      layout_6x6 == 0            ~ "11x11_TA"   # 11x11 恒为 TA=1
    ),
    layout_ta = factor(layout_ta, levels = c("11x11_TA","6x6_noTA","6x6_TA"))
  )

# —— 模型渐进（拟合过程对比）：只固定效应 → +板RE → +二维位置平滑
m_fixed0 <- mgcv::gam(
  is_bad ~ layout_ta + powder_rec,
  family = binomial, data = df_fit, method = "REML"
)
m_re1 <- mgcv::gam(
  is_bad ~ layout_ta + powder_rec + s(PlateID, bs="re"),
  family = binomial, data = df_fit, method = "REML"
)

m_gamm2 <- mgcv::gam(
  is_bad ~ layout_ta + powder_rec +
    te(row_num, col_num, bs = c("cr","cr"), k = c(5,5)) +
    s(PlateID, bs = "re"),
  family = binomial, data = df_fit, method = "REML"
)
cat("\n=== m_gamm2 (layout_ta) ===\n"); print(summary(m_gamm2))

###############################
## 6) 统一色标 + 六张图（Virgin & Recycled）
###############################
present <- df_fit |>
  dplyr::count(layout_ta, powder_rec) |>
  dplyr::filter(n > 0)

get_preds <- function(layout_ta, powder_rec = 0, mod = m_gamm2, data_fit = df_fit){
  grid <- expand.grid(row_num = 1:11, col_num = 1:11)
  newd <- transform(grid,
                    layout_ta = factor(layout_ta, levels = levels(data_fit$layout_ta)),
                    powder_rec = as.integer(powder_rec))
  plate_levels <- levels(data_fit$PlateID)
  newd$PlateID <- factor(plate_levels[1], levels = plate_levels)
  as.numeric(predict(mod, newdata = newd, type = "response", exclude = "s(PlateID)"))
}

all_vals <- c()
for (lt in levels(df_fit$layout_ta)) {
  for (pr in c(0,1)) {
    if (nrow(dplyr::filter(present, layout_ta == lt, powder_rec == pr)) > 0) {
      all_vals <- c(all_vals, get_preds(lt, pr))
    }
  }
}
rng <- range(all_vals, na.rm = TRUE)

mk_heat_lta2 <- function(mod = m_gamm2, data_fit = df_fit,
                         layout_ta = c("11x11_TA","6x6_noTA","6x6_TA"),
                         powder_rec = 0, title = "", limits = NULL) {
  layout_ta <- match.arg(layout_ta)
  grid <- expand.grid(row_num = 1:11, col_num = 1:11)
  newd <- transform(grid,
                    layout_ta = factor(layout_ta, levels = levels(data_fit$layout_ta)),
                    powder_rec = as.integer(powder_rec))
  plate_levels <- levels(data_fit$PlateID)
  newd$PlateID <- factor(plate_levels[1], levels = plate_levels)
  newd$pred <- predict(mod, newdata = newd, type = "response", exclude = "s(PlateID)")
  
  p <- ggplot(newd, aes(col_num, row_num, fill = pred)) +
    geom_tile() +
    scale_y_reverse(breaks = 1:11, labels = letters[1:11]) +
    scale_x_continuous(breaks = 1:11) +
    coord_equal() +
    labs(title = title, x = "Column", y = "Row", fill = "Pred. scrap") +
    theme_minimal()
  if (!is.null(limits)) {
    p <- p + scale_fill_continuous(limits = limits, oob = scales::squish)
  }
  p
}

placeholder_plot <- function(title = "", limits = rng) {
  ggplot() +
    theme_void() +
    labs(title = title, fill = "Pred. scrap") +
    annotate("text", x = 0.5, y = 0.5, label = "Not in data", size = 5) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(fill = NA, colour = "black"))
}
make_or_placeholder <- function(layout_ta, powder_rec, title){
  if (nrow(dplyr::filter(present, layout_ta == layout_ta, powder_rec == powder_rec)) > 0) {
    mk_heat_lta2(layout_ta = layout_ta, powder_rec = powder_rec, title = title, limits = rng)
  } else {
    placeholder_plot(title, limits = rng)
  }
}

p_v_11  <- make_or_placeholder("11x11_TA", 0, "Virgin · 11×11 (TA)")
p_v_6n  <- make_or_placeholder("6x6_noTA", 0, "Virgin · 6×6 (No TA)")
p_v_6t  <- make_or_placeholder("6x6_TA",   0, "Virgin · 6×6 (TA)")

p_r_11  <- make_or_placeholder("11x11_TA", 1, "Recycled · 11×11 (TA)")
p_r_6n  <- make_or_placeholder("6x6_noTA", 1, "Recycled · 6×6 (No TA)")
p_r_6t  <- make_or_placeholder("6x6_TA",   1, "Recycled · 6×6 (TA)")

row1 <- (p_v_11 + theme(legend.position = "bottom")) |
  (p_v_6n + theme(legend.position = "bottom")) |
  (p_v_6t + theme(legend.position = "bottom"))
row2 <- (p_r_11 + theme(legend.position = "bottom")) |
  (p_r_6n + theme(legend.position = "bottom")) |
  (p_r_6t + theme(legend.position = "bottom"))

combined6 <- row1 / row2
combined6 <- combined6 +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Predicted Scrap Heatmaps — Virgin vs Recycled (Unified Scale)")

print(combined6)
# ggsave("heatmaps_virgin_recycled_unified.png", combined6, width = 18, height = 10, dpi = 220)

###############################
## ==== 拟合过程可视化（新增） ====
###############################

## (A) 模型渐进对比：只固定效应 → +板RE → +二维位置
s0 <- summary(m_fixed0); s1 <- summary(m_re1); s2 <- summary(m_gamm2)
fit_progress <- data.frame(
  model   = factor(c("Fixed only","Fixed + Plate RE","Fixed + Plate RE + 2D pos"),
                   levels = c("Fixed only","Fixed + Plate RE","Fixed + Plate RE + 2D pos")),
  dev_expl = 100*c(s0$dev.expl, s1$dev.expl, s2$dev.expl),
  adj_r2   = 100*c(s0$r.sq,     s1$r.sq,     s2$r.sq),
  AIC      = c(AIC(m_fixed0),   AIC(m_re1),  AIC(m_gamm2)),
  logLik   = c(as.numeric(logLik(m_fixed0)),
               as.numeric(logLik(m_re1)),
               as.numeric(logLik(m_gamm2)))
)

p_dev <- ggplot(fit_progress, aes(model, dev_expl)) +
  geom_col(fill = "grey60") +
  geom_text(aes(label = sprintf("%.1f%%", dev_expl)), vjust = -0.4, size = 3.5) +
  labs(title = "Deviance explained by model step", x = NULL, y = "Deviance explained (%)") +
  theme_minimal(base_size = 12)

p_aic <- ggplot(fit_progress, aes(model, AIC, group = 1)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "AIC across model steps", x = NULL, y = "AIC (lower is better)") +
  theme_minimal(base_size = 12)

## (B) 校准图：按预测分位分箱（含误差线）
df_fit$pred_full <- as.numeric(predict(m_gamm2, type = "response"))  # 含板RE
cal <- df_fit %>%
  mutate(bin = dplyr::ntile(pred_full, 10)) %>%
  group_by(bin) %>%
  summarise(
    pred = mean(pred_full),
    obs  = mean(is_bad),
    n    = n(),
    se   = sqrt(obs*(1-obs)/n),
    .groups = "drop"
  )
p_cal <- ggplot(cal, aes(pred, obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = pmax(0, obs - 1.96*se),
                    ymax = pmin(1, obs + 1.96*se)), width = 0.01) +
  geom_point(size = 2) +
  coord_equal(xlim = c(0, max(cal$pred)*1.05), ylim = c(0, max(cal$obs)*1.05)) +
  labs(title = "Calibration: observed",
       x = "Mean predicted", y = "Observed bad rate") +
  theme_minimal(base_size = 12)

## (C) 残差图：Pearson 残差 vs 预测
df_fit$resid_pear <- residuals(m_gamm2, type = "pearson")
p_res <- ggplot(df_fit, aes(pred_full, resid_pear)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Predicted probability", y = "Pearson residual") +
  theme_minimal(base_size = 12)

## (D) 固定效应“净效应”对比（把位置+板平均掉）
## (D) 固定效应“净效应”对比（把位置+板平均掉）——修正版
# 1) 构造 newdata，并补齐占位列
nd_fx <- expand.grid(layout_ta = levels(df_fit$layout_ta),
                     powder_rec = c(0,1),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)

# 占位值：取中位数位置 & 任意一个 PlateID level
nd_fx$row_num <- median(df_fit$row_num, na.rm = TRUE)
nd_fx$col_num <- median(df_fit$col_num, na.rm = TRUE)
nd_fx$PlateID <- factor(levels(df_fit$PlateID)[1], levels = levels(df_fit$PlateID))

# 2) 预测时排除位置平滑和板随机效应
nd_fx$pred <- as.numeric(predict(
  m_gamm2, newdata = nd_fx, type = "response",
  exclude = c("s(PlateID)", "te(row_num,col_num)")
))

# 3) 画图
nd_fx$powder_lab <- ifelse(nd_fx$powder_rec==0,"Virgin","Recycled")
p_fx <- ggplot(nd_fx, aes(layout_ta, pred, fill = powder_lab)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Fixed-effects only (averaging out plate & position)",
       x = "Layout · TA", y = "Predicted scrap (fixed effects)",
       fill = "Powder") +
  theme_minimal(base_size = 12)

print(p_fx)


## (E) mgcv 自带诊断面板（基础图形）
# 运行后会弹出4幅基础诊断图：QQ、残差 vs 线性预测、响应 vs 拟合、k-index 检查
# 如在脚本里运行，可手动查看
# par(mfrow=c(2,2)); mgcv::gam.check(m_gamm2); par(mfrow=c(1,1))

## 汇总排版显示（可保存）
diag_grid <- (p_dev | p_aic) / (p_cal | p_res) / p_fx
print(diag_grid)
# ggsave("model_diagnostics.png", diag_grid, width = 16, height = 14, dpi = 220)
