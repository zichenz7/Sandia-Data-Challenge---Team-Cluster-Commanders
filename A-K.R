df <- AllData_PreEDM_Virgin_RowColIDs_correcteddates

bad_df <- df %>%
  mutate(
    ok_ID    = dplyr::between(B3_DATUM_B_LOC,    0.415, 0.435),
    ok_OD    = dplyr::between(B3_REF_OD,         0.445, 0.469),
    ok_floor = dplyr::between(C1_LOC_INSIDE_PLN, 0.049, 0.069),
    ok_lipH  = dplyr::between(C4_LOC_TOP_PLN,    0.261, 0.281),
    ok_t1    = dplyr::between(B3_THICK1_WALL,    0.010, 0.017),
    ok_t2    = dplyr::between(B3_THICK2_WALL,    0.010, 0.017),
    ok_t3    = dplyr::between(B3_THICK3_WALL,    0.010, 0.017),
    ok_t4    = dplyr::between(B3_THICK4_WALL,    0.010, 0.017),
    ok_all   = ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4
  ) %>%
  filter(!ok_all | is.na(ok_all) | Nonconformity == "TRUE")



############# check col*col for pt 1############
pt1 <- virgin_volume_pt1

odd_idx <- seq(1, ncol(pt2)-1, by = 2)  # 1,3,5,...
prod_pt2 <- setNames(
  data.frame(lapply(odd_idx, function(n) {
    x <- suppressWarnings(as.numeric(pt2[[n]]))
    y <- suppressWarnings(as.numeric(pt2[[n + 1]]))
    x * y
  })),
  paste0(odd_idx)
)


colSums(prod_pt2, na.rm = TRUE)
############# check col*col for pt 2############
pt1 <- virgin_volume_pt1

odd_idx   <- seq(1, ncol(pt1) - 1, by = 2)
odd_names <- names(pt1)[odd_idx]

md_labels <- sub(".*_(?:19|20)\\d{2}_(\\d{2}_\\d{2})$", "\\1", odd_names, perl = TRUE)

no_hit <- md_labels == odd_names
if (any(no_hit)) {
  md_labels[no_hit] <- sub(".*_(\\d{2})_(\\d{2})$", "\\1_\\2", odd_names[no_hit], perl = TRUE)
}

prod_pt1 <- setNames(
  data.frame(lapply(odd_idx, function(n) {
    x <- suppressWarnings(as.numeric(pt1[[n    ]]))
    y <- suppressWarnings(as.numeric(pt1[[n + 1]]))
    x * y
  })),
  md_labels
)

sums_pt1 <- setNames(colSums(prod_pt1, na.rm = TRUE), md_labels)
sums_pt1


######## Recycled########
rc <- recycled_volume

odd_idx   <- seq(1, ncol(rc) - 1, by = 2)
odd_names <- names(rc)[odd_idx]
md_labels <- sub(".*_(\\d{2})(\\d{2})\\d{2}$", "\\1\\2", odd_names)

prod_rc1 <- setNames(
  as.data.frame(lapply(odd_idx, function(n) {
    x <- suppressWarnings(as.numeric(rc[[n    ]]))
    y <- suppressWarnings(as.numeric(rc[[n + 1]]))
    x * y
  })),
  md_labels
)
sums_rc <- setNames(colSums(prod_rc1, na.rm = TRUE), md_labels)

sums_rc

# ================== Buildplates 可视化（坏件=红、TA=黄） ==================

## ================= Packages =================
## ====== 依赖 ======
# rm(AllData_PreEDM_Recycled_RowColIDs_correcteddates)
# needed <- c("ggplot2","dplyr","tidyr")
# to_install <- setdiff(needed, rownames(installed.packages()))
# if (length(to_install)) install.packages(to_install)
# library(ggplot2); library(dplyr); library(tidyr)
# 
# ## ====== 0) 从环境里自动拿数据框（也可手动 df <- 你的数据名）======
# must_cols <- c("PlateID","RowID","ColID","Layout")
# size_cols <- c("B3_DATUM_B_LOC","B3_REF_OD","C1_LOC_INSIDE_PLN","C4_LOC_TOP_PLN",
#                "B3_THICK1_WALL","B3_THICK2_WALL","B3_THICK3_WALL","B3_THICK4_WALL")
# objs <- mget(ls(envir=.GlobalEnv), envir=.GlobalEnv, ifnotfound = NA)
# candidates <- Filter(function(x) is.data.frame(x) &&
#                        all(must_cols %in% names(x)) &&
#                        any(size_cols %in% names(x)), objs)
# stopifnot(length(candidates) >= 1)
# df <- candidates[[1]]
# message("使用的数据框：", names(candidates)[1])
# 
# ## ====== 1) 工具函数 ======
# to_row_num <- function(x) {
#   xi <- suppressWarnings(as.integer(x))
#   if (all(is.na(xi))) match(tolower(as.character(x)), letters[1:11]) else xi
# }
# to_col_num <- function(x) suppressWarnings(as.integer(x))
# to_bool <- function(x){
#   if (is.logical(x)) return(x)
#   x <- trimws(tolower(as.character(x)))
#   out <- rep(NA, length(x))
#   out[x %in% c("true","t","1","yes","y")]  <- TRUE
#   out[x %in% c("false","f","0","no","n","")] <- FALSE
#   out
# }
# in_range <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi
# 
# ## ====== 2) 坏/好件判定（你的定义）======
# nc_name <- c("Nonconformity","Nonconfirmity")[c("Nonconformity","Nonconfirmity") %in% names(df)][1]
# stopifnot(!is.na(nc_name))
# nc_true <- to_bool(df[[nc_name]])  # TRUE = 被标为不合格
# 
# ok_ID    <- in_range(df$B3_DATUM_B_LOC,    0.415, 0.435)
# ok_OD    <- in_range(df$B3_REF_OD,         0.445, 0.469)
# ok_floor <- in_range(df$C1_LOC_INSIDE_PLN, 0.049, 0.069)
# ok_lipH  <- in_range(df$C4_LOC_TOP_PLN,    0.261, 0.281)
# ok_t1    <- in_range(df$B3_THICK1_WALL,    0.010, 0.017)
# ok_t2    <- in_range(df$B3_THICK2_WALL,    0.010, 0.017)
# ok_t3    <- in_range(df$B3_THICK3_WALL,    0.010, 0.017)
# ok_t4    <- in_range(df$B3_THICK4_WALL,    0.010, 0.017)
# ok_all <- ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4
# 
# df <- df %>%
#   mutate(
#     PlateID = toupper(as.character(PlateID)),
#     Layout  = toupper(Layout),
#     row_num = to_row_num(RowID),
#     col_num = to_col_num(ColID),
#     is_bad  = (is.na(ok_all) | !ok_all) | (nc_true %in% TRUE),   # 坏件
#     status  = ifelse(is_bad, "Bad", "Good")
#   )
# 
# # 只保留有效坐标
# df_plot <- df %>%
#   filter(!is.na(row_num), !is.na(col_num),
#          row_num >= 1, row_num <= 11,
#          col_num >= 1, col_num <= 11)
# 
# ## ====== 3) 哪些板有 TA（保持你原来的清单）======
# TA_plates <- c("B","C","D","F","G","H","J","K")
# 
# ## ====== 4) 计算 TA 坐标（修复 6×6TA 的判定）======
# # 完整 11×11 网格（用于 11×11TA 的差集）
# full11_num <- expand_grid(row_num = 1:11, col_num = 1:11)
# 
# # 6×6TA 的固定 9 个 TA 位置：位于偶数行/列（处在 6×6 杯位之间）
# ta_pattern_6x6 <- expand_grid(row_num = c(2,6,10), col_num = c(2,6,10))
# 
# plates_present <- intersect(TA_plates, unique(df_plot$PlateID))
# 
# ta_points <- bind_rows(lapply(plates_present, function(p) {
#   lay <- df_plot %>% filter(PlateID == p) %>%
#     pull(Layout) %>% unique() %>% na.omit() %>% toupper()
#   
#   # 判定该板属于哪类 TA（优先根据 Layout 字样）
#   is_11x11 <- any(grepl("11", lay))   # 11X11TA
#   is_6x6   <- any(grepl("6X6", lay))  # 6X6TA
#   
#   if (is_11x11) {
#     present <- df_plot %>% filter(PlateID == p) %>% distinct(row_num, col_num)
#     miss <- suppressMessages(anti_join(full11_num, present, by = c("row_num","col_num")))
#     if (nrow(miss) == 0) return(NULL)
#     miss$PlateID <- p
#     return(miss)
#   } else if (is_6x6) {
#     cbind(PlateID = p, ta_pattern_6x6)
#   } else {
#     # 兜底：如果 Layout 非预期，默认按 11×11TA 处理
#     present <- df_plot %>% filter(PlateID == p) %>% distinct(row_num, col_num)
#     miss <- suppressMessages(anti_join(full11_num, present, by = c("row_num","col_num")))
#     if (nrow(miss) == 0) return(NULL)
#     miss$PlateID <- p
#     miss
#   }
# }))
# 
# if (is.null(ta_points)) {
#   ta_points <- data.frame(PlateID=character(), row_num=integer(), col_num=integer())
# }
# 
# ## ====== 5) 背景网格 ======
# grid_bg <- expand_grid(PlateID = unique(df_plot$PlateID),
#                        row_num = 1:11, col_num = 1:11)
# 
# ## ====== 6) 每块板“红点百分比”，加到 facet 标题 ======
# plate_stats <- df_plot %>%
#   group_by(PlateID) %>%
#   summarise(total = n(),
#             bad = sum(status == "Bad"),
#             pct_bad = ifelse(total > 0, 100 * bad / total, NA_real_),
#             .groups = "drop") %>%
#   mutate(facet_lab = paste0(PlateID, " (", sprintf("%.1f", pct_bad), "%)"))
# lab_map <- setNames(plate_stats$facet_lab, plate_stats$PlateID)
# 
# ## ====== 7) 画图（坏=红；TA=黄；标题含红点百分比；黑色边框）======
# p <- ggplot() +
#   geom_point(data = grid_bg, aes(col_num, row_num),
#              shape = 22, size = 2.2, stroke = 0,
#              fill = "grey95", color = NA) +
#   geom_point(data = df_plot, aes(col_num, row_num, fill = status),
#              shape = 21, size = 3.2, color = "grey30", stroke = 0.2) +
#   scale_fill_manual(values = c(Good = "grey70", Bad = "#d62728")) +
#   geom_point(data = ta_points, aes(col_num, row_num),
#              shape = 21, size = 5.5, fill = "#FFD54F", color = "grey20", alpha = 0.95) +
#   facet_wrap(~ PlateID, ncol = 4,
#              labeller = labeller(PlateID = as_labeller(lab_map))) +
#   coord_equal() +
#   scale_x_continuous(breaks = 1:11) +
#   scale_y_reverse(breaks = 1:11, labels = letters[1:11], limits = c(11,1)) +
#   labs(title = "Buildplates — Bad CUPs (Red) & TAs (Yellow)",
#        x = "Column (1–11)", y = "Row (a–k)", fill = "CUP Status") +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid       = element_blank(),
#     panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
#     strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.6),
#     strip.text       = element_text(face = "bold"),
#     legend.position  = "bottom"
#   )
# 
# print(p)
# ggsave("buildplates_cups_TA.png", p, width = 12, height = 9, dpi = 220)
