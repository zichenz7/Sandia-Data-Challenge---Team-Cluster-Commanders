## ====== 依赖 ======
rm(AllData_PreEDM_Virgin_RowColIDs_correcteddates)
needed <- c("ggplot2","dplyr","tidyr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
library(ggplot2); library(dplyr); library(tidyr)

## ====== 0) 从环境里自动拿数据框（也可手动 df <- 你的数据名）======
must_cols <- c("PlateID","RowID","ColID","Layout")
size_cols <- c("B3_DATUM_B_LOC","B3_REF_OD","C1_LOC_INSIDE_PLN","C4_LOC_TOP_PLN",
               "B3_THICK1_WALL","B3_THICK2_WALL","B3_THICK3_WALL","B3_THICK4_WALL")
objs <- mget(ls(envir=.GlobalEnv), envir=.GlobalEnv, ifnotfound = NA)
candidates <- Filter(function(x) is.data.frame(x) &&
                       all(must_cols %in% names(x)) &&
                       any(size_cols %in% names(x)), objs)
stopifnot(length(candidates) >= 1)
df <- candidates[[1]]
message("使用的数据框：", names(candidates)[1])

## ====== 1) 工具函数 ======
to_row_num <- function(x) {
  xi <- suppressWarnings(as.integer(x))
  if (all(is.na(xi))) match(tolower(as.character(x)), letters[1:11]) else xi
}
to_col_num <- function(x) suppressWarnings(as.integer(x))
to_bool <- function(x){
  if (is.logical(x)) return(x)
  x <- trimws(tolower(as.character(x)))
  out <- rep(NA, length(x))
  out[x %in% c("true","t","1","yes","y")]  <- TRUE
  out[x %in% c("false","f","0","no","n","")] <- FALSE
  out
}
in_range <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi

## ====== 2) 坏/好件判定（你的定义）======
nc_name <- c("Nonconformity","Nonconfirmity")[c("Nonconformity","Nonconfirmity") %in% names(df)][1]
stopifnot(!is.na(nc_name))
nc_true <- to_bool(df[[nc_name]])  # TRUE = 被标为不合格

ok_ID    <- in_range(df$B3_DATUM_B_LOC,    0.415, 0.435)
ok_OD    <- in_range(df$B3_REF_OD,         0.445, 0.469)
ok_floor <- in_range(df$C1_LOC_INSIDE_PLN, 0.049, 0.069)
ok_lipH  <- in_range(df$C4_LOC_TOP_PLN,    0.261, 0.281)
ok_t1    <- in_range(df$B3_THICK1_WALL,    0.010, 0.017)
ok_t2    <- in_range(df$B3_THICK2_WALL,    0.010, 0.017)
ok_t3    <- in_range(df$B3_THICK3_WALL,    0.010, 0.017)
ok_t4    <- in_range(df$B3_THICK4_WALL,    0.010, 0.017)
ok_all <- ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4

df <- df %>%
  mutate(
    PlateID = toupper(as.character(PlateID)),
    Layout  = toupper(Layout),
    row_num = to_row_num(RowID),
    col_num = to_col_num(ColID),
    is_bad  = (is.na(ok_all) | !ok_all) | (nc_true %in% TRUE),   # 坏件
    status  = ifelse(is_bad, "Bad", "Good")
  )

# 只保留有效坐标，避免 geom_point 警告
df_plot <- df %>%
  filter(!is.na(row_num), !is.na(col_num),
         row_num >= 1, row_num <= 11,
         col_num >= 1, col_num <= 11)

## ====== 3) TA = LMNOPQR ======
TA_plates <- c("L","M","N","O","P","Q","R")

## ====== 4) 计算 TA 坐标 ======
full11_num <- expand_grid(row_num = 1:11, col_num = 1:11)
baseline6_num <- df %>%
  filter(Layout == "6X6") %>%
  transmute(row_num = to_row_num(RowID), col_num = to_col_num(ColID)) %>%
  distinct() %>%
  filter(!is.na(row_num), !is.na(col_num),
         row_num %in% 1:11, col_num %in% 1:11)
if (nrow(baseline6_num) == 0) {
  baseline6_num <- expand_grid(row_num = c(1,3,5,7,9,11),
                               col_num = c(1,3,5,7,9,11))
}
plates_present <- intersect(TA_plates, unique(df_plot$PlateID))
ta_list <- lapply(plates_present, function(p) {
  present <- df_plot %>% filter(PlateID == p) %>%
    distinct(row_num, col_num, Layout)
  if (nrow(present) == 0) return(NULL)
  layout <- unique(na.omit(present$Layout)); layout <- if (length(layout)) layout[1] else "6X6TA"
  base_set <- if (layout == "6X6TA") baseline6_num else full11_num
  miss <- suppressMessages(anti_join(base_set, present[, c("row_num","col_num")],
                                     by = c("row_num","col_num")))
  if (nrow(miss) == 0) return(NULL)
  miss$PlateID <- p
  miss
})
if (length(Filter(Negate(is.null), ta_list)) > 0) {
  ta_points <- bind_rows(Filter(Negate(is.null), ta_list))
} else {
  ta_points <- data.frame(PlateID=character(), row_num=integer(), col_num=integer())
}

## ====== 5) 背景网格 ======
grid_bg <- expand_grid(PlateID = unique(df_plot$PlateID),
                       row_num = 1:11, col_num = 1:11)

## ====== 6) 计算每块板“红点百分比”，并加到 facet 标题 ======
plate_stats <- df_plot %>%
  group_by(PlateID) %>%
  summarise(total = n(),
            bad = sum(status == "Bad"),
            pct_bad = ifelse(total > 0, 100 * bad / total, NA_real_),
            .groups = "drop") %>%
  mutate(facet_lab = paste0(PlateID, " (", sprintf("%.1f", pct_bad), "%)"))
lab_map <- setNames(plate_stats$facet_lab, plate_stats$PlateID)

## ====== 7) 画图（坏=红；TA=黄；标题含红点百分比）======
p <- ggplot() +
  geom_point(data = grid_bg, aes(col_num, row_num),
             shape = 22, size = 2.2, stroke = 0,
             fill = "grey95", color = NA) +
  geom_point(data = df_plot, aes(col_num, row_num, fill = status),
             shape = 21, size = 3.2, color = "grey30", stroke = 0.2) +
  scale_fill_manual(values = c(Good = "grey70", Bad = "#d62728")) +
  geom_point(data = ta_points, aes(col_num, row_num),
             shape = 21, size = 5.5, fill = "#FFD54F", color = "grey20", alpha = 0.95) +
  facet_wrap(~ PlateID, ncol = 4,
             labeller = labeller(PlateID = as_labeller(lab_map))) +  # ★ 标题加百分比
  coord_equal() +
  scale_x_continuous(breaks = 1:11) +
  scale_y_reverse(breaks = 1:11, labels = letters[1:11], limits = c(11,1)) +
  labs(title = "Buildplates — Bad CUPs (Red) & TAs (Yellow)",
       x = "Column (1–11)", y = "Row (a–k)", fill = "CUP Status") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),  # ★ 每个小图黑色边框
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.6),  # ★ 标题条也加黑边
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom"
  )

print(p)
ggsave("buildplates_cups_TA.png", p, width = 12, height = 9, dpi = 220)

## （可选）只显示 L..R 这些板：
# keep <- c("L","M","N","O","P","Q","R")
# df_plot  <- df_plot  %>% filter(PlateID %in% keep)
# grid_bg  <- grid_bg  %>% filter(PlateID %in% keep)
# ta_points<- ta_points%>% filter(PlateID %in% keep)
# 重新 print(p) / ggsave(...)

