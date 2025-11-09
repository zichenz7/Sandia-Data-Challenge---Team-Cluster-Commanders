install.packages("tidyverse")
library(dplyr)
library(readr)
install.packages("ggplot2")   
library(ggplot2)
library(scales)             
df <- AllData_PreEDM_Recycled_RowColIDs

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
df <- df %>%
  mutate(
    ok_ID    = between(B3_DATUM_B_LOC,    0.415, 0.435),  
    ok_OD    = between(B3_REF_OD,         0.445, 0.469),
    ok_floor = between(C1_LOC_INSIDE_PLN, 0.049, 0.069), 
    ok_lipH  = between(C4_LOC_TOP_PLN,    0.261, 0.281), 
    ok_t1    = between(B3_THICK1_WALL,    0.010, 0.017), 
    ok_t2    = between(B3_THICK2_WALL,    0.010, 0.017),
    ok_t3    = between(B3_THICK3_WALL,    0.010, 0.017),
    ok_t4    = between(B3_THICK4_WALL,    0.010, 0.017),
    ok_all   = ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4,
    scrap    = (!ok_all | Nonconformity == "TRUE")   # ✅ 这里定义 scrap
  )
mean(df$scrap)
scrap_by_powder <- df %>%
  group_by(Powder) %>%
  summarise(n = n(),
            scrap_rate = mean(scrap)) %>%
  arrange(scrap_rate)

scrap_by_powder

ggplot(scrap_by_powder, aes(Powder, scrap_rate, fill = Powder)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

library(dplyr)
library(ggplot2)
library(scales)

df_virgin <- AllData_PreEDM_Virgin_RowColIDs

df_virgin <- df_virgin %>%
  mutate(
    ok_ID    = between(B3_DATUM_B_LOC,    0.415, 0.435),
    ok_OD    = between(B3_REF_OD,         0.445, 0.469),
    ok_floor = between(C1_LOC_INSIDE_PLN, 0.049, 0.069),
    ok_lipH  = between(C4_LOC_TOP_PLN,    0.261, 0.281),
    ok_t1    = between(B3_THICK1_WALL,    0.010, 0.017),
    ok_t2    = between(B3_THICK2_WALL,    0.010, 0.017),
    ok_t3    = between(B3_THICK3_WALL,    0.010, 0.017),
    ok_t4    = between(B3_THICK4_WALL,    0.010, 0.017),
    ok_all   = ok_ID & ok_OD & ok_floor & ok_lipH & ok_t1 & ok_t2 & ok_t3 & ok_t4,
    scrap    = (!ok_all | Nonconformity == "TRUE")
  )
mean(df_virgin$scrap)

powder_compare <- bind_rows(
  df %>% mutate(PowderType = "Recycled"),
  df_virgin %>% mutate(PowderType = "Virgin")
) %>%
  group_by(PowderType) %>%
  summarise(scrap_rate = mean(scrap),
            n = n()) %>%
  arrange(scrap_rate)

powder_compare

ggplot(powder_compare, aes(x = PowderType, y = scrap_rate, fill = PowderType)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Scrap Rate Comparison: Virgin vs Recycled Powder",
       x = "Powder Type",
       y = "Scrap Rate") +
  theme_minimal() +
  theme(legend.position = "none")

##################################################
# 4. Bootstrapping function (放这里，一次定义，全程复用)
##################################################
library(tidyverse)
bootstrap_rate <- function(data, group_var, n_boot = 5000) {
  data <- data %>% select({{ group_var }}, scrap)
  data %>% 
    group_by({{ group_var }}) %>%
    do({
      boot <- replicate(n_boot, mean(sample(.$scrap, replace = TRUE)))
      tibble(
        group = unique(.[[deparse(substitute(group_var))]]),
        mean = mean(.$scrap),
        ci_low = quantile(boot, 0.025),
        ci_high = quantile(boot, 0.975)
      )
    })
}

##################################################
# 5. Run bootstrapping for Powder type
##################################################
combined <- bind_rows(
  df %>% mutate(Powder="Recycled"),
  df_virgin %>% mutate(Powder="Virgin")
)

bootstrap_rate(combined, Powder)
colnames(df)
##################################################
# 6. Two-proportion significance test
##################################################
prop.test(
  x = c(sum(df_virgin$scrap), sum(df$scrap)),
  n = c(nrow(df_virgin), nrow(df)),
  alternative = "two.sided",
  correct = FALSE
)
library(tidyverse)
combined <- bind_rows(
  df %>% mutate(Powder="Recycled"),
  df_virgin %>% mutate(Powder="Virgin")
)
bootstrap_rate <- function(data, n_boot = 5000) {
  replicate(n_boot, mean(sample(data$scrap, replace = TRUE))) %>%
    quantile(c(0.025, 0.975))
}
boot_virgin <- bootstrap_rate(df_virgin)
boot_recycled <- bootstrap_rate(df)
boot_virgin
boot_recycled
