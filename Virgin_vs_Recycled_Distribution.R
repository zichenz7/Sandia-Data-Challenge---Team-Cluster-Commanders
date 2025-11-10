library(tidyverse)

recycled <- read_csv("recycled_volume.csv")
virgin1  <- read_csv("virgin_volume_pt1.csv")
virgin2  <- read_csv("virgin_volume_pt2.csv")

recycled_clean <- recycled %>%
  rename(Size = 1) %>%   # 第一列改名为 Size
  slice(-1) %>%          # 移除 header 重复行 ("Size Classes")
  mutate(across(everything(), as.numeric)) %>%  # 转成数值
  pivot_longer(-Size, names_to = "Sample", values_to = "VolumeDensity") %>%
  drop_na()

head(recycled_clean)
virgin_clean <- bind_rows(virgin1, virgin2) %>%
  rename(Size = 1) %>% 
  slice(-1) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(-Size, names_to = "Sample", values_to = "VolumeDensity") %>%
  drop_na()

head(virgin_clean)
summary(recycled_clean)
summary(virgin_clean)
ggplot() +
  geom_line(data = recycled_clean %>% group_by(Size) %>% summarise(mean_VD = mean(VolumeDensity)),
            aes(Size, mean_VD, color = "Recycled"), linewidth = 1.2) +
  geom_line(data = virgin_clean %>% group_by(Size) %>% summarise(mean_VD = mean(VolumeDensity)),
            aes(Size, mean_VD, color = "Virgin"), linewidth = 1.2) +
  scale_color_manual(values = c("Virgin" = "#0077FF", "Recycled" = "#FF9900")) +
  labs(title = "Particle Size Distribution: Virgin vs. Recycled Powder",
       x = "Particle Size (µm)", y = "Volume Density (%)", color = "Powder Type") +
  theme_minimal(base_size = 14)


library(dplyr)
library(tidyr)
library(ggplot2)



recycled_pdf <- recycled_clean %>%
  group_by(Sample) %>%
  mutate(pdf = VolumeDensity / sum(VolumeDensity, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Size) %>%
  summarise(pdf = mean(pdf), .groups="drop") %>%
  mutate(Type = "Recycled")

virgin_pdf <- virgin_clean %>%
  group_by(Sample) %>%
  mutate(pdf = VolumeDensity / sum(VolumeDensity, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Size) %>%
  summarise(pdf = mean(pdf), .groups="drop") %>%
  mutate(Type = "Virgin")

plot_df <- bind_rows(recycled_pdf, virgin_pdf)

ggplot(plot_df, aes(x = Size, y = pdf, color = Type)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("Virgin" = "#0077FF", "Recycled" = "#FF9500")) +
  scale_x_log10() +
  labs(
    title = "Particle Size Distribution (PDF): Virgin vs Recycled Powder",
    x = "Particle Size (µm)",
    y = "Relative Volume Fraction (PDF)",
    color = "Powder Type"
  ) +
  theme_minimal(base_size = 16)
