
library(tidyverse)
library(broom)
library(ggpubr)
library(scales)

dir.create("output", showWarnings = FALSE)
dir.create("output/plots", showWarnings = FALSE)

sink("output/Rscript.log", split = TRUE)

df <- read_csv("data/youtube-top-100-songs-2025.csv", show_col_types = FALSE)

d <- df %>%
  transmute(
    view_count = as.numeric(view_count),
    channel_follower_count = as.numeric(channel_follower_count)
  ) %>%
  filter(!is.na(view_count), !is.na(channel_follower_count),
         view_count > 0, channel_follower_count > 0)

cat("Rows after cleaning:", nrow(d), "\n")

summary(d)

d_log <- d %>% mutate(
  log_views = log10(view_count),
  log_followers = log10(channel_follower_count)
)

p_main <- ggplot(d_log, aes(x = log_followers, y = log_views)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship between Channel Followers and Video Views (log10 scale)",
    x = "Channel followers (log10)",
    y = "Video views (log10)"
  )
ggsave("output/plots/scatter_log_log.png", p_main, width = 7, height = 5, dpi = 300)

p_hist_views <- ggplot(d, aes(x = view_count)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = scales::label_number(big.mark = ","))
  labs(title="Histogram of Video Views", x="Views", y="Count")
ggsave("output/plots/hist_views.png", p_hist_views, width = 7, height = 5, dpi = 300)

p_hist_foll <- ggplot(d, aes(x = channel_follower_count)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = scales::label_number(big.mark = ","))
  labs(title="Histogram of Channel Followers", x="Followers", y="Count")
ggsave("output/plots/hist_followers.png", p_hist_foll, width = 7, height = 5, dpi = 300)

pearson_log <- cor.test(d_log$log_followers, d_log$log_views, method = "pearson")
cat("\nPearson (log10-log10):\n")
print(pearson_log)

spearman_raw <- cor.test(d$channel_follower_count, d$view_count, method = "spearman")
cat("\nSpearman (raw):\n")
print(spearman_raw)

fit <- lm(log_views ~ log_followers, data = d_log)
cat("\nLinear model (log10-log10):\n")
print(summary(fit))
cat("\nTidy model:\n")
print(broom::tidy(fit))

sink()
