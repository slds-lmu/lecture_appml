library(ggplot2)
library(dplyr)
library(lubridate)

root <- rprojroot::find_root(rprojroot::is_git_root)

# ---------------------------------------------------------------------------
# 1. Load the Bike Sharing Demand dataset
#    CSV was exported from the exercise notebook (OpenML ID 44063)
# ---------------------------------------------------------------------------
bike <- read.csv(file.path(root, "data", "bike_sharing_demand.csv"))
bike$datetime <- as.POSIXct(bike$datetime, format = "%Y-%m-%d %H:%M:%S")

# ---------------------------------------------------------------------------
# 2. Plot: one representative week of data (hourly rentals)
#    June 4-10 2012: Mon-Sun, summer, clear weekday/weekend contrast
# ---------------------------------------------------------------------------
bike <- bike %>% filter(!is.na(datetime))

week_start <- as.POSIXct("2012-06-04")
one_week <- bike %>% filter(datetime >= week_start,
                            datetime < week_start + lubridate::days(7))

day_breaks <- seq(week_start, by = "1 day", length.out = 7)
day_labels <- format(day_breaks, "%a\n%b %d")

p_week <- ggplot(one_week, aes(x = datetime, y = count)) +
  geom_line(color = "#0033A0", linewidth = 0.5) +
  scale_x_datetime(breaks = day_breaks, labels = day_labels, expand = c(0.01, 0)) +
  labs(x = NULL, y = "Hourly rentals (count)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 10, 5, 5)
  )

ggsave("../figure/bike_one_week.pdf", p_week, width = 8, height = 3)
ggsave("../figure/bike_one_week.png", p_week, width = 8, height = 3, dpi = 200)

message("Saved bike_one_week.{pdf,png}")

# ---------------------------------------------------------------------------
# 3. Print first 5 rows for the slides table
# ---------------------------------------------------------------------------
sample_rows <- bike %>%
  head(5) %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M"))

message("\nFirst 5 rows for slides table:")
print(as.data.frame(sample_rows), row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Plot: mean hourly rentals - workday vs weekend
# ---------------------------------------------------------------------------
bike$hour <- hour(bike$datetime)
bike$day_type <- ifelse(bike$workingday == "True", "Working day", "Weekend / Holiday")

hourly_avg <- bike %>%
  group_by(hour, day_type) %>%
  summarise(mean_count = mean(count), .groups = "drop")

p_calendar <- ggplot(hourly_avg, aes(x = hour, y = mean_count, color = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("#D90012", "#0033A0")) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(x = "Hour of day", y = "Mean rentals", color = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("../figure/bike_hourly_by_daytype.pdf", p_calendar, width = 7, height = 4)
ggsave("../figure/bike_hourly_by_daytype.png", p_calendar, width = 7, height = 4, dpi = 200)

message("Saved bike_hourly_by_daytype.{pdf,png}")

# ---------------------------------------------------------------------------
# 5. Plot: count vs lag_168 scatter
# ---------------------------------------------------------------------------
set.seed(509)
bike_lag <- bike %>%
  arrange(datetime) %>%
  mutate(lag_168 = lag(count, 168)) %>%
  filter(!is.na(lag_168)) %>%
  sample_n(3000)

p_lag <- ggplot(bike_lag, aes(x = lag_168, y = count)) +
  geom_point(alpha = 0.15, size = 0.8, color = "#0033A0") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = expression(y[t-168] ~ "(same hour, one week ago)"),
       y = expression(y[t] ~ "(current count)")) +
  coord_equal(xlim = c(0, 1000), ylim = c(0, 1000)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave("../figure/bike_lag168_scatter.pdf", p_lag, width = 5, height = 5)
ggsave("../figure/bike_lag168_scatter.png", p_lag, width = 5, height = 5, dpi = 200)

message("Saved bike_lag168_scatter.{pdf,png}")

# ---------------------------------------------------------------------------
# 6. Plot: full 2-year series with linear trend overlay
# ---------------------------------------------------------------------------
# Daily mean for a cleaner plot (hourly is too noisy at this scale)
bike_daily <- bike %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_count = sum(count), .groups = "drop")

trend_fit <- lm(daily_count ~ as.numeric(date), data = bike_daily)

p_trend <- ggplot(bike_daily, aes(x = date, y = daily_count)) +
  geom_line(color = "#0033A0", linewidth = 0.3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "#D90012", linewidth = 1,
              linetype = "dashed") +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
  labs(x = NULL, y = "Daily total rentals") +
  annotate("text", x = as.Date("2012-08-01"), y = 1500,
           label = "linear trend", color = "#D90012", size = 4, fontface = "italic") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave("../figure/bike_trend.pdf", p_trend, width = 8, height = 3.5)
ggsave("../figure/bike_trend.png", p_trend, width = 8, height = 3.5, dpi = 200)

message("Saved bike_trend.{pdf,png}")
