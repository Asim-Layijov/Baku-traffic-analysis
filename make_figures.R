# =====================================================================
# make_figures.R: пересоздаёт три PNG в ТОМ ЖЕ виде, что и раньше,
# но по исправленным данным (правильное время, без аномальных дней).
# Запускать после analysis.R. Файлы перезапишут старые в папке проекта.
# =====================================================================
library(tidyverse)
library(lubridate)
library(scales)

Sys.setlocale("LC_TIME", "C")   # английские названия месяцев и дней

daily        <- readRDS("data/daily.rds")
worst        <- readRDS("data/worst.rds")
excl         <- readRDS("data/excluded_days.rds")
rain_profile <- readRDS("data/rain_profile.rds")

# ---------- 1. worst_days_top10.png ----------
reason_levels <- sort(unique(daily$reason))
pal <- setNames(hcl.colors(length(reason_levels), "Dark 3"), reason_levels)
if ("No known cause in data" %in% names(pal)) pal["No known cause in data"] <- "#8c8c8c"

worst_plot <- worst %>%
  mutate(label = paste0(day(date), " ", month.abb[month(date)], " ", year(date),
                        " (", wday(date, label = TRUE, week_start = 1), ")",
                        ifelse(date %in% excl$date, " *", "")),
         label = fct_reorder(label, excess_pct))

g1 <- ggplot(worst_plot, aes(excess_pct, label, colour = reason)) +
  geom_segment(aes(x = 0, xend = excess_pct, yend = label), linewidth = 1.2) +
  geom_point(size = 4.5) +
  geom_text(aes(label = sprintf("+%.0f%%", excess_pct)), hjust = -0.4, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = pal,
                      guide = guide_legend(nrow = 3, byrow = TRUE, title.position = "top",
                                           override.aes = list(size = 3.5))) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "The 10 worst traffic days in Baku",
       subtitle = "Extra delay vs a typical same-weekday of that month",
       x = NULL, y = NULL, colour = "Likely factor") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom", legend.justification = "left",
        plot.margin = margin(10, 20, 10, 10),
        panel.grid.major.y = element_blank())
print(g1)
ggsave("worst_days_top10.png", g1, width = 11, height = 7.5, dpi = 200, bg = "white")

# ---------- 2. worst_days_calendar.png ----------
all_days <- tibble(date = seq(min(c(daily$date, excl$date)), max(c(daily$date, excl$date)), by = "day")) %>%
  left_join(daily %>% select(date, excess_pct), by = "date") %>%
  mutate(first_wd = wday(floor_date(date, "month"), week_start = 1),
         wk     = ((day(date) - 1) + (first_wd - 1)) %/% 7 + 1,
         wd     = wday(date, week_start = 1),
         ym_chr = paste(month.abb[month(date)], year(date)),
         ym_lab = factor(ym_chr, levels = unique(ym_chr)))
worst_cal <- all_days %>% semi_join(worst, by = "date")

g2 <- ggplot(all_days, aes(wd, wk, fill = excess_pct)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_tile(data = worst_cal, fill = NA, colour = "black", linewidth = 1) +
  geom_text(aes(label = paste0(day(date), ifelse(date %in% excl$date, "*", ""))), size = 2.8, colour = "grey20") +
  facet_wrap(~ ym_lab, ncol = 6) +
  scale_x_continuous(breaks = 1:7, labels = c("M", "T", "W", "T", "F", "S", "S"), position = "top") +
  scale_y_reverse() +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-50, 100), oob = squish,
                       na.value = "grey85", name = "Excess\ndelay, %") +
  labs(title = "Traffic delay by day vs a typical day",
       subtitle = "Red = worse than typical, blue = better; black frame = 10 worst days; grey = excluded days (bad data collection)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 16))
print(g2)
ggsave("worst_days_calendar.png", g2, width = 14, height = 8, dpi = 200, bg = "white")

# ---------- 3. rain_hourly_profile.png ----------
g3 <- ggplot(rain_profile, aes(hr, rec, colour = wet)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c("Dry" = "grey50", "Rain" = "#2166ac")) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Average jam records per hour: dry vs rain (Baku local time)",
       x = "Hour of day", y = "Avg jam records per hour", colour = NULL) +
  theme_minimal(base_size = 12)
print(g3)
ggsave("rain_hourly_profile.png", g3, width = 10, height = 6, dpi = 200, bg = "white")

cat("Готово: worst_days_top10.png, worst_days_calendar.png, rain_hourly_profile.png\n")
