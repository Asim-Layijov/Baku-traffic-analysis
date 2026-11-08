# Запускать ОДИН РАЗ в текущей сессии R, где ещё лежат твои объекты
# (baku_jams, hex, hex_stats, hex_street, joined, ranked, results, dose, by_hour, daily, worst, panel).
# Скрипт сохраняет маленькие таблицы в папку data/, чтобы сайт собирался за секунды без 5 млн строк.

library(tidyverse)
library(lubridate)
library(sf)

dir.create("data", showWarnings = FALSE)

# ---------- 1. Всё в бакинском времени ----------
bj <- baku_jams %>%
  transmute(
    local      = with_tz(as.POSIXct(start_datetime, tz = "UTC"), "Asia/Baku"),
    date       = as_date(local),
    hr         = hour(local),
    wd         = wday(local, week_start = 1),
    year_month = format(local, "%Y-%m"),
    delay_min  = pmax(delay_minutes, 0, na.rm = TRUE),
    speedKMH, street
  )

# ---------- 2. Тепловая карта: день недели x час (в среднем за день) ----------
heat <- bj %>%
  group_by(wd, hr) %>%
  summarise(n_days = n_distinct(date), delay_h = sum(delay_min) / 60, .groups = "drop") %>%
  mutate(delay_h_per_day = delay_h / n_days)

# ---------- 3. Динамика по месяцам ----------
monthly <- bj %>%
  group_by(year_month) %>%
  summarise(records = n(), delay_h = sum(delay_min) / 60, avg_delay = mean(delay_min), .groups = "drop")

# ---------- 4. Топ улиц ----------
top_streets <- bj %>%
  filter(!is.na(street), street != "") %>%
  group_by(street) %>%
  summarise(delay_h = sum(delay_min) / 60, records = n(),
            avg_speed = mean(speedKMH, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(delay_h)) %>%
  slice_head(n = 30)

# ---------- 5. Шестиугольники (среднее за день, часы) ----------
n_days <- n_distinct(bj$date)
hex_out <- hex %>%
  inner_join(hex_stats, by = "hex_id") %>%
  left_join(hex_street, by = "hex_id") %>%
  st_transform(4326) %>%
  mutate(across(c(delay_h, morning_h, evening_h, other_h), ~ .x / n_days))

# ---------- 6. Ключевые цифры ----------
rain_row <- results %>% filter(str_detect(outcome, "delay")) %>% slice_tail(n = 1)

kpi <- list(
  n_raw       = 6965817,                      # строк в исходном CSV (из read_csv)
  n_records   = nrow(baku_jams),
  n_days      = n_days,
  date_from   = min(bj$date),
  date_to     = max(bj$date),
  delay_hours = sum(bj$delay_min) / 60,
  rain_pct    = rain_row$estimate,
  rain_lo     = rain_row$lo,
  rain_hi     = rain_row$hi,
  rain_hours  = sum(as.integer(panel$rain_cat) == 3)
)

# ---------- 7. Сохранение ----------
saveRDS(kpi,         "data/kpi.rds")
saveRDS(heat,        "data/heat_local.rds")
saveRDS(monthly,     "data/monthly.rds")
saveRDS(top_streets, "data/top_streets.rds")
saveRDS(hex_out,     "data/hex_map.rds")
saveRDS(ranked %>% slice_head(n = 200), "data/ranked_top.rds")
saveRDS(results,     "data/results.rds")
saveRDS(dose,        "data/dose.rds")
saveRDS(by_hour,     "data/by_hour.rds")
saveRDS(daily %>% select(date, delay_hours, typical_hours, excess_pct, reason, rain_mm),
        "data/daily.rds")
saveRDS(worst,       "data/worst.rds")
saveRDS(reason_tbl,  "data/reasons.rds")

print(kpi)
print(list.files("data"))