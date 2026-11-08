# =====================================================================
# analysis.R: полный анализ с нуля. CSV -> таблицы в папке data/
# Запуск: открой файл в RStudio и нажми Source. Займёт 5-10 минут.
#
# Показатели:
#  - jam records = записи о пробках в датасете (одна запись = одна пробка в одном снимке Waze)
#  - reported delay = задержка, которую Waze указывает для записи (минуты), суммы по записям
# Данные сохранялись снимками раз в 5 минут, поэтому счётчики приводятся к полному
# часу (12 снимков), чтобы пропуски в сборе не искажали сравнение.
# =====================================================================
library(tidyverse)
library(lubridate)
library(httr2)
library(sf)
library(scales)

CSV_PATH      <- "C:/Users/Asim/Downloads/baku_jams_2024-11_2026-03-20.csv"
BAKU_TZ       <- "Asia/Baku"
HEX_SIZE_M    <- 500
MIN_RECORDS   <- 200
EXCLUDE_DATES <- as_date(character())   # вручную исключить даты, если нужно
APPLY_DAY_QC  <- TRUE    # TRUE = исключить дни с аномальным сбором данных (рекомендуется)
                         # FALSE = оставить ВСЕ дни; аномальные дни будут помечены звёздочкой *

dir.create("data", showWarnings = FALSE)

# ---------- 1. Загрузка и очистка ----------
raw <- read_csv(CSV_PATH, show_col_types = FALSE,
                col_select = c(line, street, delay, length, speedKMH, start_datetime, end_datetime))
n_raw <- nrow(raw)
cat("Строк в CSV:", n_raw, "\n")

bj <- raw %>%
  rename(len_m = length) %>%          # длина пробки в метрах
  filter(!is.na(street), street != "", !is.na(start_datetime), !is.na(end_datetime)) %>%
  mutate(
    start_datetime = as.POSIXct(start_datetime, tz = "UTC"),   # метка UTC условная: реально это бакинское время
    end_datetime   = as.POSIXct(end_datetime,   tz = "UTC"),
    dur_min   = as.numeric(difftime(end_datetime, start_datetime, units = "mins"))
  ) %>%
  filter(dur_min > 0) %>%
  mutate(
    local      = force_tz(start_datetime, BAKU_TZ),   # время в датасете УЖЕ бакинское, сдвигать на +4 часа нельзя
    date       = as_date(local),
    hr         = hour(local),
    wd         = wday(local, week_start = 1),
    year_month = format(local, "%Y-%m"),
    delay_min  = pmax(delay / 60, 0, na.rm = TRUE),
    len_m      = coalesce(len_m, 0),
    lon = as.numeric(str_extract(str_sub(line, 1, 80), '(?<="x":)[0-9.]+')),
    lat = as.numeric(str_extract(str_sub(line, 1, 80), '(?<="y":)[0-9.]+'))
  ) %>%
  select(-line, -delay, -end_datetime, -dur_min)
rm(raw); gc()

# ---------- 1b. Контроль качества дней ----------
# Сбор идёт раз в 5 минут, то есть максимум 288 снимков в сутки. Исключаем дни:
#  - со снимками < 200 (неполные сутки) или > 300 (дублирующий сбор),
#  - с числом записей на снимок в 1.4+ раза выше нормы месяца (удвоение записей).
day_qc <- bj %>%
  group_by(date) %>%
  summarise(records = n(), snapshots = n_distinct(start_datetime), .groups = "drop") %>%
  mutate(per_snapshot = records / snapshots, month = format(date, "%Y-%m"),
         ok_snap = snapshots >= 200 & snapshots <= 300)

month_ref <- day_qc %>%
  filter(ok_snap) %>%
  group_by(month) %>%
  summarise(ref = median(per_snapshot), .groups = "drop")

day_qc <- day_qc %>%
  left_join(month_ref, by = "month") %>%
  mutate(too_many = coalesce(per_snapshot > 1.4 * ref, FALSE),
         exclude  = !ok_snap | too_many | date %in% EXCLUDE_DATES,
         why = case_when(snapshots < 200 ~ "incomplete day (under 200 snapshots)",
                         snapshots > 300 ~ "duplicated collection (over 300 snapshots)",
                         too_many        ~ "records per snapshot far above the month norm",
                         date %in% EXCLUDE_DATES ~ "excluded manually",
                         TRUE ~ NA_character_))

excluded_days <- day_qc %>% filter(exclude) %>% select(date, snapshots, records, per_snapshot, why)
cat("Исключено дней:", nrow(excluded_days), "из", nrow(day_qc), "\n")
print(excluded_days, n = 100)

if (APPLY_DAY_QC) bj <- bj %>% filter(!date %in% excluded_days$date)
bj$snap <- match(bj$start_datetime, sort(unique(bj$start_datetime)))
n_days <- n_distinct(bj$date)
n_snap <- n_distinct(bj$snap)
cat("Записей:", nrow(bj), "| дней:", n_days, "\n")

# ---------- 2. Километры дорог в пробке ----------
# В каждом снимке складываем длины всех пробок (км), потом усредняем по часу, дню, месяцу.
# Такое среднее не зависит от того, сколько снимков сохранилось в часе.
hourly <- bj %>%
  mutate(datetime = floor_date(local, "hour")) %>%
  group_by(datetime) %>%
  summarise(n_jams = n(), snapshots = n_distinct(snap),
            delay_hours = sum(delay_min) / 60, jam_km = sum(len_m) / 1000, .groups = "drop") %>%
  filter(snapshots >= 6) %>%                      # час учитываем, если есть хотя бы 6 снимков из 12
  mutate(km_snap     = jam_km / snapshots,             # км пробок в один момент (среднее за час)
         delay_rec_h = delay_hours * 12 / snapshots,   # заявленная задержка за полный час, часы
         date = as_date(datetime), hr = hour(datetime), wd = wday(datetime, week_start = 1))

# типичный день: км пробок по часам, будни и выходные
profile <- hourly %>%
  mutate(type = ifelse(wd >= 6, "Weekend", "Weekday")) %>%
  group_by(type, hr) %>%
  summarise(km = mean(km_snap), .groups = "drop")

# среднее за сутки
daily_km <- hourly %>%
  group_by(date, wd) %>%
  summarise(hours = n(), km = mean(km_snap), .groups = "drop") %>%
  filter(hours >= 20)

wd_tbl <- daily_km %>%
  group_by(wd) %>%
  summarise(km = mean(km), days = n(), .groups = "drop")

monthly <- daily_km %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(km = mean(km), days = n(), .groups = "drop") %>%
  left_join(bj %>% group_by(year_month) %>% summarise(avg_delay = mean(delay_min), .groups = "drop"),
            by = "year_month")

# ---------- 3. Улицы: по одной строке на улицу ----------
streets <- bj %>%
  group_by(street) %>%
  summarise(delay_h = sum(delay_min) / 60, records = n(),
            jam_hours_day = n_distinct(snap) / n_snap * 24,   # сколько часов в сутки на улице есть хотя бы одна пробка
            delay_per_jam = mean(delay_min), avg_speed = mean(speedKMH, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(delay_h)) %>%
  slice_head(n = 200)

# ---------- 4. Погода ----------
rng <- range(hourly$date)
resp <- request("https://archive-api.open-meteo.com/v1/archive") %>%
  req_url_query(latitude = 40.4093, longitude = 49.8671,
                start_date = format(rng[1]), end_date = format(rng[2]),
                hourly = "precipitation", timezone = BAKU_TZ) %>%
  req_retry(max_tries = 3) %>%
  req_perform() %>%
  resp_body_json()

weather <- tibble(
  datetime = ymd_hm(unlist(resp$hourly$time), tz = BAKU_TZ),
  precip   = map_dbl(resp$hourly$precipitation, ~ if (is.null(.x)) NA_real_ else as.numeric(.x))
)

panel <- hourly %>%
  inner_join(weather, by = "datetime") %>%
  filter(!is.na(precip)) %>%
  mutate(
    rain_cat   = cut(precip, breaks = c(-Inf, 0.1, 1, Inf), right = FALSE,
                     labels = c("Dry", "Light rain", "Rainfall")),
    hour_local = factor(hr),
    wday_local = factor(wd),
    month_f    = factor(format(datetime, "%Y-%m"))
  )
print(table(panel$rain_cat))

# сухо и дождь по часам (как график rain_hourly_profile)
rain_profile <- panel %>%
  mutate(wet = ifelse(rain_cat == "Dry", "Dry", "Rain")) %>%
  group_by(hr, wet) %>%
  summarise(km = mean(km_snap), hours = n(), .groups = "drop")

# ---------- 5. Модель эффекта дождя ----------
# Контроль: час суток, день недели, месяц; offset учитывает число снимков в часе
fit_effect <- function(y, label) {
  f  <- as.formula(paste(y, "~ rain_cat + hour_local + wday_local + month_f + offset(log(snapshots))"))
  m  <- glm(f, data = panel, family = quasipoisson())
  vc <- if (requireNamespace("sandwich", quietly = TRUE)) {
    sandwich::vcovCL(m, cluster = panel$date)
  } else vcov(m)
  est <- coef(m); se <- sqrt(diag(vc))
  idx <- grep("^rain_cat", names(est))
  tibble(outcome = label, level = sub("rain_cat", "", names(est)[idx]),
         estimate = est[idx], lo = est[idx] - 1.96 * se[idx], hi = est[idx] + 1.96 * se[idx]) %>%
    mutate(across(c(estimate, lo, hi), ~ (exp(.x) - 1) * 100))
}

results <- bind_rows(
  fit_effect("jam_km",      "Road length in jam (% change)"),
  fit_effect("delay_hours", "Reported delay (% change)")
) %>%
  mutate(level = factor(level, levels = c("Light rain", "Rainfall")),
         across(c(estimate, lo, hi), ~ round(.x, 2)))
print(results)

# ---------- 6. Худшие дни (заявленная задержка за сутки) ----------
daily <- hourly %>%
  group_by(date) %>%
  summarise(delay_hours = mean(delay_rec_h) * 24, hours_covered = n(), .groups = "drop") %>%
  filter(hours_covered >= 20, delay_hours > 0)

daily_weather <- weather %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(rain_mm = sum(precip, na.rm = TRUE),
            max_mm_h = suppressWarnings(max(precip, na.rm = TRUE)), .groups = "drop")

holidays_fixed <- tribble(
  ~md, ~holiday,
  "01-01", "New Year", "01-02", "New Year", "01-20", "Martyrs' Day", "03-08", "Women's Day",
  "05-09", "Victory Day", "05-28", "Independence Day", "06-15", "National Salvation Day",
  "06-26", "Armed Forces Day", "11-08", "Victory Day (2020)", "11-09", "Flag Day",
  "11-12", "Constitution Day", "12-31", "Solidarity Day"
)
all_holidays <- tibble(date = seq(min(daily$date), max(daily$date), by = "day")) %>%
  mutate(md = format(date, "%m-%d")) %>%
  inner_join(holidays_fixed, by = "md") %>%
  select(date, holiday) %>%
  distinct(date, .keep_all = TRUE)

events <- tribble(
  ~start, ~end, ~event,
  "2024-11-11", "2024-11-22", "COP29",
  "2025-09-19", "2025-09-21", "Formula 1 Baku GP"
)
event_days <- events %>%
  mutate(date = map2(as_date(start), as_date(end), ~ seq(.x, .y, by = "day"))) %>%
  unnest(date) %>%
  select(date, event)

daily <- daily %>%
  left_join(daily_weather, by = "date") %>%
  left_join(all_holidays,  by = "date") %>%
  left_join(event_days,    by = "date") %>%
  mutate(wd_f = factor(wday(date, week_start = 1)),
         month_f = factor(format(date, "%Y-%m")),
         eve = (date + 1) %in% all_holidays$date)

m0 <- lm(log(delay_hours) ~ month_f + wd_f, data = daily)

daily <- daily %>%
  mutate(
    typical_hours = exp(fitted(m0)),
    excess_pct    = (exp(resid(m0)) - 1) * 100,
    reason = case_when(
      !is.na(event)   ~ event,
      !is.na(holiday) ~ paste0("Holiday: ", holiday),
      eve             ~ "Day before a holiday",
      max_mm_h >= 1   ~ "Heavy rain hour (1+ mm/h)",
      rain_mm  >= 5   ~ "Rainy day (5+ mm)",
      TRUE            ~ "No known cause in data"
    )
  )

worst <- daily %>% slice_max(excess_pct, n = 10) %>% arrange(desc(excess_pct))
print(worst %>% select(date, delay_hours, typical_hours, excess_pct, reason))

thr <- sort(daily$excess_pct, decreasing = TRUE)[30]
reasons <- daily %>%
  group_by(reason) %>%
  summarise(all_days = n(), in_top30 = sum(excess_pct >= thr), .groups = "drop") %>%
  mutate(share_all_days = percent(all_days / sum(all_days), 0.1),
         share_top30    = percent(in_top30 / 30, 0.1)) %>%
  arrange(desc(in_top30))

# ---------- 7. Шестиугольная карта ----------
bj_hex <- bj %>%
  filter(!is.na(lon), !is.na(lat), lon > 49.0, lon < 50.5, lat > 40.0, lat < 41.0) %>%
  transmute(wd, hr, street, delay_min, len_m, speedKMH, lon_r = round(lon, 4), lat_r = round(lat, 4))

xy       <- distinct(bj_hex, lon_r, lat_r)
xy_sf    <- st_as_sf(xy, coords = c("lon_r", "lat_r"), crs = 4326, remove = FALSE) %>% st_transform(32639)
hex_geom <- st_make_grid(xy_sf, cellsize = HEX_SIZE_M, square = FALSE)
hex      <- st_sf(hex_id = seq_along(hex_geom), geometry = hex_geom)
xy_hex   <- st_join(xy_sf, hex, join = st_within) %>%
  st_drop_geometry() %>% filter(!is.na(hex_id)) %>% select(lon_r, lat_r, hex_id)

bj_hex <- bj_hex %>% inner_join(xy_hex, by = c("lon_r", "lat_r"))

hex_stats <- bj_hex %>%
  group_by(hex_id) %>%
  summarise(morning_h = sum(delay_min[hr %in% 7:10]) / 60,
            evening_h = sum(delay_min[hr %in% 17:20]) / 60,
            other_h   = sum(delay_min[!hr %in% c(7:10, 17:20)]) / 60,
            records   = n(),
            avg_speed = mean(speedKMH, na.rm = TRUE), .groups = "drop") %>%
  mutate(delay_h = morning_h + evening_h + other_h) %>%
  filter(records >= MIN_RECORDS) %>%
  mutate(pattern = case_when(
    morning_h / 4 >= evening_h / 4 & morning_h / 4 >= (other_h / 16) * 1.5 ~ "Morning-heavy",
    evening_h / 4 >= morning_h / 4 & evening_h / 4 >= (other_h / 16) * 1.5 ~ "Evening-heavy",
    TRUE ~ "All-day"))

hex_street <- bj_hex %>%
  count(hex_id, street) %>%
  group_by(hex_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(hex_id, street)

hex_out <- hex %>%
  inner_join(hex_stats, by = "hex_id") %>%
  left_join(hex_street, by = "hex_id") %>%
  st_transform(4326)

# Пульс города: записей о пробках в ячейке за час в средний будний день
cent     <- suppressWarnings(st_coordinates(st_centroid(hex_out)))
hex_cent <- tibble(hex_id = hex_out$hex_id, lon = cent[, 1], lat = cent[, 2], street = hex_out$street)
n_wkdays <- n_distinct(bj$date[bj$wd <= 5])

# Для анимации: в каких ячейках бывают пробки в этот час и насколько медленно там едут
hex_hour <- bj_hex %>%
  filter(wd <= 5, hex_id %in% hex_out$hex_id) %>%
  group_by(hex_id, hr) %>%
  summarise(records = n(), speed = mean(speedKMH, na.rm = TRUE), .groups = "drop") %>%
  mutate(per_day = records / n_wkdays) %>%
  filter(per_day >= 1) %>%                  # ячейка показывается, если пробка в этот час в среднем есть каждый будний день
  left_join(hex_cent, by = "hex_id")

# ---------- 8. Ключевые цифры и сохранение ----------
rain_row <- results %>% filter(str_detect(outcome, "elay")) %>% slice_tail(n = 1)

kpi <- list(
  n_raw = n_raw, n_records = nrow(bj), n_days = n_days,
  n_excluded = if (APPLY_DAY_QC) nrow(excluded_days) else 0L,
  n_flagged = nrow(excluded_days), qc_applied = APPLY_DAY_QC,
  date_from = min(bj$date), date_to = max(bj$date),
  km_avg = sum(bj$len_m) / 1000 / n_snap,
  delay_per_jam = mean(bj$delay_min),
  rain_pct = rain_row$estimate, rain_lo = rain_row$lo, rain_hi = rain_row$hi,
  rain_hours = sum(panel$rain_cat == "Rainfall")
)

saveRDS(kpi,           "data/kpi.rds")
saveRDS(profile,       "data/profile.rds")
saveRDS(wd_tbl,        "data/wd_tbl.rds")
saveRDS(monthly,       "data/monthly.rds")
saveRDS(streets,       "data/streets.rds")
saveRDS(hex_out,       "data/hex_map.rds")
saveRDS(hex_hour,      "data/hex_hour.rds")
saveRDS(results,       "data/results.rds")
saveRDS(rain_profile,  "data/rain_profile.rds")
saveRDS(daily %>% select(date, delay_hours, typical_hours, excess_pct, reason, rain_mm), "data/daily.rds")
saveRDS(worst,         "data/worst.rds")
saveRDS(reasons,       "data/reasons.rds")
saveRDS(excluded_days, "data/excluded_days.rds")

print(kpi)
cat("\nГотово. Файлы в data/:\n"); print(list.files("data"))
