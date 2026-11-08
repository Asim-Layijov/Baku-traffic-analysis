# =====================================================================
# export_maps.R: готовит данные для двух карт с линиями пробок
#  1) пробки в будни в часы пик (с фильтром по скорости)
# Запускать ПОСЛЕ analysis.R (нужен файл data/excluded_days.rds). Занимает 2-4 минуты.
# =====================================================================
library(tidyverse)
library(lubridate)
library(sf)

Sys.setlocale("LC_TIME", "C")      # английские названия месяцев
set.seed(42)

CSV_PATH <- "C:/Users/Asim/Downloads/baku_jams_2024-11_2026-03-20.csv"
BAKU_TZ  <- "Asia/Baku"

excluded <- readRDS("data/excluded_days.rds")$date

# Та же очистка, что в analysis.R
raw <- read_csv(CSV_PATH, show_col_types = FALSE,
                col_select = c(line, street, delay, speedKMH, start_datetime, end_datetime)) %>%
  filter(!is.na(street), street != "", !is.na(line), line != "",
         !is.na(start_datetime), !is.na(end_datetime)) %>%
  mutate(
    start_datetime = as.POSIXct(start_datetime, tz = "UTC"),
    end_datetime   = as.POSIXct(end_datetime,   tz = "UTC"),
    dur_min = as.numeric(difftime(end_datetime, start_datetime, units = "mins"))
  ) %>%
  filter(dur_min > 0) %>%
  mutate(local = force_tz(start_datetime, BAKU_TZ),   # время в датасете уже бакинское
         date  = as_date(local),
         hr    = hour(local),
         wd    = wday(local, week_start = 1),
         delay_min = pmax(delay / 60, 0, na.rm = TRUE)) %>%
  filter(!date %in% excluded)

cat("Записей для карт:", nrow(raw), "\n")

# Превращает столбец line (JSON с координатами) в линии на карте
to_lines <- function(d) {
  geoms <- map(d$line, function(s) {
    x <- as.numeric(str_extract_all(s, '(?<="x":)[0-9.]+')[[1]])
    y <- as.numeric(str_extract_all(s, '(?<="y":)[0-9.]+')[[1]])
    if (length(x) < 2 || length(x) != length(y)) return(NULL)
    st_linestring(cbind(x, y))
  })
  keep <- !map_lgl(geoms, is.null)
  st_sf(d[keep, ] %>% select(-line), geometry = st_sfc(geoms[keep], crs = 4326))
}

# ---------- 1. Будни, часы пик (08-09 и 17-19 по бакинскому времени) ----------
rush <- raw %>%
  filter(wd <= 5, hr %in% c(8, 9, 17, 18, 19), speedKMH < 25) %>%
  distinct(line, .keep_all = TRUE) %>%
  slice_sample(n = 3000) %>%
  mutate(speed_class = case_when(speedKMH < 5  ~ "Under 5 km/h",
                                 speedKMH < 12 ~ "5–12 km/h",
                                 TRUE          ~ "12–25 km/h")) %>%
  select(line, street, speedKMH, delay_min, speed_class)

lines_rush <- to_lines(rush)
print(count(st_drop_geometry(lines_rush), speed_class))

saveRDS(lines_rush, "data/lines_rush.rds")
cat("\nГотово:\n"); print(list.files("data"))
