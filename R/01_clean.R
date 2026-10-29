library(tidyverse)

# 1. Read the file. The "-" entries mean "no rate that day", so treat them as missing.
raw <- read_csv("data/euro-daily-hist_1999_.csv",
                na = c("", "NA", "-"), show_col_types = FALSE)

# 2. Keep only the columns we need and give them short names.
#    Every value here means "units of that currency per 1 euro".
fx <- raw |>
  rename(date = 1) |>
  select(date,
         usd = `[US dollar ]`,
         gbp = `[UK pound sterling ]`,
         jpy = `[Japanese yen ]`,
         inr = `[Indian rupee ]`) |>
  mutate(date = as.Date(date)) |>
  arrange(date)

# 3. Remove days with no rupee rate (before Jan 2000 and holidays).
fx <- fx |> filter(!is.na(inr))

# 4. Convert to rupees per foreign currency (what our project needs).
fx <- fx |>
  mutate(inr_per_usd     = inr / usd,
         inr_per_eur     = inr,
         inr_per_gbp     = inr / gbp,
         inr_per_100_jpy = inr / jpy * 100) |>
  select(date, starts_with("inr_per"))

# 5. Check the result.
nrow(fx)
range(fx$date)
colSums(is.na(fx))
head(fx, 3)
tail(fx, 3)

# 6. Save the clean table.
write_csv(fx, "data/rupee_daily_clean.csv")