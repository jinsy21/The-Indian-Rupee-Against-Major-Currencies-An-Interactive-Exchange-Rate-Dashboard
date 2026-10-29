library(tidyverse)
library(jsonlite)

fx <- read_csv("data/rupee_daily_clean.csv", show_col_types = FALSE)

# Ask the API for one block of dates (rates are per 1 euro, same as the Kaggle file)
get_chunk <- function(from, to) {
  url <- paste0("https://api.frankfurter.dev/v1/", from, "..", to,
                "?base=EUR&symbols=INR,USD,GBP,JPY")
  js <- fromJSON(url, simplifyVector = FALSE)
  imap(js$rates, \(r, d) tibble(date = as.Date(d),
                                usd = r$USD, gbp = r$GBP,
                                jpy = r$JPY, inr = r$INR)) |>
    list_rbind()
}

# Fetch in blocks of 60 days so we always get daily values
starts <- seq(max(fx$date) + 1, Sys.Date(), by = "60 days")
new <- map(starts, \(s) get_chunk(s, min(s + 59, Sys.Date()))) |>
  list_rbind() |>
  distinct(date, .keep_all = TRUE)

# Same conversion as step 3: rupees per foreign currency
new <- new |>
  mutate(inr_per_usd     = inr / usd,
         inr_per_eur     = inr,
         inr_per_gbp     = inr / gbp,
         inr_per_100_jpy = inr / jpy * 100) |>
  select(all_of(names(fx)))

# Join the old and new data
fx_all <- bind_rows(fx, new) |> arrange(date)

# Checks
nrow(new)
range(new$date)
tail(fx, 1)
head(new, 1)
nrow(fx_all)
tail(fx_all, 3)

write_csv(fx_all, "data/rupee_daily_full.csv")