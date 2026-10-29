# The Indian Rupee Against Major Currencies: Rupee Explorer

An interactive website, built in R with Shiny, that explores how the Indian rupee (INR) has moved against the US dollar (USD), euro (EUR), UK pound (GBP) and Japanese yen (JPY) from January 2000 to October 2026.

**Author:** (your name)
**Course / Faculty:** (your course and faculty name)
**Date:** (submission date)

---

## What the website shows

| Tab | What it shows | Controls |
|---|---|---|
| **Overview** | Latest rupee price of each currency, percentage change since 2000, indexed line chart | none |
| **Trend** | Exchange rates over time, as rupees per unit or as an index (start = 100), with a Chart/Table switch | currency checkboxes, date range calendar, display mode |
| **Volatility** | Rolling volatility of the rupee rate, with crisis periods shaded | currency checkboxes, date range calendar, rolling window (7, 30, 90 days), annualise, mark crisis periods |
| **Heatmap** | Volatility for every month of every year | currency, year range slider |
| **World map** | Change in the rupee price of each economy's currency between two dates | From and To calendars, economy checkboxes |
| **Candlestick** | Open, high, low and close of the rate for each period | currency, candle size (monthly, quarterly, yearly), date range calendar |
| **Forecast** | 6-month (up to 12) projection with 80% and 95% uncertainty bands | currency, months to forecast, history shown, band checkboxes |
| **About** | Data sources and tools | none |

Every tab has a **Reset controls** button.

---

## Data

- **Source:** European Central Bank (ECB) euro foreign exchange reference rates.
- **Historical file:** `euro-daily-hist_1999_.csv` (Kaggle copy of the ECB data), covering up to 4 June 2026.
- **Recent rates:** downloaded from the **Frankfurter API**, which serves ECB reference rates, for 5 June 2026 to 28 October 2026 (104 rows).
- **Final table:** `rupee_daily_full.csv`, with 6,857 working-day rows from 13 January 2000 to 28 October 2026.

| Column | Meaning |
|---|---|
| `date` | working day |
| `inr_per_usd` | rupees needed to buy 1 US dollar |
| `inr_per_eur` | rupees needed to buy 1 euro |
| `inr_per_gbp` | rupees needed to buy 1 UK pound |
| `inr_per_100_jpy` | rupees needed to buy 100 Japanese yen |

The ECB quotes every rate per 1 euro. The cleaning script converts these to rupees per one unit of each currency by dividing the rupee rate by the foreign currency's rate. Yen is shown per 100 yen because one yen is worth too little to read easily.

The ECB publishes one rate per working day, at about 16:00 Central European Time. There are no weekend or holiday rates, and the rate for "today" appears only after that publication time.

---

## Project structure

```
ind_rupee_project/
├── R/
│   ├── 01_clean.R                   # reads the Kaggle file, converts to rupees per unit
│   └── 02_api.R                     # downloads recent rates from the Frankfurter API
├── data/
│   ├── euro-daily-hist_1999_.csv    # raw ECB data
│   ├── rupee_daily_clean.csv        # output of 01_clean.R
│   └── rupee_daily_full.csv         # output of 02_api.R (the final table)
└── ind_rupee_interactive_website/
    ├── app.R                        # the Shiny website
    └── data/
        └── rupee_daily_full.csv     # copy of the final table, read by the app
```

---

## How to run

### 1. Install the packages (first time only)

```r
install.packages(c("shiny", "bslib", "tidyverse", "lubridate",
                   "plotly", "DT", "forecast", "zoo", "jsonlite"))
```

### 2. Build the data (skip if `rupee_daily_full.csv` already exists)

Open the R project, make sure the working directory is the project root (`getwd()`), then run:

```r
source("R/01_clean.R")
source("R/02_api.R")      # needs an internet connection
```

### 3. Copy the data into the website folder

```r
dir.create("ind_rupee_interactive_website/data", recursive = TRUE, showWarnings = FALSE)
file.copy("data/rupee_daily_full.csv",
          "ind_rupee_interactive_website/data/", overwrite = TRUE)
```

### 4. Start the website

```r
shiny::runApp("ind_rupee_interactive_website")
```

The site opens in your browser at an address like `127.0.0.1:xxxx`. Stop it with the red stop button in the RStudio Console.

---

## Updating the data

The website reads a saved file, so it does not update by itself. After about 8:30 pm IST on a working day (the ECB publishes at about 16:00 CET), run:

```r
source("R/02_api.R")
file.copy("data/rupee_daily_full.csv",
          "ind_rupee_interactive_website/data/", overwrite = TRUE)
```

Then click **Reload App** in the app window.

---

## How the numbers are calculated

| Measure | Formula |
|---|---|
| **Percentage change** | (rate on end date ÷ rate on start date − 1) × 100. A positive value means the rupee weakened against that currency. |
| **Index (start = 100)** | rate ÷ rate on the first day of the chosen range × 100. Index − 100 equals the percentage change. |
| **Daily change** | (today's rate ÷ yesterday's rate − 1) × 100 |
| **Rolling volatility** | standard deviation of the daily changes over the last 7, 30 or 90 working days |
| **Annualised volatility** | rolling volatility × √252 (about 252 working days a year) |
| **Heatmap cell** | standard deviation of the daily changes inside one calendar month (not annualised) |
| **Candlestick** | open = first rate of the period, close = last rate, high = highest, low = lowest. Red means the rupee weakened, green means it strengthened. |
| **Forecast** | daily rates averaged to monthly values, an ETS (exponential smoothing) model fitted with `forecast::ets()`, and 80% and 95% prediction intervals |

### Crisis periods marked on the Volatility page

- Global financial crisis: Sep 2008 to Mar 2009
- Taper tantrum: May to Sep 2013
- COVID-19: Feb to Jun 2020

These shaded periods are a visual aid. They show where the spikes coincide with known events and do not prove the cause.

---

## Limitations

- Only four currencies are covered.
- Rates are ECB reference rates, not the RBI's official reference rates.
- The forecast extends past patterns. It cannot predict policy changes or sudden shocks, and it has not been tested against held-back data. It is not financial advice.
- The newest month in the data is partial, so its monthly average and candle use only the days available.
- The percentage change depends on the two dates chosen, so quote the dates with any figure.
- The app needs an R session or a Shiny server to run. It is not a static web page.

---

## Tools

R, Shiny, bslib, tidyverse, lubridate, plotly, DT, forecast, zoo, jsonlite.

## Data source credit

European Central Bank euro foreign exchange reference rates, via a Kaggle copy and the Frankfurter API.
