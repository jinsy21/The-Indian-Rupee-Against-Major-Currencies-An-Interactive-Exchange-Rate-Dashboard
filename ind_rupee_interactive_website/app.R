# app.R  --  Rupee Explorer (interactive website in R / Shiny)
# Put this file inside the folder: ind_rupee_interactive_website
# and keep the data at: ind_rupee_interactive_website/data/rupee_daily_full.csv
#
# install.packages(c("shiny","bslib","tidyverse","lubridate","plotly","DT","forecast","zoo"))
# Run with: shiny::runApp("ind_rupee_interactive_website")

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(forecast)
library(zoo)

# ------------------------------------------------------------------
# Data
# ------------------------------------------------------------------
fx <- read_csv("data/rupee_daily_full.csv", show_col_types = FALSE) |> arrange(date)

cur_cols   <- c(USD = "inr_per_usd", EUR = "inr_per_eur",
                GBP = "inr_per_gbp", JPY = "inr_per_100_jpy")
cur_names  <- c(USD = "US dollar", EUR = "Euro", GBP = "UK pound", JPY = "Japanese yen (100)")
cur_choices <- setNames(names(cur_names), paste0(names(cur_names), " - ", cur_names))
cur_colors <- c(USD = "#2b6fd6", EUR = "#1aa37a", GBP = "#e4572e", JPY = "#e8a200")
rupee <- "\u20b9"

long <- fx |>
  pivot_longer(-date, names_to = "col", values_to = "rate") |>
  mutate(cur = names(cur_cols)[match(col, cur_cols)]) |>
  select(date, cur, rate) |>
  filter(!is.na(rate)) |>
  arrange(cur, date) |>
  group_by(cur) |>
  mutate(ret = (rate / lag(rate) - 1) * 100) |>
  ungroup()

min_d <- min(long$date)
max_d <- max(long$date)

events <- tibble(
  label = c("Global financial crisis", "Taper tantrum", "COVID-19"),
  from  = as.Date(c("2008-09-01", "2013-05-01", "2020-02-15")),
  to    = as.Date(c("2009-03-31", "2013-09-30", "2020-06-30"))
)

eurozone <- c("AUT","BEL","HRV","CYP","EST","FIN","FRA","DEU","GRC","IRL",
              "ITA","LVA","LTU","LUX","MLT","NLD","PRT","SVK","SVN","ESP")
map_base <- bind_rows(
  tibble(iso = "USA", cur = "USD", place = "United States"),
  tibble(iso = "GBR", cur = "GBP", place = "United Kingdom"),
  tibble(iso = "JPN", cur = "JPY", place = "Japan"),
  tibble(iso = eurozone, cur = "EUR", place = "Eurozone")
)

# ------------------------------------------------------------------
# Look and feel (clean, like the Energy Explorer screenshots)
# ------------------------------------------------------------------
css <- "
body{background:#fbfaf8}
.navbar{background:#fbfaf8 !important;border-bottom:1px solid #e6e3dd}
.navbar-brand{font-weight:800;font-size:1.4rem;color:#111 !important}
.navbar .nav-link{color:#444 !important;font-weight:500;padding:.7rem 1rem;border-bottom:2px solid transparent}
.navbar .nav-link.active{color:#111 !important;font-weight:700;border-bottom:2px solid #111}
.ctrl-card,.chart-card{border:1px solid #e6e3dd;border-radius:14px;background:#fff;box-shadow:none}
.ctrl-card .card-body{padding:18px 22px}
.ctrl-label,.ctrl-card label.control-label{font-size:.72rem;letter-spacing:.08em;text-transform:uppercase;color:#777;font-weight:600}
.page-head{font-weight:800;font-size:2rem;line-height:1.15;margin:26px 0 8px;max-width:760px;color:#111}
.page-sub{color:#555;max-width:760px;margin-bottom:18px}
.chart-head{display:flex;justify-content:space-between;align-items:flex-start;gap:12px}
.chart-head h5{font-weight:700;margin:0}
.chart-head .sub{color:#777;font-size:.9rem;margin:2px 0 8px}
.chart-head .shiny-input-container{margin:0}
.chart-head .form-check-inline{margin-right:0}
.chart-head .radio-inline,.chart-head .form-check{padding:0}
.insight{color:#333;margin-top:14px;max-width:900px}
.btn-outline-dark{border-radius:10px;font-weight:600}
.bslib-value-box{border-radius:14px !important;border:1px solid #e6e3dd !important;box-shadow:none !important}
"

# ------------------------------------------------------------------
# UI helpers
# ------------------------------------------------------------------
chart_card <- function(id, title, subtitle, chart, table = NULL) {
  card(
    class = "chart-card",
    div(class = "chart-head",
        div(h5(title), p(class = "sub", subtitle)),
        if (!is.null(table))
          radioButtons(paste0(id, "_view"), NULL, c("Chart", "Table"), inline = TRUE)),
    if (!is.null(table)) {
      tagList(
        conditionalPanel(sprintf("input.%s_view == 'Chart'", id), chart),
        conditionalPanel(sprintf("input.%s_view == 'Table'", id), table)
      )
    } else chart
  )
}

reset_box <- function(id) {
  div(div(class = "ctrl-label", style = "margin-bottom:6px", "Reset"),
      actionButton(id, "Reset controls", class = "btn btn-outline-dark w-100"))
}

date_input <- function(id, label = "Date range (calendar)") {
  dateRangeInput(id, label, start = min_d, end = max_d, min = min_d, max = max_d,
                 format = "dd M yyyy", separator = " to ")
}

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- page_navbar(
  title = "Rupee Explorer",
  window_title = "Rupee Explorer",
  id = "nav",
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#fbfaf8", fg = "#111", primary = "#111",
                   base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif")),
  header = tags$style(HTML(css)),

  # ---------------- Overview ----------------
  nav_panel("Overview",
    uiOutput("ov_head"),
    p(class = "page-sub",
      "The Indian rupee against the US dollar, euro, pound and yen, from 2000 to today. ",
      "Use the tabs above to explore trends, volatility, a world map and a forecast."),
    uiOutput("ov_boxes"),
    br(),
    chart_card("ov", "Rupee cost of each currency, indexed",
               "13 Jan 2000 = 100, all four currencies", plotlyOutput("ov_plot", height = "460px"))
  ),

  # ---------------- Trend ----------------
  nav_panel("Trend",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(4, 4, 2, 2),
        checkboxGroupInput("tr_cur", "Currencies", choices = cur_choices,
                           selected = names(cur_cols)),
        date_input("tr_dates"),
        radioButtons("tr_mode", "Display mode",
                     c("Index (start = 100)" = "idx", "Rupees per unit" = "abs")),
        reset_box("tr_reset"))),
    uiOutput("tr_head"),
    chart_card("tr", "Rupee exchange rates over time", "Hover the chart to read each day",
               plotlyOutput("tr_plot", height = "480px"), DTOutput("tr_table")),
    uiOutput("tr_sub")
  ),

  # ---------------- Volatility ----------------
  nav_panel("Volatility",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(3, 3, 4, 2),
        checkboxGroupInput("vol_cur", "Currencies", choices = cur_choices,
                           selected = c("USD", "EUR")),
        date_input("vol_dates"),
        div(
          selectInput("vol_win", "Rolling window (days)",
                      c("7" = 7, "30" = 30, "90" = 90), selected = 30),
          checkboxInput("vol_annual", "Annualise (x sqrt 252)", TRUE),
          checkboxInput("vol_events", "Mark crisis periods (2008, 2013, 2020)", TRUE)),
        reset_box("vol_reset"))),
    uiOutput("vol_head"),
    chart_card("vol", "Rolling volatility", "Standard deviation of daily % change in the rupee rate",
               plotlyOutput("vol_plot", height = "480px"), DTOutput("vol_table"))
  ),

  # ---------------- Heatmap ----------------
  nav_panel("Heatmap",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(4, 6, 2),
        selectInput("hm_cur", "Currency", choices = cur_choices, selected = "USD"),
        sliderInput("hm_years", "Year range", min = year(min_d), max = year(max_d),
                    value = c(year(min_d), year(max_d)), step = 1, sep = ""),
        reset_box("hm_reset"))),
    uiOutput("hm_head"),
    chart_card("hm", "Volatility by month", "Darker cells = more volatile months",
               plotlyOutput("hm_plot", height = "620px"))
  ),

  # ---------------- World map ----------------
  nav_panel("World map",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(3, 3, 4, 2),
        dateInput("map_from", "From (calendar)", value = min_d, min = min_d, max = max_d,
                  format = "dd M yyyy"),
        dateInput("map_to", "To (calendar)", value = max_d, min = min_d, max = max_d,
                  format = "dd M yyyy"),
        checkboxGroupInput("map_cur", "Show economies", choices = cur_choices,
                           selected = names(cur_cols), inline = TRUE),
        reset_box("map_reset"))),
    uiOutput("map_head"),
    chart_card("map", "Change in rupee price of each currency",
               "Red = the rupee weakened, blue = the rupee strengthened",
               plotlyOutput("map_plot", height = "540px"))
  ),

  # ---------------- Candlestick ----------------
  nav_panel("Candlestick",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(3, 3, 4, 2),
        selectInput("cd_cur", "Currency", choices = cur_choices, selected = "USD"),
        radioButtons("cd_freq", "Candle size", c("Monthly", "Quarterly", "Yearly"),
                     selected = "Monthly", inline = TRUE),
        date_input("cd_dates"),
        reset_box("cd_reset"))),
    uiOutput("cd_head"),
    chart_card("cd", "Open, high, low, close", "Red candle = rupee weakened in that period, green = strengthened",
               plotlyOutput("cd_plot", height = "500px"))
  ),

  # ---------------- Forecast ----------------
  nav_panel("Forecast",
    card(class = "ctrl-card",
      layout_columns(col_widths = c(3, 3, 3, 3),
        div(selectInput("fc_cur", "Currency", choices = cur_choices, selected = "USD"),
            selectInput("fc_hist", "History shown",
                        c("3 years" = 3, "5 years" = 5, "10 years" = 10, "All" = 0), selected = 10)),
        sliderInput("fc_h", "Months to forecast", min = 3, max = 12, value = 6, step = 1),
        checkboxGroupInput("fc_bands", "Uncertainty bands",
                           c("80% range" = "80", "95% range" = "95"), selected = c("80", "95")),
        reset_box("fc_reset"))),
    uiOutput("fc_head"),
    chart_card("fc", "Forecast with uncertainty", "Monthly average rate, ETS statistical model",
               plotlyOutput("fc_plot", height = "500px")),
    p(class = "insight", "This is a statistical projection from past data, not financial advice.")
  ),

  # ---------------- About ----------------
  nav_panel("About",
    h2(class = "page-head", "About this project"),
    p(class = "page-sub",
      "Data: ECB euro foreign exchange reference rates (Kaggle file for 1999 onwards, ",
      "extended to today with the Frankfurter API). All rates are converted to rupees per ",
      "one unit of the foreign currency (yen per 100 yen)."),
    p(class = "page-sub", "Built in R with Shiny, bslib, plotly and forecast.")
  )
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------
server <- function(input, output, session) {

  fmt_date <- function(d) format(as.Date(d), "%d %b %Y")
  heading <- function(txt) h2(class = "page-head", txt)

  # ============ Overview ============
  summary_all <- long |>
    group_by(cur) |>
    summarise(first = first(rate), last = last(rate), pct = (last / first - 1) * 100, .groups = "drop")

  output$ov_head <- renderUI({
    usd <- summary_all |> filter(cur == "USD")
    heading(sprintf("A US dollar now costs %.0f%% more rupees than in %d",
                    usd$pct, year(min_d)))
  })

  output$ov_boxes <- renderUI({
    boxes <- lapply(names(cur_cols), function(k) {
      s <- summary_all |> filter(cur == k)
      value_box(title = cur_names[[k]],
                value = sprintf("%s%.2f", rupee, s$last),
                p(sprintf("%+.0f%% since %d", s$pct, year(min_d))))
    })
    layout_columns(col_widths = c(3, 3, 3, 3), !!!boxes)
  })

  output$ov_plot <- renderPlotly({
    d <- long |> group_by(cur) |> mutate(index = rate / first(rate) * 100) |> ungroup()
    plot_ly(d, x = ~date, y = ~index, color = ~cur, colors = cur_colors,
            type = "scatter", mode = "lines",
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.1f}<extra>%{fullData.name}</extra>") |>
      layout(hovermode = "x unified", xaxis = list(title = ""),
             yaxis = list(title = "Index (2000 = 100)"),
             legend = list(orientation = "h", y = 1.1))
  })

  # ============ Trend ============
  trend_data <- reactive({
    req(input$tr_cur, input$tr_dates)
    d <- long |>
      filter(cur %in% input$tr_cur, date >= input$tr_dates[1], date <= input$tr_dates[2])
    validate(need(nrow(d) > 1, "No data for this date range."))
    d <- d |> group_by(cur)
    if (input$tr_mode == "idx") {
      d <- d |> mutate(value = rate / first(rate) * 100)
    } else {
      d <- d |> mutate(value = rate)
    }
    ungroup(d)
  })

  trend_summary <- reactive({
    trend_data() |> group_by(cur) |>
      summarise(a = first(rate), b = last(rate), pct = (b / a - 1) * 100, .groups = "drop")
  })

  output$tr_head <- renderUI({
    s <- trend_summary() |> slice_max(abs(pct), n = 1, with_ties = FALSE)
    heading(sprintf("%s moved the most: %+.0f%% in rupee terms between %s and %s",
                    cur_names[[s$cur]], s$pct, fmt_date(input$tr_dates[1]), fmt_date(input$tr_dates[2])))
  })

  output$tr_sub <- renderUI({
    s <- trend_summary()
    txt <- paste(sprintf("%s went from %s%.2f to %s%.2f (%+.1f%%)",
                         s$cur, rupee, s$a, rupee, s$b, s$pct), collapse = "; ")
    p(class = "insight", paste0(txt, "."))
  })

  output$tr_plot <- renderPlotly({
    d <- trend_data()
    ylab <- if (input$tr_mode == "idx") "Index (start = 100)" else paste0("Rupees per unit (", rupee, ")")
    plot_ly(d, x = ~date, y = ~value, color = ~cur, colors = cur_colors,
            type = "scatter", mode = "lines",
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>%{fullData.name}</extra>") |>
      layout(hovermode = "x unified", xaxis = list(title = ""), yaxis = list(title = ylab),
             legend = list(orientation = "h", y = 1.1))
  })

  output$tr_table <- renderDT({
    d <- trend_data() |>
      select(date, cur, value) |>
      pivot_wider(names_from = cur, values_from = value) |>
      mutate(across(-date, ~ round(.x, 2))) |>
      arrange(desc(date))
    datatable(d, rownames = FALSE, options = list(pageLength = 10))
  })

  observeEvent(input$tr_reset, {
    updateCheckboxGroupInput(session, "tr_cur", selected = names(cur_cols))
    updateDateRangeInput(session, "tr_dates", start = min_d, end = max_d)
    updateRadioButtons(session, "tr_mode", selected = "idx")
  })

  # ============ Volatility ============
  vol_data <- reactive({
    req(input$vol_cur, input$vol_dates)
    w <- as.integer(input$vol_win)
    k <- if (isTRUE(input$vol_annual)) sqrt(252) else 1
    d <- long |>
      filter(cur %in% input$vol_cur) |>
      group_by(cur) |>
      mutate(vol = rollapplyr(ret, w, sd, fill = NA) * k) |>
      ungroup() |>
      filter(!is.na(vol), date >= input$vol_dates[1], date <= input$vol_dates[2])
    validate(need(nrow(d) > 1, "No data for this date range."))
    d
  })

  output$vol_head <- renderUI({
    d <- vol_data()
    top <- d |> slice_max(vol, n = 1, with_ties = FALSE)
    heading(sprintf("%s was most volatile around %s (%.1f%%)",
                    cur_names[[top$cur]], format(top$date, "%b %Y"), top$vol))
  })

  output$vol_plot <- renderPlotly({
    d <- vol_data()
    shapes <- list(); ann <- list()
    if (isTRUE(input$vol_events)) {
      shapes <- lapply(seq_len(nrow(events)), function(i)
        list(type = "rect", xref = "x", yref = "paper",
             x0 = as.character(events$from[i]), x1 = as.character(events$to[i]),
             y0 = 0, y1 = 1, fillcolor = "rgba(214,39,40,0.10)", line = list(width = 0)))
      ann <- lapply(seq_len(nrow(events)), function(i)
        list(x = as.character(events$from[i]), y = 1, xref = "x", yref = "paper",
             text = events$label[i], showarrow = FALSE, xanchor = "left",
             yanchor = "top", font = list(size = 10, color = "#a33")))
    }
    ylab <- if (isTRUE(input$vol_annual)) "Annualised volatility (%)" else "Daily volatility (%)"
    plot_ly(d, x = ~date, y = ~vol, color = ~cur, colors = cur_colors,
            type = "scatter", mode = "lines",
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}%<extra>%{fullData.name}</extra>") |>
      layout(hovermode = "x unified", xaxis = list(title = ""), yaxis = list(title = ylab),
             shapes = shapes, annotations = ann, legend = list(orientation = "h", y = 1.1))
  })

  output$vol_table <- renderDT({
    d <- vol_data() |>
      select(date, cur, vol) |>
      pivot_wider(names_from = cur, values_from = vol) |>
      mutate(across(-date, ~ round(.x, 2))) |>
      arrange(desc(date))
    datatable(d, rownames = FALSE, options = list(pageLength = 10))
  })

  observeEvent(input$vol_reset, {
    updateCheckboxGroupInput(session, "vol_cur", selected = c("USD", "EUR"))
    updateDateRangeInput(session, "vol_dates", start = min_d, end = max_d)
    updateSelectInput(session, "vol_win", selected = 30)
    updateCheckboxInput(session, "vol_annual", value = TRUE)
    updateCheckboxInput(session, "vol_events", value = TRUE)
  })

  # ============ Heatmap ============
  hm_data <- reactive({
    req(input$hm_cur, input$hm_years)
    long |>
      filter(cur == input$hm_cur, !is.na(ret),
             year(date) >= input$hm_years[1], year(date) <= input$hm_years[2]) |>
      mutate(year = year(date), month = factor(month.abb[month(date)], levels = month.abb)) |>
      group_by(year, month) |>
      summarise(vol = sd(ret), .groups = "drop") |>
      complete(year, month) |>
      arrange(year, month)
  })

  output$hm_head <- renderUI({
    d <- hm_data() |> filter(!is.na(vol))
    top <- d |> slice_max(vol, n = 1, with_ties = FALSE)
    heading(sprintf("%s's calmest and wildest months: the peak was %s %d",
                    input$hm_cur, as.character(top$month), top$year))
  })

  output$hm_plot <- renderPlotly({
    d <- hm_data()
    wide <- d |> pivot_wider(names_from = month, values_from = vol) |> arrange(year)
    z <- as.matrix(wide[, month.abb])
    plot_ly(x = month.abb, y = wide$year, z = z, type = "heatmap",reversescale = TRUE, colorscale = "YlOrRd",
            hovertemplate = "%{y} %{x}<br>Daily volatility: %{z:.2f}%<extra></extra>") |>
      layout(xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
             yaxis = list(title = "", autorange = "reversed", dtick = 2))
  })

  observeEvent(input$hm_reset, {
    updateSelectInput(session, "hm_cur", selected = "USD")
    updateSliderInput(session, "hm_years", value = c(year(min_d), year(max_d)))
  })

  # ============ World map ============
  map_data <- reactive({
    req(input$map_from, input$map_to, input$map_cur)
    validate(need(input$map_from < input$map_to, "Pick a 'From' date before the 'To' date."))
    a <- long |> filter(date >= input$map_from) |> group_by(cur) |>
      slice_min(date, n = 1, with_ties = FALSE) |> ungroup() |> select(cur, start = rate)
    b <- long |> filter(date <= input$map_to) |> group_by(cur) |>
      slice_max(date, n = 1, with_ties = FALSE) |> ungroup() |> select(cur, end = rate)
    chg <- inner_join(a, b, by = "cur") |> mutate(pct = (end / start - 1) * 100)
    map_base |>
      filter(cur %in% input$map_cur) |>
      left_join(chg, by = "cur") |>
      mutate(hover = paste0(place, " (", cur, ")<br>", rupee, round(start, 2), " to ",
                            rupee, round(end, 2), "<br>", sprintf("%+.1f%%", pct)))
  })

  output$map_head <- renderUI({
    d <- map_data() |> distinct(cur, pct) |> slice_max(pct, n = 1, with_ties = FALSE)
    heading(sprintf("The rupee fell most against the %s: %+.0f%% between %s and %s",
                    cur_names[[d$cur]], d$pct, fmt_date(input$map_from), fmt_date(input$map_to)))
  })

  output$map_plot <- renderPlotly({
    d <- map_data()
    plot_ly(d, type = "choropleth", locations = ~iso, z = ~pct, text = ~hover,
            hoverinfo = "text", colorscale = "RdBu", zmid = 0,
            colorbar = list(title = "% change")) |>
      layout(geo = list(projection = list(type = "natural earth"), showframe = FALSE,
                        showcountries = TRUE, countrycolor = "#cccccc",
                        bgcolor = "rgba(0,0,0,0)"))
  })

  observeEvent(input$map_reset, {
    updateDateInput(session, "map_from", value = min_d)
    updateDateInput(session, "map_to", value = max_d)
    updateCheckboxGroupInput(session, "map_cur", selected = names(cur_cols))
  })

  # ============ Candlestick ============
  cd_data <- reactive({
    req(input$cd_cur, input$cd_dates, input$cd_freq)
    unit <- c(Monthly = "month", Quarterly = "quarter", Yearly = "year")[[input$cd_freq]]
    d <- long |>
      filter(cur == input$cd_cur, date >= input$cd_dates[1], date <= input$cd_dates[2]) |>
      mutate(p = floor_date(date, unit)) |>
      group_by(p) |>
      summarise(open = first(rate), high = max(rate), low = min(rate),
                close = last(rate), .groups = "drop")
    validate(need(nrow(d) > 1, "No data for this date range."))
    d
  })

  output$cd_head <- renderUI({
    d <- cd_data()
    heading(sprintf("%s: from %s%.2f to %s%.2f across %d %s candles",
                    cur_names[[input$cd_cur]], rupee, first(d$open), rupee, last(d$close),
                    nrow(d), tolower(input$cd_freq)))
  })

  output$cd_plot <- renderPlotly({
    d <- cd_data()
    plot_ly(d, x = ~p, type = "candlestick", open = ~open, high = ~high, low = ~low, close = ~close,
            increasing = list(line = list(color = "#d62728")),
            decreasing = list(line = list(color = "#2ca02c"))) |>
      layout(xaxis = list(rangeslider = list(visible = TRUE), title = ""),
             yaxis = list(title = paste0("Rupees per unit (", rupee, ")")))
  })

  observeEvent(input$cd_reset, {
    updateSelectInput(session, "cd_cur", selected = "USD")
    updateRadioButtons(session, "cd_freq", selected = "Monthly")
    updateDateRangeInput(session, "cd_dates", start = min_d, end = max_d)
  })

  # ============ Forecast ============
  fc_res <- reactive({
    req(input$fc_cur, input$fc_h)
    h <- as.integer(input$fc_h)
    m <- long |>
      filter(cur == input$fc_cur) |>
      mutate(m = floor_date(date, "month")) |>
      group_by(m) |>
      summarise(v = mean(rate), .groups = "drop")
    tsd <- ts(m$v, start = c(year(min(m$m)), month(min(m$m))), frequency = 12)
    fc <- forecast(ets(tsd), h = h, level = c(80, 95))
    fut <- seq(max(m$m), by = "month", length.out = h + 1)[-1]
    list(hist = m,
         fc = tibble(date = fut, mean = as.numeric(fc$mean),
                     lo80 = as.numeric(fc$lower[, 1]), hi80 = as.numeric(fc$upper[, 1]),
                     lo95 = as.numeric(fc$lower[, 2]), hi95 = as.numeric(fc$upper[, 2])))
  })

  output$fc_head <- renderUI({
    r <- fc_res(); f <- r$fc
    heading(sprintf("Rupee per %s could be near %s%.2f by %s (95%% range %s%.2f to %s%.2f)",
                    input$fc_cur, rupee, last(f$mean), format(last(f$date), "%b %Y"),
                    rupee, last(f$lo95), rupee, last(f$hi95)))
  })

  output$fc_plot <- renderPlotly({
    r <- fc_res(); f <- r$fc; hist <- r$hist
    yrs <- as.integer(input$fc_hist)
    if (yrs > 0) hist <- hist |> filter(m >= max(m) - years(yrs))
    p <- plot_ly() |>
      add_lines(data = hist, x = ~m, y = ~v, name = "History",
                line = list(color = "#2b6fd6"))
    if ("95" %in% input$fc_bands)
      p <- p |> add_ribbons(data = f, x = ~date, ymin = ~lo95, ymax = ~hi95, name = "95% range",
                            fillcolor = "rgba(228,87,46,0.18)", line = list(width = 0))
    if ("80" %in% input$fc_bands)
      p <- p |> add_ribbons(data = f, x = ~date, ymin = ~lo80, ymax = ~hi80, name = "80% range",
                            fillcolor = "rgba(228,87,46,0.35)", line = list(width = 0))
    p |> add_lines(data = f, x = ~date, y = ~mean, name = "Forecast",
                   line = list(color = "#e4572e", dash = "dash")) |>
      layout(hovermode = "x unified", xaxis = list(title = ""),
             yaxis = list(title = paste0("Rupees per unit (", rupee, ")")),
             legend = list(orientation = "h", y = 1.1))
  })

  observeEvent(input$fc_reset, {
    updateSelectInput(session, "fc_cur", selected = "USD")
    updateSelectInput(session, "fc_hist", selected = 10)
    updateSliderInput(session, "fc_h", value = 6)
    updateCheckboxGroupInput(session, "fc_bands", selected = c("80", "95"))
  })
}

shinyApp(ui, server)
