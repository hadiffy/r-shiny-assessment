# Cumulative Paid Claims - R Shiny app
#
# Lets the user enter a claims-paid triangle by hand or upload it from a
# CSV/Excel file, pick a tail factor, and then projects the cumulative paid
# claims using the basic chain-ladder method. Results are shown as a table
# and a chart.

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)
library(readr)
library(readxl)
library(rhandsontable)
library(DT)

# Column labels used in the editable grid (kept human-friendly on purpose).
col_labels <- c("Loss Year", "Development Year", "Amount of Claims Paid ($)")

# A single blank row, so the app opens with a clean, empty table.
empty_claims <- tibble(
  `Loss Year` = NA_real_,
  `Development Year` = NA_real_,
  `Amount of Claims Paid ($)` = NA_real_
)

# Sample data taken from the assignment spreadsheet.
sample_claims <- tibble(
  `Loss Year` = c(2017, 2017, 2017, 2018, 2018, 2019),
  `Development Year` = c(1, 2, 3, 1, 2, 1),
  `Amount of Claims Paid ($)` = c(524792, 218265, 2225, 798502, 197157, 917636)
)

# Build the empty run-off triangle from a range of loss years. The oldest year
# gets the most development years and each later year gets one fewer, e.g.
# 2017-2019 -> 2017 has 3 dev years, 2018 has 2, 2019 has 1. The user then only
# needs to fill in the claim amounts.
build_skeleton <- function(start_year, end_year) {
  years <- start_year:end_year
  n <- length(years)

  bind_rows(lapply(seq_along(years), function(i) {
    tibble(
      `Loss Year` = years[i],
      `Development Year` = seq_len(n - (i - 1)),
      `Amount of Claims Paid ($)` = NA_real_
    )
  }))
}

# Rename whatever columns an uploaded file has into the three we expect.
# We match on key words so a file exported straight from Excel still works.
tidy_upload <- function(df) {
  nm <- tolower(names(df))
  loss_col <- which(grepl("loss", nm))[1]
  dev_col <- which(grepl("dev", nm))[1]
  paid_col <- which(grepl("paid|claim|amount", nm))[1]

  # If the headers are not recognised, fall back to the first three columns.
  if (anyNA(c(loss_col, dev_col, paid_col))) {
    loss_col <- 1
    dev_col <- 2
    paid_col <- 3
  }

  df %>%
    select(all_of(c(loss_col, dev_col, paid_col))) %>%
    setNames(col_labels) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(`Loss Year`), !is.na(`Development Year`),
           !is.na(`Amount of Claims Paid ($)`))
}

# Core calculation: incremental triangle -> cumulative square via chain-ladder.
# The last development step uses the user-supplied tail factor.
build_cumulative <- function(claims, tail_factor) {
  claims <- claims %>%
    setNames(c("loss_year", "dev_year", "paid")) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(loss_year), !is.na(dev_year), !is.na(paid)) %>%
    arrange(loss_year, dev_year)

  validate(need(nrow(claims) > 0, "Enter or upload some claims data first."))

  loss_years <- sort(unique(claims$loss_year))
  n_obs <- max(claims$dev_year)   # development years we actually observe
  n_dev <- n_obs + 1              # one extra column projected with the tail

  # Incremental amounts laid out as a loss-year x development-year matrix.
  inc <- matrix(
    NA_real_, nrow = length(loss_years), ncol = n_obs,
    dimnames = list(loss_years, seq_len(n_obs))
  )
  inc[cbind(match(claims$loss_year, loss_years), claims$dev_year)] <- claims$paid

  # Cumulate the known (upper-left) part of each row.
  cum <- matrix(
    NA_real_, nrow = length(loss_years), ncol = n_dev,
    dimnames = list(loss_years, seq_len(n_dev))
  )
  for (i in seq_along(loss_years)) {
    known <- which(!is.na(inc[i, ]))
    cum[i, known] <- cumsum(inc[i, known])
  }

  # Volume-weighted development factors, with the tail factor tacked on.
  factors <- numeric(n_dev - 1)
  for (j in seq_len(n_obs - 1)) {
    rows <- which(!is.na(cum[, j + 1]))
    factors[j] <- sum(cum[rows, j + 1]) / sum(cum[rows, j])
  }
  factors[n_dev - 1] <- tail_factor

  # Fill the lower triangle and the tail column, left to right.
  for (j in 2:n_dev) {
    gaps <- which(is.na(cum[, j]))
    cum[gaps, j] <- cum[gaps, j - 1] * factors[j - 1]
  }

  as.data.frame(cum, check.names = FALSE) %>%
    rownames_to_column("Loss Year") %>%
    mutate(`Loss Year` = as.integer(`Loss Year`)) %>%
    pivot_longer(
      -`Loss Year`, names_to = "Development Year", values_to = "Cumulative"
    ) %>%
    mutate(`Development Year` = as.integer(`Development Year`))
}

ui <- page_sidebar(
  title = "Cumulative Paid Claims",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 320,
    h5("Input Parameter"),
    numericInput(
      "tail_factor", "Tail factor",
      value = 1.1, min = 1, step = 0.05
    ),
    hr(),
    h5("Set up table"),
    helpText(
      "Enter the first and last loss year and the app builds the triangle",
      "for you, so you only need to type in the claim amounts."
    ),
    numericInput("start_year", "Start year", value = 2017, step = 1),
    numericInput("end_year", "End year", value = 2019, step = 1),
    actionButton("generate", "Generate table", class = "btn-primary btn-sm"),
    hr(),
    h5("Load from a file"),
    helpText(
      "Optional. Upload a CSV or Excel file with columns",
      "Loss Year, Development Year and Amount of Claims Paid ($).",
      "It will fill the table on the right, which you can still edit."
    ),
    fileInput(
      "file", NULL,
      accept = c(".csv", ".xls", ".xlsx"),
      buttonLabel = "Browse..."
    ),
    actionButton("load_sample", "Insert sample data", class = "btn-outline-secondary btn-sm")
  ),
  navset_card_tab(
    nav_panel(
      "Claims Data",
      p("Enter the amount of claims paid for each loss year and",
        "development year. Edit cells directly, or right-click to add rows."),
      actionButton(
        "add_row", "Add row",
        class = "btn-outline-secondary btn-sm mb-2"
      ),
      actionButton(
        "clear", "Clear table",
        class = "btn-outline-secondary btn-sm mb-2"
      ),
      rHandsontableOutput("grid")
    ),
    nav_panel(
      "Cumulative Paid Claims ($)",
      DTOutput("cum_table")
    ),
    nav_panel(
      "Plot",
      plotOutput("cum_plot", height = 460)
    )
  )
)

server <- function(input, output, session) {
  # The editable grid is the single source of truth for the input data.
  # Start empty so the table is clean when the app opens.
  claims <- reactiveVal(empty_claims)

  output$grid <- renderRHandsontable({
    rhandsontable(claims(), rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Loss Year", format = "0") %>%
      hot_col("Development Year", format = "0") %>%
      hot_col("Amount of Claims Paid ($)", format = "0,0")
  })

  # Keep the reactive value in sync with manual edits.
  observeEvent(input$grid, {
    claims(hot_to_r(input$grid))
  })

  # A file upload simply loads its contents into the grid.
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    raw <- switch(
      tolower(ext),
      csv = read_csv(input$file$datapath, show_col_types = FALSE),
      xls = read_excel(input$file$datapath),
      xlsx = read_excel(input$file$datapath),
      validate("Please upload a .csv, .xls or .xlsx file.")
    )
    claims(tidy_upload(raw))
  })

  observeEvent(input$load_sample, {
    claims(sample_claims)
  })

  # Append a blank row. Works even when the table has been emptied, which
  # the right-click menu cannot do once the last row is gone.
  observeEvent(input$add_row, {
    claims(bind_rows(claims(), empty_claims))
  })

  # Reset the table back to a single blank row.
  observeEvent(input$clear, {
    claims(empty_claims)
  })

  # Build the triangle skeleton from the chosen range of loss years.
  observeEvent(input$generate, {
    if (anyNA(c(input$start_year, input$end_year)) ||
        input$end_year < input$start_year) {
      showNotification(
        "End year must be the same as or after the start year.",
        type = "error"
      )
      return()
    }
    claims(build_skeleton(input$start_year, input$end_year))
  })

  cumulative <- reactive({
    build_cumulative(claims(), input$tail_factor)
  })

  # Wide layout (loss year x development year) for display.
  cumulative_wide <- reactive({
    cumulative() %>%
      pivot_wider(
        names_from = `Development Year`, values_from = Cumulative
      ) %>%
      arrange(`Loss Year`)
  })

  output$cum_table <- renderDT({
    df <- cumulative_wide()
    value_cols <- setdiff(names(df), "Loss Year")
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      caption = "Cumulative Paid Claims ($)"
    ) %>%
      formatRound(value_cols, digits = 0)
  })

  output$cum_plot <- renderPlot({
    df <- cumulative() %>%
      mutate(`Loss Year` = factor(`Loss Year`))

    ggplot(df, aes(`Development Year`, Cumulative, colour = `Loss Year`)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      geom_text(
        aes(label = comma(round(Cumulative))),
        vjust = -0.9, size = 3.2, show.legend = FALSE
      ) +
      scale_x_continuous(breaks = sort(unique(df$`Development Year`))) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Cumulative Paid Claims ($)",
        x = "Development Year", y = "Cumulative Paid Claims ($)",
        colour = "Loss Year"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
