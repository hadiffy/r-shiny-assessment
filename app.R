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

col_labels <- c("Loss Year", "Development Year", "Amount of Claims Paid ($)")

# start the app with an empty table
empty_claims <- tibble(
  `Loss Year` = NA_real_,
  `Development Year` = NA_real_,
  `Amount of Claims Paid ($)` = NA_real_
)

# sample data from the assignment spreadsheet
sample_claims <- tibble(
  `Loss Year` = c(2017, 2017, 2017, 2018, 2018, 2019),
  `Development Year` = c(1, 2, 3, 1, 2, 1),
  `Amount of Claims Paid ($)` = c(524792, 218265, 2225, 798502, 197157, 917636)
)

# build the empty triangle from a year range: oldest year has the most dev
# years, each later year one fewer (2017-2019 -> 3, 2, 1 dev years)
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

# map an uploaded file's columns onto the 3 we need
tidy_upload <- function(df) {
  nm <- tolower(names(df))
  loss_col <- which(grepl("loss", nm))[1]
  dev_col <- which(grepl("dev", nm))[1]
  paid_col <- which(grepl("paid|claim|amount", nm))[1]

  # headers not recognised -> just use the first 3 columns
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

# chain-ladder: incremental triangle -> projected cumulative square.
# last development step uses the tail factor
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

  # incremental amounts as a loss year x dev year matrix
  inc <- matrix(
    NA_real_, nrow = length(loss_years), ncol = n_obs,
    dimnames = list(loss_years, seq_len(n_obs))
  )
  inc[cbind(match(claims$loss_year, loss_years), claims$dev_year)] <- claims$paid

  # cumulate the known part of each row
  cum <- matrix(
    NA_real_, nrow = length(loss_years), ncol = n_dev,
    dimnames = list(loss_years, seq_len(n_dev))
  )
  for (i in seq_along(loss_years)) {
    known <- which(!is.na(inc[i, ]))
    cum[i, known] <- cumsum(inc[i, known])
  }

  # volume-weighted development factors + tail
  factors <- numeric(n_dev - 1)
  for (j in seq_len(n_obs - 1)) {
    rows <- which(!is.na(cum[, j + 1]))
    factors[j] <- sum(cum[rows, j + 1]) / sum(cum[rows, j])
  }
  factors[n_dev - 1] <- tail_factor

  # project the lower triangle + tail column, left to right
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
  # the grid is the single source of truth for the input data
  claims <- reactiveVal(empty_claims)

  output$grid <- renderRHandsontable({
    rhandsontable(claims(), rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Loss Year", format = "0") %>%
      hot_col("Development Year", format = "0") %>%
      hot_col("Amount of Claims Paid ($)", format = "0,0")
  })

  # keep claims() in sync with manual edits
  observeEvent(input$grid, {
    claims(hot_to_r(input$grid))
  })

  # an upload just loads its contents into the grid
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

  # add row button (right-click menu can't add once the table is empty)
  observeEvent(input$add_row, {
    claims(bind_rows(claims(), empty_claims))
  })

  observeEvent(input$clear, {
    claims(empty_claims)
  })

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
