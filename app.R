#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# 1) Packages --------------------------------------------------------
library(shiny)
library(tidyverse)    # readr, dplyr, ggplot2, tidyr, tibble, etc.
library(DT)           # interactive tables
library(shinythemes)  # simple theming

# 2) Helpers from Part 2 --------------------------------------------
# These functions detect numeric/categorical/datetime columns, validate columns,
# and perform light coercion. If the file is missing, we provide safe fallbacks.
if (file.exists("scripts/utils_data.R")) source("scripts/utils_data.R")

# Fallbacks so this app still runs if utils_data.R is absent:
if (!exists("numeric_cols"))    numeric_cols    <- function(df) names(df)[sapply(df, is.numeric)]
if (!exists("is_categorical"))  is_categorical  <- function(x) is.character(x) || is.factor(x) || is.logical(x)
if (!exists("categorical_cols"))categorical_cols<- function(df) names(df)[sapply(df, is_categorical)]
if (!exists("describe_df")) {
  describe_df <- function(df) tibble::tibble(
    column   = names(df),
    class    = vapply(df, \(x) paste(class(x), collapse = "/"), character(1)),
    n_na     = vapply(df, \(x) sum(is.na(x)), integer(1)),
    n_unique = vapply(df, \(x) dplyr::n_distinct(x), integer(1))
  )
}
if (!exists("coerce_types"))    coerce_types    <- function(df) df  # no-op fallback

# 3) Safe CSV read helper -------------------------------------------
# We centralise reading so the server code stays clear and errors become messages.
safe_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# 4) UI --------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application theme
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("HackVis — CSV to Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # 4a) Data source: upload with fallback
          fileInput(
            inputId = "csv",
            label   = "Upload a CSV file",
            accept  = c(".csv")
          ),
          helpText("No file? The app will use data/sample_projects.csv automatically."),
          
          # 4b) Chart selection: the user chooses one of three
          selectInput(
            inputId = "chart_type",
            label   = "Chart type",
            choices = c("Histogram / Density" = "hist",
                        "Boxplot (Y numeric by X category)" = "box",
                        "Scatter (X numeric vs Y numeric)"   = "scatter")
          ),
          
          # 4c) Dynamic UI placeholders (server will populate these)
          uiOutput("column_ui"),  # x/y/colour selectors based on chart type + data
          uiOutput("filter_ui"),  # auto-generated filters (numeric sliders, categorical multi-selects)
          
          # 4d) Download of the filtered slice
          downloadButton("download_filtered", "Download filtered CSV")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotOutput("plot", height = "460px")),
            tabPanel("Table", DTOutput("table")),
            tabPanel("Summary", verbatimTextOutput("summary"))
          )
        )
    )
)

# 5) Server ----------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # 5a) Reactive data source: uploaded CSV OR fallback sample -------
  # This is the single source of truth for the rest of the app.
  raw_data <- reactive({
    # If the user uploaded a file during this session, use it
    if (!is.null(input$csv) && isTruthy(input$csv$datapath)) {
      df <- safe_read_csv(input$csv$datapath)
    } else {
      # Otherwise, fall back to the bundled sample
      df <- safe_read_csv("data/sample_projects.csv")
    }
    # Light coercion/normalisation to make downstream logic predictable
    coerce_types(df)
  })
  
  # 5b) Column type detection (drives UI and chart validity) --------
  numeric_choices <- reactive({
    df <- raw_data()
    numeric_cols(df)
  })
  categorical_choices <- reactive({
    df <- raw_data()
    categorical_cols(df)
  })
  
  # 5c) Dynamic column selectors ------------------------------------
  # We build selectors differently depending on the chosen chart.
  output$column_ui <- renderUI({
    df_num <- numeric_choices()
    df_cat <- categorical_choices()
    
    # Common options: empty "" indicates "none"/optional
    empty       <- ""
    cat_choices <- if (length(df_cat)) c(empty, df_cat) else empty
    
    # Per chart type, render the relevant inputs
    switch(input$chart_type,
           "hist" = {
             # Histogram / density requires one numeric (x)
             tagList(
               selectInput("xcol", "Numeric column (X axis)", choices = df_num, selected = dplyr::first(df_num)),
               checkboxInput("show_density", "Overlay density curve", value = FALSE),
               sliderInput("bins", "Number of bins", min = 5, max = 60, value = 30, step = 1)
             )
           },
           "box" = {
             # Boxplot requires: numeric Y by categorical X
             tagList(
               selectInput("xcat", "Categorical column (X / groups)", choices = df_cat, selected = dplyr::first(df_cat)),
               selectInput("ycol", "Numeric column (Y)", choices = df_num, selected = dplyr::first(df_num)),
               checkboxInput("show_points", "Jitter points", value = TRUE)
             )
           },
           "scatter" = {
             # Scatter requires: numeric X and numeric Y; optional colour by category
             tagList(
               selectInput("xcol", "Numeric X", choices = df_num, selected = dplyr::first(df_num)),
               selectInput("ycol", "Numeric Y", choices = df_num, selected = dplyr::nth(df_num, 2, default = dplyr::first(df_num))),
               selectInput("colourby", "Colour by (optional category)", choices = cat_choices, selected = empty),
               sliderInput("alpha", "Point transparency (alpha)", min = 0.2, max = 1.0, value = 0.7, step = 0.1)
             )
           }
    )
  })
  
  # 5d) Auto-generated filters for ALL columns ----------------------
  # Numeric -> range slider; Categorical -> multi-select of unique values (capped for sanity)
  output$filter_ui <- renderUI({
    df <- raw_data()
    ui_list <- list()
    for (nm in names(df)) {
      col <- df[[nm]]
      input_id <- paste0("f_", nm)  # filter input IDs are "f_<colname>"
      
      if (is.numeric(col)) {
        rng <- range(col, na.rm = TRUE)
        # Handle constant vectors gracefully (min == max)
        if (is.finite(rng[1]) && is.finite(rng[2])) {
          if (identical(rng[1], rng[2])) rng <- c(rng[1] - 0.5, rng[2] + 0.5)
          ui_list[[nm]] <- sliderInput(
            inputId = input_id,
            label   = paste("Filter numeric:", nm),
            min     = rng[1],
            max     = rng[2],
            value   = rng,
            step    = (rng[2] - rng[1]) / 100
          )
        }
      } else {
        # Treat character/factor/logical as categorical
        choices <- unique(as.character(col))
        choices <- sort(choices[!is.na(choices)])
        if (length(choices) > 200) choices <- c(head(choices, 199), "(others omitted)")
        # Default: select all observed levels (so filter is non-restrictive initially)
        ui_list[[nm]] <- selectInput(
          inputId = input_id,
          label   = paste("Filter categorical:", nm),
          choices = choices,
          selected = choices,
          multiple = TRUE
        )
      }
    }
    do.call(tagList, ui_list)
  })
  
  # 5e) Apply filters to the current data ---------------------------
  filtered_data <- reactive({
    df <- raw_data()
    # Iterate all columns; for each, look for a corresponding filter input "f_<name>"
    for (nm in names(df)) {
      id <- paste0("f_", nm)
      val <- input[[id]]
      col <- df[[nm]]
      
      # Skip if the UI for this column hasn't been created yet
      if (is.null(val)) next
      
      if (is.numeric(col)) {
        # Range slider returns c(min, max)
        df <- df |> dplyr::filter(.data[[nm]] >= val[1], .data[[nm]] <= val[2])
      } else {
        # Multi-select of allowed values; rows with NA drop out naturally
        df <- df |> dplyr::filter(as.character(.data[[nm]]) %in% val)
      }
    }
    df
  })
  
  # 5f) Plot based on selected chart type ---------------------------
  output$plot <- renderPlot({
    req(input$chart_type)
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No rows after filters. Widen filters or upload a different CSV."))
    
    if (input$chart_type == "hist") {
      # Histogram/density: needs a numeric xcol and a bin count
      req(input$xcol, input$bins)
      validate(need(is.numeric(df[[input$xcol]]), "Selected column is not numeric."))
      
      ggplot(df, aes(x = .data[[input$xcol]])) +
        geom_histogram(bins = input$bins) +
        {if (isTRUE(input$show_density)) geom_density(aes(y = after_stat(..count..)), linewidth = 0.8)} +
        labs(title = paste("Histogram of", input$xcol),
             x = input$xcol, y = "Count") +
        theme_minimal()
      
    } else if (input$chart_type == "box") {
      # Boxplot: numeric Y by categorical X
      req(input$xcat, input$ycol)
      validate(
        need(is.numeric(df[[input$ycol]]), "Y must be numeric."),
        need(!is.numeric(df[[input$xcat]]), "X must be categorical (character/factor/logical).")
      )
      
      p <- ggplot(df, aes(x = .data[[input$xcat]], y = .data[[input$ycol]])) +
        geom_boxplot() +
        labs(title = paste("Boxplot:", input$ycol, "by", input$xcat),
             x = input$xcat, y = input$ycol) +
        theme_minimal()
      if (isTRUE(input$show_points)) {
        # Jitter to reveal overlapping points
        p <- p + geom_jitter(width = 0.2, alpha = 0.5)
      }
      p
      
    } else {
      # Scatter: numeric X vs numeric Y; optional colour by category
      req(input$xcol, input$ycol)
      validate(
        need(is.numeric(df[[input$xcol]]), "X must be numeric."),
        need(is.numeric(df[[input$ycol]]), "Y must be numeric.")
      )
      
      aes_map <- aes(x = .data[[input$xcol]], y = .data[[input$ycol]])
      # Add colour aesthetic if a category was chosen
      if (!is.null(input$colourby) && nzchar(input$colourby)) {
        validate(need(!is.numeric(df[[input$colourby]]), "Colour-by column must be categorical."))
        aes_map <- modifyList(aes_map, aes(colour = .data[[input$colourby]]))
      }
      
      ggplot(df, aes_map) +
        geom_point(alpha = input$alpha) +
        labs(title = paste("Scatter:", input$xcol, "vs", input$ycol),
             x = input$xcol, y = input$ycol,
             colour = if (!is.null(input$colourby) && nzchar(input$colourby)) input$colourby else NULL) +
        theme_minimal()
    }
  })
  
  # 5g) Interactive table and summary --------------------------------
  output$table <- renderDT({
    DT::datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$summary <- renderPrint({
    df <- filtered_data()
    list(
      dimensions = dim(df),
      classes = sapply(df, class),
      preview  = utils::head(df, 5),
      describe = utils::head(describe_df(df), 10)
    )
  })
  
  # 5h) Download of the filtered slice --------------------------------
  output$download_filtered <- downloadHandler(
    filename = function() paste0("hackvis_filtered_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(filtered_data(), file)
  )
}

# 6) Launch ----------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
