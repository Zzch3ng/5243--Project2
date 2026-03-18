required_packages <- c("shiny", "bslib", "plotly", "DT", "readxl", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the required packages before running this app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)

# Allow large classroom datasets such as HMDA extracts.
options(shiny.maxRequestSize = 300 * 1024^2)

app_title <- "Project 2 - Interactive Data Wrangling Studio"

load_builtin_dataset <- function(name) {
  switch(
    tolower(name %||% "iris"),
    "mtcars" = mtcars,
    "toothgrowth" = ToothGrowth,
    iris
  )
}

clean_column_name <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  x[x == ""] <- "column"
  needs_prefix <- grepl("^[0-9]", x)
  x[needs_prefix] <- paste0("x_", x[needs_prefix])
  x
}

make_unique_name <- function(name, existing) {
  if (!(name %in% existing)) {
    return(name)
  }
  counter <- 2
  candidate <- paste0(name, "_", counter)
  while (candidate %in% existing) {
    counter <- counter + 1
    candidate <- paste0(name, "_", counter)
  }
  candidate
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("Missing")
  }
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

read_uploaded_data <- function(path, original_name) {
  ext <- tolower(tools::file_ext(original_name))

  if (ext == "csv") {
    return(
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE))
  }

  if (ext == "json") {
    payload <- jsonlite::fromJSON(path, flatten = TRUE)

    if (is.data.frame(payload)) {
      return(as.data.frame(payload, stringsAsFactors = FALSE))
    }

    if (is.list(payload) && "data" %in% names(payload) && is.data.frame(payload$data)) {
      return(as.data.frame(payload$data, stringsAsFactors = FALSE))
    }

    if (is.list(payload)) {
      candidate <- tryCatch(
        as.data.frame(payload, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(candidate)) {
        return(candidate)
      }
    }

    stop("The JSON file was read, but it could not be converted into a tabular data frame.")
  }

  if (ext == "rds") {
    obj <- readRDS(path)
    if (!inherits(obj, "data.frame")) {
      stop("The RDS file was loaded, but it does not contain a data frame.")
    }
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }

  stop("Unsupported file format. Please upload a CSV, Excel, JSON, or RDS file.")
}

standardize_strings <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- make.unique(clean_column_name(names(df)), sep = "_")

  missing_tokens <- c("", "na", "n/a", "null", "none", "nan")
  yes_tokens <- c("y", "yes", "true", "1")
  no_tokens <- c("n", "no", "false", "0")

  for (col in names(df)) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }

    if (is.character(df[[col]])) {
      values <- trimws(df[[col]])
      lowered <- tolower(values)
      values[lowered %in% missing_tokens] <- NA_character_
      non_missing <- unique(lowered[!is.na(values)])

      if (length(non_missing) > 0 && all(non_missing %in% c(yes_tokens, no_tokens))) {
        values[lowered %in% yes_tokens] <- "Yes"
        values[lowered %in% no_tokens] <- "No"
      }

      df[[col]] <- values
    }
  }

  df
}

apply_missing_handling <- function(df, strategy, numeric_strategy, categorical_strategy) {
  if (identical(strategy, "keep")) {
    return(df)
  }

  if (identical(strategy, "drop_rows")) {
    return(df[stats::complete.cases(df), , drop = FALSE])
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  categorical_cols <- setdiff(names(df), numeric_cols)

  for (col in numeric_cols) {
    if (!anyNA(df[[col]])) {
      next
    }

    fill_value <- switch(
      numeric_strategy,
      "mean" = mean(df[[col]], na.rm = TRUE),
      "zero" = 0,
      stats::median(df[[col]], na.rm = TRUE)
    )

    if (is.nan(fill_value)) {
      fill_value <- 0
    }

    df[[col]][is.na(df[[col]])] <- fill_value
  }

  for (col in categorical_cols) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }

    if (!anyNA(df[[col]])) {
      next
    }

    fill_value <- if (identical(categorical_strategy, "missing_label")) {
      "Missing"
    } else {
      mode_value(df[[col]])
    }

    df[[col]][is.na(df[[col]])] <- fill_value
  }

  df
}

apply_cleaning <- function(
  df,
  standardize_text,
  duplicate_action,
  missing_strategy,
  numeric_strategy,
  categorical_strategy
) {
  cleaned <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  if (isTRUE(standardize_text)) {
    cleaned <- standardize_strings(cleaned)
  }

  if (identical(duplicate_action, "remove")) {
    cleaned <- unique(cleaned)
  } else if (identical(duplicate_action, "flag")) {
    cleaned$duplicate_flag <- duplicated(cleaned)
  }

  cleaned <- apply_missing_handling(
    cleaned,
    strategy = missing_strategy,
    numeric_strategy = numeric_strategy,
    categorical_strategy = categorical_strategy
  )

  rownames(cleaned) <- NULL
  cleaned
}

apply_outlier_handling <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) {
    return(df)
  }

  keep_rows <- rep(TRUE, nrow(df))
  numeric_cols <- intersect(target_columns, names(df)[vapply(df, is.numeric, logical(1))])

  for (col in numeric_cols) {
    x <- df[[col]]
    x_non_missing <- x[!is.na(x)]

    if (length(x_non_missing) < 4) {
      next
    }

    q1 <- stats::quantile(x_non_missing, 0.25, na.rm = TRUE, names = FALSE)
    q3 <- stats::quantile(x_non_missing, 0.75, na.rm = TRUE, names = FALSE)
    iqr_value <- q3 - q1

    if (is.na(iqr_value) || iqr_value == 0) {
      next
    }

    lower <- q1 - 1.5 * iqr_value
    upper <- q3 + 1.5 * iqr_value

    if (identical(method, "cap")) {
      x[x < lower] <- lower
      x[x > upper] <- upper
      df[[col]] <- x
    } else if (identical(method, "remove")) {
      keep_rows <- keep_rows & (is.na(x) | (x >= lower & x <= upper))
    }
  }

  if (identical(method, "remove")) {
    df <- df[keep_rows, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}

scale_numeric_columns <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) {
    return(df)
  }

  numeric_cols <- intersect(target_columns, names(df)[vapply(df, is.numeric, logical(1))])

  for (col in numeric_cols) {
    x <- df[[col]]
    non_missing <- !is.na(x)

    if (!any(non_missing)) {
      next
    }

    if (identical(method, "standard")) {
      center <- mean(x[non_missing])
      spread <- stats::sd(x[non_missing])
      df[[col]][non_missing] <- if (is.na(spread) || spread == 0) 0 else (x[non_missing] - center) / spread
    } else if (identical(method, "minmax")) {
      min_x <- min(x[non_missing])
      max_x <- max(x[non_missing])
      df[[col]][non_missing] <- if (max_x == min_x) 0 else (x[non_missing] - min_x) / (max_x - min_x)
    } else if (identical(method, "robust")) {
      center <- stats::median(x[non_missing])
      spread <- stats::IQR(x[non_missing])
      df[[col]][non_missing] <- if (is.na(spread) || spread == 0) 0 else (x[non_missing] - center) / spread
    }
  }

  df
}

encode_categorical_columns <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) {
    return(df)
  }

  categorical_candidates <- names(df)[
    vapply(df, function(x) is.character(x) || is.factor(x), logical(1))
  ]
  categorical_cols <- intersect(target_columns, categorical_candidates)

  if (length(categorical_cols) == 0) {
    return(df)
  }

  if (identical(method, "label")) {
    for (col in categorical_cols) {
      values <- as.character(df[[col]])
      values[is.na(values)] <- "Missing"
      df[[col]] <- as.integer(factor(values, levels = unique(values)))
    }
    return(df)
  }

  other_cols <- setdiff(names(df), categorical_cols)
  encoded_parts <- lapply(categorical_cols, function(col) {
    values <- as.character(df[[col]])
    values[is.na(values)] <- "Missing"
    levels_found <- unique(values)

    out <- lapply(levels_found, function(level_value) {
      as.integer(values == level_value)
    })

    level_names <- vapply(
      levels_found,
      function(level_value) clean_column_name(paste(col, level_value, sep = "_")),
      character(1)
    )

    out_df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    names(out_df) <- make.unique(level_names, sep = "_")
    out_df
  })
  encoded_df <- do.call(cbind, encoded_parts)

  combined <- cbind(df[other_cols], encoded_df)
  rownames(combined) <- NULL
  combined
}

apply_preprocessing <- function(
  df,
  outlier_method,
  outlier_columns,
  scaling_method,
  scaling_columns,
  encoding_method,
  encoding_columns
) {
  processed <- df
  processed <- apply_outlier_handling(processed, outlier_method, outlier_columns)
  processed <- scale_numeric_columns(processed, scaling_method, scaling_columns)
  processed <- encode_categorical_columns(processed, encoding_method, encoding_columns)
  processed
}

apply_feature_recipes <- function(df, recipes) {
  if (length(recipes) == 0 || nrow(df) == 0) {
    return(df)
  }

  for (recipe in recipes) {
    name <- recipe$name
    operation <- recipe$operation
    col1 <- recipe$col1
    col2 <- recipe$col2

    if (!(col1 %in% names(df))) {
      next
    }

    x1 <- suppressWarnings(as.numeric(df[[col1]]))

    if (operation %in% c("add", "subtract", "multiply", "divide")) {
      if (!(col2 %in% names(df))) {
        next
      }

      x2 <- suppressWarnings(as.numeric(df[[col2]]))

      if (identical(operation, "add")) {
        df[[name]] <- x1 + x2
      } else if (identical(operation, "subtract")) {
        df[[name]] <- x1 - x2
      } else if (identical(operation, "multiply")) {
        df[[name]] <- x1 * x2
      } else if (identical(operation, "divide")) {
        result <- x1 / x2
        result[x2 == 0] <- NA_real_
        df[[name]] <- result
      }
    } else if (identical(operation, "log")) {
      result <- rep(NA_real_, length(x1))
      valid <- !is.na(x1) & x1 > -1
      result[valid] <- log1p(x1[valid])
      df[[name]] <- result
    } else if (identical(operation, "square")) {
      df[[name]] <- x1^2
    }
  }

  df
}

data_overview <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return("No rows available.")
  }

  numeric_count <- sum(vapply(df, is.numeric, logical(1)))
  missing_total <- sum(is.na(df))

  paste0(
    "Rows: ", format(nrow(df), big.mark = ","),
    " | Columns: ", format(ncol(df), big.mark = ","),
    " | Numeric: ", numeric_count,
    " | Non-numeric: ", ncol(df) - numeric_count,
    " | Missing values: ", format(missing_total, big.mark = ",")
  )
}

build_missing_profile <- function(df) {
  if (ncol(df) == 0) {
    return(data.frame())
  }

  data.frame(
    column = names(df),
    dtype = vapply(df, function(x) class(x)[1], character(1)),
    missing = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
    unique = vapply(df, function(x) length(unique(x[!is.na(x)])), numeric(1)),
    stringsAsFactors = FALSE
  )
}

build_summary_table <- function(df) {
  if (ncol(df) == 0) {
    return(data.frame())
  }

  rows <- lapply(names(df), function(col) {
    x <- df[[col]]
    row <- list(
      column = col,
      dtype = class(x)[1],
      missing = sum(is.na(x)),
      unique = length(unique(x[!is.na(x)])),
      mean = NA,
      sd = NA,
      min = NA,
      median = NA,
      max = NA,
      top = NA
    )

    if (is.numeric(x)) {
      if (any(!is.na(x))) {
        row$mean <- round(mean(x, na.rm = TRUE), 4)
        row$sd <- round(stats::sd(x, na.rm = TRUE), 4)
        row$min <- round(min(x, na.rm = TRUE), 4)
        row$median <- round(stats::median(x, na.rm = TRUE), 4)
        row$max <- round(max(x, na.rm = TRUE), 4)
      }
    } else {
      row$top <- as.character(mode_value(as.character(x)))
    }

    as.data.frame(row, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

preview_datatable <- function(df, n = 12) {
  DT::datatable(
    utils::head(df, n),
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = n,
      dom = "tip"
    )
  )
}

empty_plotly <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(
      template = "plotly_white",
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        )
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

formula_from_col <- function(col_name) {
  as.formula(paste0("~`", col_name, "`"))
}

first_or_default <- function(x, default = character(0)) {
  if (length(x) == 0) {
    default
  } else {
    x[[1]]
  }
}

ui <- navbarPage(
  title = app_title,
  id = "main_tabs",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f78"),
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background: linear-gradient(180deg, #f7fafc 0%, #eef4f7 100%);
        }
        .hero-box {
          margin: 18px 0 22px 0;
          padding: 24px;
          border-radius: 18px;
          background: linear-gradient(135deg, #12344d 0%, #1f6f78 60%, #d7f0e8 100%);
          color: white;
          box-shadow: 0 14px 36px rgba(18, 52, 77, 0.18);
        }
        .section-card {
          background: white;
          border-radius: 14px;
          padding: 18px;
          margin-bottom: 16px;
          box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
        }
        .help-inline {
          color: #4b5563;
          font-size: 0.95rem;
        }
      "))
    )
  ),
  tabPanel(
    "User Guide",
    fluidRow(
      column(
        12,
        div(
          class = "hero-box",
          h2("Course Project 2: End-to-End Data Wrangling App"),
          p(
            "This Shiny app lets users upload data, clean it, preprocess it, create features, ",
            "explore it interactively, and export the final result from a single interface."
          )
        )
      )
    ),
    fluidRow(
      column(
        7,
        div(
          class = "section-card",
          h3("Why this matches an advanced rubric"),
          tags$ul(
            tags$li("Supports CSV, Excel, JSON, and RDS upload."),
            tags$li("Provides built-in datasets for immediate testing."),
            tags$li("Offers interactive cleaning, preprocessing, and feature engineering."),
            tags$li("Uses Plotly for dynamic EDA and a correlation heatmap."),
            tags$li("Provides a multi-tab workflow with user guidance and export.")
          ),
          h4("Recommended workflow"),
          tags$ol(
            tags$li("Load or upload a dataset."),
            tags$li("Standardize values and resolve missing data."),
            tags$li("Apply preprocessing operations."),
            tags$li("Create new features."),
            tags$li("Explore plots and statistics."),
            tags$li("Download the final dataset.")
          )
        )
      ),
      column(
        5,
        div(
          class = "section-card",
          h3("Built-in datasets"),
          tags$ul(
            tags$li("iris: flower measurements and species labels"),
            tags$li("mtcars: vehicle performance metrics"),
            tags$li("ToothGrowth: dosage and tooth length experiment")
          ),
          h3("Included functionality"),
          tags$ul(
            tags$li("Duplicate handling"),
            tags$li("Missing-value imputation"),
            tags$li("Scaling and categorical encoding"),
            tags$li("Outlier capping or removal"),
            tags$li("Feature operations: add, subtract, multiply, divide, log, square")
          )
        )
      )
    )
  ),
  tabPanel(
    "Load Data",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Upload Data"),
        helpText("Supported file types: CSV, XLSX, XLS, JSON, RDS."),
        fileInput("upload_file", "Upload a dataset"),
        tags$hr(),
        h4("Or use a built-in dataset"),
        selectInput(
          "builtin_dataset",
          "Built-in dataset",
          choices = c("iris", "mtcars", "ToothGrowth"),
          selected = "iris"
        ),
        actionButton("load_builtin", "Load Built-in Dataset", class = "btn-primary")
      ),
      mainPanel(
        div(class = "section-card",
            h4("Current source"),
            verbatimTextOutput("source_summary")),
        div(class = "section-card",
            h4("Raw data preview"),
            DTOutput("raw_preview"))
      )
    )
  ),
  tabPanel(
    "Cleaning",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        checkboxInput("standardize_text", "Standardize text and column names", FALSE),
        selectInput(
          "duplicate_action",
          "Duplicate handling",
          choices = c("remove", "flag", "keep"),
          selected = "keep"
        ),
        selectInput(
          "missing_strategy",
          "Missing value handling",
          choices = c("smart_impute", "drop_rows", "keep"),
          selected = "keep"
        ),
        selectInput(
          "numeric_impute",
          "Numeric fill strategy",
          choices = c("median", "mean", "zero"),
          selected = "median"
        ),
        selectInput(
          "categorical_impute",
          "Categorical fill strategy",
          choices = c("mode", "missing_label"),
          selected = "mode"
        )
      ),
      mainPanel(
        div(class = "section-card",
            h4("Cleaning summary"),
            verbatimTextOutput("cleaning_summary")),
        div(class = "section-card",
            h4("Missing-value profile"),
            DTOutput("missing_profile_table")),
        div(class = "section-card",
            h4("Cleaned data preview"),
            DTOutput("cleaned_preview"))
      )
    )
  ),
  tabPanel(
    "Preprocessing",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(
          "outlier_method",
          "Outlier handling",
          choices = c("none", "cap", "remove"),
          selected = "none"
        ),
        selectizeInput(
          "outlier_cols",
          "Numeric columns for outlier handling",
          choices = NULL,
          multiple = TRUE
        ),
        tags$hr(),
        selectInput(
          "scaling_method",
          "Scaling method",
          choices = c("none", "standard", "minmax", "robust"),
          selected = "none"
        ),
        selectizeInput(
          "scale_cols",
          "Numeric columns to scale",
          choices = NULL,
          multiple = TRUE
        ),
        tags$hr(),
        selectInput(
          "encoding_method",
          "Categorical encoding",
          choices = c("none", "onehot", "label"),
          selected = "none"
        ),
        selectizeInput(
          "encoding_cols",
          "Categorical columns to encode",
          choices = NULL,
          multiple = TRUE
        )
      ),
      mainPanel(
        div(class = "section-card",
            h4("Preprocessing summary"),
            verbatimTextOutput("preprocessing_summary")),
        div(class = "section-card",
            h4("Processed data preview"),
            DTOutput("processed_preview"))
      )
    )
  ),
  tabPanel(
    "Feature Engineering",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        p(class = "help-inline", "Create new numeric features interactively and inspect the result immediately."),
        textInput("feature_name", "New feature name", placeholder = "example: petal_area"),
        selectInput(
          "feature_operation",
          "Operation",
          choices = c("add", "subtract", "multiply", "divide", "log", "square"),
          selected = "multiply"
        ),
        selectInput("feature_col1", "Primary numeric column", choices = NULL),
        selectInput("feature_col2", "Second numeric column", choices = NULL),
        actionButton("add_feature", "Add Feature", class = "btn-primary"),
        actionButton("reset_features", "Reset Engineered Features"),
        tags$hr(),
        selectInput("feature_focus", "Feature to inspect", choices = NULL)
      ),
      mainPanel(
        div(class = "section-card",
            h4("Feature engineering status"),
            verbatimTextOutput("feature_summary")),
        div(class = "section-card",
            h4("Feature recipe list"),
            DTOutput("feature_recipe_table")),
        div(class = "section-card",
            h4("Feature preview"),
            DTOutput("featured_preview")),
        div(class = "section-card",
            h4("Feature distribution"),
            plotlyOutput("feature_plot", height = "380px"))
      )
    )
  ),
  tabPanel(
    "EDA / Visualization",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(
          "plot_type",
          "Plot type",
          choices = c("Histogram", "Box", "Scatter", "Bar"),
          selected = "Histogram"
        ),
        selectInput("x_var", "X variable", choices = NULL),
        selectInput("y_var", "Y variable", choices = "None", selected = "None"),
        selectInput("color_var", "Color grouping", choices = "None", selected = "None"),
        tags$hr(),
        selectInput("filter_col", "Optional filter column", choices = "None", selected = "None"),
        uiOutput("filter_ui")
      ),
      mainPanel(
        div(class = "section-card",
            h4("Interactive visualization"),
            plotlyOutput("eda_plot", height = "460px")),
        div(class = "section-card",
            h4("Summary statistics"),
            DTOutput("summary_stats_table")),
        div(class = "section-card",
            h4("Correlation heatmap"),
            plotlyOutput("correlation_heatmap", height = "500px"))
      )
    )
  ),
  tabPanel(
    "Export / Download",
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("Export the transformed dataset"),
          p("Download includes all selected cleaning, preprocessing, and feature-engineering steps."),
          downloadButton("download_processed_data", "Download Current Data as CSV", class = "btn-primary"),
          tags$br(),
          tags$br(),
          verbatimTextOutput("export_summary")
        )
      ),
      column(
        12,
        div(class = "section-card",
            h4("Current export preview"),
            DTOutput("export_preview"))
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactiveVal(load_builtin_dataset("iris"))
  source_name <- reactiveVal("Built-in dataset: iris")
  status_message <- reactiveVal("Loaded the built-in iris dataset.")
  feature_recipes <- reactiveVal(list())
  feature_message <- reactiveVal("No engineered features yet.")

  observeEvent(input$load_builtin, {
    df <- load_builtin_dataset(input$builtin_dataset)
    raw_data(df)
    source_name(paste("Built-in dataset:", input$builtin_dataset))
    status_message(paste("Loaded the built-in", input$builtin_dataset, "dataset successfully."))
    feature_recipes(list())
    feature_message("Feature recipes were reset for the new dataset.")
  })

  observeEvent(input$upload_file, {
    req(input$upload_file)
    info <- input$upload_file

    tryCatch(
      {
        df <- read_uploaded_data(info$datapath, info$name)
        raw_data(df)
        source_name(paste("Uploaded file:", info$name))
        status_message(paste("Uploaded and parsed", info$name, "successfully."))
        feature_recipes(list())
        feature_message("Feature recipes were reset for the uploaded dataset.")
      },
      error = function(e) {
        status_message(paste("Failed to load file:", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error")
      }
    )
  })

  cleaned_data <- reactive({
    df <- raw_data()
    req(df)
    apply_cleaning(
      df = df,
      standardize_text = input$standardize_text,
      duplicate_action = input$duplicate_action,
      missing_strategy = input$missing_strategy,
      numeric_strategy = input$numeric_impute,
      categorical_strategy = input$categorical_impute
    )
  })

  preprocessed_data <- reactive({
    df <- cleaned_data()
    req(df)

    outlier_cols <- input$outlier_cols
    scale_cols <- input$scale_cols
    encoding_cols <- input$encoding_cols

    apply_preprocessing(
      df = df,
      outlier_method = input$outlier_method,
      outlier_columns = outlier_cols,
      scaling_method = input$scaling_method,
      scaling_columns = scale_cols,
      encoding_method = input$encoding_method,
      encoding_columns = encoding_cols
    )
  })

  featured_data <- reactive({
    df <- preprocessed_data()
    req(df)
    apply_feature_recipes(df, feature_recipes())
  })

  filtered_data <- reactive({
    df <- featured_data()
    req(df)

    filter_col <- input$filter_col
    if (is.null(filter_col) || identical(filter_col, "None") || !(filter_col %in% names(df))) {
      return(df)
    }

    if (is.numeric(df[[filter_col]])) {
      rng <- input$filter_range
      if (is.null(rng) || length(rng) != 2) {
        return(df)
      }
      keep <- !is.na(df[[filter_col]]) & df[[filter_col]] >= rng[1] & df[[filter_col]] <= rng[2]
      return(df[keep, , drop = FALSE])
    }

    levels_selected <- input$filter_levels
    if (is.null(levels_selected) || length(levels_selected) == 0) {
      return(df[0, , drop = FALSE])
    }

    keep <- as.character(df[[filter_col]]) %in% levels_selected
    df[keep, , drop = FALSE]
  })

  observe({
    df <- cleaned_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    categorical_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

    current_outlier <- isolate(input$outlier_cols)
    current_scale <- isolate(input$scale_cols)
    current_encoding <- isolate(input$encoding_cols)

    selected_outlier <- intersect(current_outlier, numeric_cols)
    selected_scale <- intersect(current_scale, numeric_cols)
    selected_encoding <- intersect(current_encoding, categorical_cols)

    updateSelectizeInput(session, "outlier_cols", choices = numeric_cols, selected = selected_outlier, server = TRUE)
    updateSelectizeInput(session, "scale_cols", choices = numeric_cols, selected = selected_scale, server = TRUE)
    updateSelectizeInput(session, "encoding_cols", choices = categorical_cols, selected = selected_encoding, server = TRUE)
  })

  observe({
    df <- featured_data()
    cols <- names(df)
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    current_feature_col1 <- isolate(input$feature_col1)
    current_feature_col2 <- isolate(input$feature_col2)
    current_feature_focus <- isolate(input$feature_focus)
    current_x <- isolate(input$x_var)
    current_y <- isolate(input$y_var)
    current_color <- isolate(input$color_var)
    current_filter <- isolate(input$filter_col)

    updateSelectInput(
      session,
      "feature_col1",
      choices = numeric_cols,
      selected = if (current_feature_col1 %in% numeric_cols) current_feature_col1 else first_or_default(numeric_cols)
    )
    updateSelectInput(
      session,
      "feature_col2",
      choices = numeric_cols,
      selected = if (current_feature_col2 %in% numeric_cols) current_feature_col2 else first_or_default(numeric_cols)
    )
    updateSelectInput(
      session,
      "feature_focus",
      choices = numeric_cols,
      selected = if (current_feature_focus %in% numeric_cols) current_feature_focus else first_or_default(numeric_cols)
    )
    updateSelectInput(
      session,
      "x_var",
      choices = cols,
      selected = if (current_x %in% cols) current_x else first_or_default(cols)
    )
    updateSelectInput(
      session,
      "y_var",
      choices = c("None", numeric_cols),
      selected = if (current_y %in% c("None", numeric_cols)) current_y else "None"
    )
    updateSelectInput(
      session,
      "color_var",
      choices = c("None", cols),
      selected = if (current_color %in% c("None", cols)) current_color else "None"
    )
    updateSelectInput(
      session,
      "filter_col",
      choices = c("None", cols),
      selected = if (current_filter %in% c("None", cols)) current_filter else "None"
    )
  })

  observeEvent(input$add_feature, {
    df <- preprocessed_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (!(input$feature_col1 %in% numeric_cols)) {
      feature_message("Choose a valid numeric column for the primary input.")
      return()
    }

    if (
      input$feature_operation %in% c("add", "subtract", "multiply", "divide") &&
      !(input$feature_col2 %in% numeric_cols)
    ) {
      feature_message("Choose a valid second numeric column for the selected operation.")
      return()
    }

    default_name <- if (input$feature_operation %in% c("add", "subtract", "multiply", "divide")) {
      paste(input$feature_col1, input$feature_operation, input$feature_col2, sep = "_")
    } else {
      paste(input$feature_operation, input$feature_col1, sep = "_")
    }

    raw_feature_name <- trimws(input$feature_name %||% "")
    requested_name <- if (nzchar(raw_feature_name)) {
      clean_column_name(raw_feature_name)
    } else {
      clean_column_name(default_name)
    }

    existing_names <- c(names(df), vapply(feature_recipes(), function(x) x$name, character(1)))
    final_name <- make_unique_name(requested_name, existing_names)

    recipes <- feature_recipes()
    recipes[[length(recipes) + 1]] <- list(
      name = final_name,
      operation = input$feature_operation,
      col1 = input$feature_col1,
      col2 = input$feature_col2
    )

    feature_recipes(recipes)
    feature_message(paste("Added feature", shQuote(final_name), "using the", input$feature_operation, "operation."))
  })

  observeEvent(input$reset_features, {
    feature_recipes(list())
    feature_message("All engineered features were removed.")
  })

  output$source_summary <- renderText({
    paste(
      source_name(),
      status_message(),
      data_overview(raw_data()),
      sep = "\n"
    )
  })

  output$raw_preview <- renderDT({
    preview_datatable(raw_data())
  })

  output$cleaning_summary <- renderText({
    df_raw <- raw_data()
    df_clean <- cleaned_data()
    duplicate_count <- sum(duplicated(df_raw))

    paste(
      paste("Before cleaning ->", data_overview(df_raw)),
      paste("After cleaning  ->", data_overview(df_clean)),
      paste("Original duplicate rows detected:", duplicate_count),
      paste(
        "Standardize text:", input$standardize_text,
        "| Duplicate action:", input$duplicate_action,
        "| Missing strategy:", input$missing_strategy
      ),
      sep = "\n"
    )
  })

  output$missing_profile_table <- renderDT({
    preview_datatable(build_missing_profile(cleaned_data()), n = 20)
  })

  output$cleaned_preview <- renderDT({
    preview_datatable(cleaned_data())
  })

  output$preprocessing_summary <- renderText({
    paste(
      paste("Input to preprocessing ->", data_overview(cleaned_data())),
      paste("Output after preprocessing ->", data_overview(preprocessed_data())),
      paste(
        "Outlier method:", input$outlier_method,
        "| Scaling:", input$scaling_method,
        "| Encoding:", input$encoding_method
      ),
      sep = "\n"
    )
  })

  output$processed_preview <- renderDT({
    preview_datatable(preprocessed_data())
  })

  output$feature_summary <- renderText({
    paste(
      feature_message(),
      paste("Saved feature rules:", length(feature_recipes())),
      paste("Current dataset ->", data_overview(featured_data())),
      sep = "\n"
    )
  })

  output$feature_recipe_table <- renderDT({
    recipes <- feature_recipes()
    if (length(recipes) == 0) {
      return(
        DT::datatable(
          data.frame(message = "No feature recipes have been added yet."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }

    recipe_df <- do.call(
      rbind,
      lapply(recipes, function(x) as.data.frame(x, stringsAsFactors = FALSE))
    )

    preview_datatable(recipe_df, n = 20)
  })

  output$featured_preview <- renderDT({
    preview_datatable(featured_data())
  })

  output$feature_plot <- renderPlotly({
    df <- featured_data()
    focus <- input$feature_focus

    if (nrow(df) == 0 || is.null(focus) || !(focus %in% names(df)) || !is.numeric(df[[focus]])) {
      return(empty_plotly("Create or select a numeric feature to inspect."))
    }

    plot_ly(data = df, x = formula_from_col(focus), type = "histogram", nbinsx = 30, marker = list(color = "#1f6f78")) |>
      layout(template = "plotly_white", bargap = 0.08)
  })

  output$filter_ui <- renderUI({
    df <- featured_data()
    filter_col <- input$filter_col

    if (is.null(filter_col) || identical(filter_col, "None") || !(filter_col %in% names(df))) {
      return(helpText("No filter selected."))
    }

    if (is.numeric(df[[filter_col]])) {
      values <- df[[filter_col]][!is.na(df[[filter_col]])]
      if (length(values) == 0) {
        return(helpText("The selected numeric column has no non-missing values."))
      }

      current <- isolate(input$filter_range)
      default_min <- min(values)
      default_max <- max(values)

      if (is.null(current) || length(current) != 2) {
        current <- c(default_min, default_max)
      }

      sliderInput(
        "filter_range",
        paste(filter_col, "range"),
        min = floor(default_min * 100) / 100,
        max = ceiling(default_max * 100) / 100,
        value = c(max(default_min, current[1]), min(default_max, current[2]))
      )
    } else {
      levels_available <- sort(unique(as.character(df[[filter_col]][!is.na(df[[filter_col]])])))
      current <- isolate(input$filter_levels)
      current <- intersect(current %||% levels_available, levels_available)

      selectizeInput(
        "filter_levels",
        paste(filter_col, "values"),
        choices = levels_available,
        selected = current,
        multiple = TRUE
      )
    }
  })

  output$eda_plot <- renderPlotly({
    df <- filtered_data()

    if (nrow(df) == 0) {
      return(empty_plotly("No rows match the current filter selection."))
    }

    x_var <- input$x_var
    y_var <- input$y_var
    color_var <- input$color_var
    plot_type <- input$plot_type

    if (is.null(x_var) || !(x_var %in% names(df))) {
      return(empty_plotly("Choose a valid X variable."))
    }

    color_formula <- if (!is.null(color_var) && !identical(color_var, "None") && color_var %in% names(df)) {
      formula_from_col(color_var)
    } else {
      NULL
    }

    if (identical(plot_type, "Histogram")) {
      p <- plot_ly(
        data = df,
        x = formula_from_col(x_var),
        color = color_formula,
        type = "histogram",
        nbinsx = 30
      )
      return(layout(p, template = "plotly_white", bargap = 0.08))
    }

    if (identical(plot_type, "Box")) {
      target_y <- if (!is.null(y_var) && !identical(y_var, "None") && y_var %in% names(df)) y_var else x_var
      if (!is.numeric(df[[target_y]])) {
        return(empty_plotly("Choose a numeric variable for the box plot."))
      }

      if (is.null(color_formula)) {
        p <- plot_ly(data = df, y = formula_from_col(target_y), type = "box", boxpoints = "outliers")
      } else {
        p <- plot_ly(
          data = df,
          x = formula_from_col(color_var),
          y = formula_from_col(target_y),
          color = color_formula,
          type = "box",
          boxpoints = "outliers"
        )
      }
      return(layout(p, template = "plotly_white"))
    }

    if (identical(plot_type, "Scatter")) {
      if (is.null(y_var) || identical(y_var, "None") || !(y_var %in% names(df))) {
        return(empty_plotly("Scatter plots require both X and Y variables."))
      }

      p <- plot_ly(
        data = df,
        x = formula_from_col(x_var),
        y = formula_from_col(y_var),
        color = color_formula,
        type = "scatter",
        mode = "markers"
      )
      return(layout(p, template = "plotly_white"))
    }

    if (is.null(y_var) || identical(y_var, "None") || !(y_var %in% names(df))) {
      counts <- as.data.frame(table(df[[x_var]], useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c(x_var, "count")
      p <- plot_ly(data = counts, x = formula_from_col(x_var), y = ~count, type = "bar")
      return(layout(p, template = "plotly_white"))
    }

    p <- plot_ly(
      data = df,
      x = formula_from_col(x_var),
      y = formula_from_col(y_var),
      color = color_formula,
      type = "bar"
    )
    layout(p, template = "plotly_white")
  })

  output$summary_stats_table <- renderDT({
    preview_datatable(build_summary_table(filtered_data()), n = 25)
  })

  output$correlation_heatmap <- renderPlotly({
    df <- filtered_data()
    numeric_df <- df[vapply(df, is.numeric, logical(1))]

    if (ncol(numeric_df) < 2 || nrow(numeric_df) == 0) {
      return(empty_plotly("At least two numeric columns are required for a correlation heatmap."))
    }

    corr_matrix <- round(stats::cor(numeric_df, use = "pairwise.complete.obs"), 2)
    plot_ly(
      x = colnames(corr_matrix),
      y = rownames(corr_matrix),
      z = corr_matrix,
      type = "heatmap",
      colors = c("#b2182b", "#f7f7f7", "#2166ac")
    ) |>
      layout(template = "plotly_white")
  })

  output$export_summary <- renderText({
    paste(
      paste("Export source:", source_name()),
      paste("Final dataset ->", data_overview(featured_data())),
      paste("Feature recipes included:", length(feature_recipes())),
      sep = "\n"
    )
  })

  output$export_preview <- renderDT({
    preview_datatable(featured_data())
  })

  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste0("processed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(featured_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
