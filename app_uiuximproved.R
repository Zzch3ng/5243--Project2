
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)

# Global Configuration 
options(shiny.maxRequestSize = 300 * 1024^2)
app_title <- "Interactive Data Wrangling Studio"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Data Loading Helpers 
load_builtin_dataset <- function(name) {
  switch(
    tolower(name %||% "test1"),
    "test2" = mtcars,
    "mtcars" = mtcars,
    "test3" = ToothGrowth,
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

# Data Cleaning Helpers 
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
    return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
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

# Preprocessing Helpers 
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

# Feature Engineering Helpers 
feature_description <- function(operation, col1, col2 = NULL) {
  if (identical(operation, "add")) return(paste0(col1, " + ", col2))
  if (identical(operation, "subtract")) return(paste0(col1, " - ", col2))
  if (identical(operation, "multiply")) return(paste0(col1, " × ", col2))
  if (identical(operation, "divide")) return(paste0(col1, " / ", col2))
  if (identical(operation, "log")) return(paste0("log1p(", col1, ")"))
  if (identical(operation, "square")) return(paste0(col1, "^2"))
  operation
}

apply_single_feature <- function(df, recipe) {
  if (nrow(df) == 0) {
    return(df)
  }

  name <- recipe$name
  operation <- recipe$operation
  col1 <- recipe$col1
  col2 <- recipe$col2

  if (!(col1 %in% names(df))) {
    return(df)
  }

  x1 <- suppressWarnings(as.numeric(df[[col1]]))

  if (operation %in% c("add", "subtract", "multiply", "divide")) {
    if (!(col2 %in% names(df))) {
      return(df)
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

  df
}

apply_feature_recipes <- function(df, recipes) {
  if (length(recipes) == 0 || nrow(df) == 0) {
    return(df)
  }

  out <- df
  for (recipe in recipes) {
    out <- apply_single_feature(out, recipe)
  }

  out
}

feature_stats_table <- function(x, label) {
  if (is.null(x) || all(is.na(x))) {
    return(data.frame(
      dataset = label,
      non_missing = 0,
      mean = NA,
      sd = NA,
      min = NA,
      median = NA,
      max = NA
    ))
  }

  data.frame(
    dataset = label,
    non_missing = sum(!is.na(x)),
    mean = round(mean(x, na.rm = TRUE), 4),
    sd = round(stats::sd(x, na.rm = TRUE), 4),
    min = round(min(x, na.rm = TRUE), 4),
    median = round(stats::median(x, na.rm = TRUE), 4),
    max = round(max(x, na.rm = TRUE), 4)
  )
}

# Display and Reporting Helpers 
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
  if (length(x) == 0) default else x[[1]]
}

metric_card <- function(title, value_output_id, subtitle = NULL) {
  div(
    class = "metric-box",
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(value_output_id, inline = TRUE)),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

workflow_step <- function(number, title, text) {
  div(
    class = "workflow-step",
    div(class = "step-badge", number),
    div(
      class = "step-copy",
      h4(title),
      p(text)
    )
  )
}

# User Interface 
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
        .navbar {
          box-shadow: 0 6px 18px rgba(15, 23, 42, 0.08);
        }
        .hero-box {
          margin: 18px 0 22px 0;
          padding: 28px;
          border-radius: 20px;
          background: linear-gradient(135deg, #12344d 0%, #1f6f78 60%, #d7f0e8 100%);
          color: white;
          box-shadow: 0 14px 36px rgba(18, 52, 77, 0.18);
        }
        .hero-box p {
          margin-bottom: 0;
          font-size: 1rem;
          line-height: 1.6;
          opacity: 0.98;
        }
        .section-card {
          background: white;
          border-radius: 16px;
          padding: 18px;
          margin-bottom: 16px;
          box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
          border: 1px solid rgba(148, 163, 184, 0.15);
        }
        .section-card h4, .section-card h3 {
          margin-top: 0;
        }
        .panel-note {
          color: #475569;
          font-size: 0.96rem;
          line-height: 1.55;
          margin-bottom: 14px;
        }
        .help-inline {
          color: #4b5563;
          font-size: 0.95rem;
          line-height: 1.5;
        }
        .control-group {
          background: #f8fafc;
          border: 1px solid #e2e8f0;
          border-radius: 14px;
          padding: 14px;
          margin-bottom: 14px;
        }
        .control-group h5 {
          margin-top: 0;
          margin-bottom: 8px;
          color: #12344d;
          font-weight: 700;
        }
        .metric-box {
          background: white;
          border-radius: 16px;
          padding: 16px 18px;
          box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
          border: 1px solid rgba(148, 163, 184, 0.15);
          min-height: 112px;
          margin-bottom: 16px;
        }
        .metric-title {
          color: #64748b;
          font-size: 0.85rem;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 8px;
          font-weight: 700;
        }
        .metric-value {
          color: #0f172a;
          font-size: 1.7rem;
          font-weight: 800;
          line-height: 1.2;
        }
        .metric-subtitle {
          color: #64748b;
          font-size: 0.88rem;
          margin-top: 6px;
        }
        .workflow-step {
          display: flex;
          gap: 14px;
          align-items: flex-start;
          padding: 14px 0;
          border-bottom: 1px solid #e5e7eb;
        }
        .workflow-step:last-child {
          border-bottom: none;
          padding-bottom: 0;
        }
        .step-badge {
          width: 36px;
          height: 36px;
          border-radius: 50%;
          background: #1f6f78;
          color: white;
          font-weight: 700;
          display: flex;
          align-items: center;
          justify-content: center;
          flex-shrink: 0;
        }
        .step-copy h4 {
          margin: 0 0 6px 0;
          font-size: 1.05rem;
        }
        .step-copy p {
          margin: 0;
          color: #475569;
        }
        .status-banner {
          background: linear-gradient(90deg, rgba(31,111,120,0.12), rgba(31,111,120,0.04));
          border: 1px solid rgba(31,111,120,0.14);
          border-radius: 14px;
          padding: 14px 16px;
          color: #12344d;
          margin-bottom: 16px;
        }
        .btn-warning {
          color: #7c2d12;
          background-color: #ffedd5;
          border-color: #fdba74;
          font-weight: 600;
        }
        .sidebar-panel .form-group {
          margin-bottom: 12px;
        }
        .tab-pane {
          padding-top: 8px;
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
          h2("Interactive Data Wrangling Studio"),
          p("Upload a dataset, clean it, preprocess it, create new features, explore the data interactively, and export the final result — all in one guided workflow.")
        )
      )
    ),
    fluidRow(
      column(
        7,
        div(
          class = "section-card",
          h3("How to use the app"),
          workflow_step("1", "Load data", "Upload a CSV, Excel, JSON, or RDS file, or start with one of the built-in example datasets."),
          workflow_step("2", "Clean the dataset", "Standardize text, handle duplicates, and decide how to treat missing values."),
          workflow_step("3", "Preprocess variables", "Apply outlier treatment, scaling, and categorical encoding based on your goals."),
          workflow_step("4", "Create new features", "Build derived variables from existing numeric columns and inspect them immediately."),
          workflow_step("5", "Explore and export", "Use interactive plots, summaries, and filters, then download the transformed dataset.")
        )
      ),
      column(
        5,
        div(
          class = "section-card",
          h3("What this app includes"),
          tags$ul(
            tags$li("Multiple upload formats: CSV, Excel, JSON, and RDS"),
            tags$li("Built-in datasets for quick testing"),
            tags$li("Interactive cleaning and preprocessing controls"),
            tags$li("Feature engineering with instant preview"),
            tags$li("Interactive Plotly charts and a correlation heatmap"),
            tags$li("Final dataset export as CSV")
          ),
          h4("Built-in datasets"),
          tags$ul(
            tags$li("test1 (iris)"),
            tags$li("test2 (mtcars)"),
            tags$li("test3 (ToothGrowth)")
          )
        )
      )
    )
  ),

  tabPanel(
    "Load Data",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows")),
      column(3, metric_card("Columns", "metric_cols")),
      column(3, metric_card("Missing Values", "metric_missing")),
      column(3, metric_card("Engineered Features", "metric_features"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Start here."), br(), "Choose a built-in dataset or upload your own file to begin the workflow."),
        div(
          class = "control-group",
          h5("Upload a dataset"),
          p(class = "panel-note", "Supported file types: CSV, XLSX, XLS, JSON, and RDS."),
          fileInput("upload_file", "Choose file")
        ),
        div(
          class = "control-group",
          h5("Use a built-in dataset"),
          p(class = "panel-note", "Useful for testing the app quickly without preparing a file."),
          selectInput(
            "builtin_dataset",
            "Built-in dataset",
            choices = c(
              "test1 (iris)" = "test1",
              "test2 (mtcars)" = "test2",
              "test3 (ToothGrowth)" = "test3"
            ),
            selected = "test1"
          ),
          actionButton("load_builtin", "Load Built-in Dataset", class = "btn-primary")
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Current source"), p(class = "panel-note", "This box shows the current dataset source and a quick overview."), verbatimTextOutput("source_summary")),
        div(class = "section-card", h4("Raw data preview"), p(class = "panel-note", "Preview the first rows before applying any transformations."), DTOutput("raw_preview"))
      )
    )
  ),

  tabPanel(
    "Cleaning",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_clean")),
      column(3, metric_card("Columns", "metric_cols_clean")),
      column(3, metric_card("Missing Values", "metric_missing_clean")),
      column(3, metric_card("Duplicates", "metric_duplicates_clean"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Cleaning controls."), br(), "Standardize messy text, choose how to handle duplicate rows, and decide how missing values should be treated."),
        div(
          class = "control-group",
          h5("Standardization"),
          p(class = "panel-note", "Useful for messy uploads with inconsistent column names or Yes/No text values."),
          checkboxInput("standardize_text", "Standardize text and column names", FALSE)
        ),
        div(
          class = "control-group",
          h5("Duplicates"),
          p(class = "panel-note", "Remove duplicates, flag them, or leave them unchanged."),
          selectInput(
            "duplicate_action",
            "Duplicate handling",
            choices = c("remove", "flag", "keep"),
            selected = "keep"
          )
        ),
        div(
          class = "control-group",
          h5("Missing values"),
          p(class = "panel-note", "Choose whether to keep missing values, remove incomplete rows, or fill them automatically."),
          selectInput(
            "missing_strategy",
            "Missing value handling",
            choices = c("smart_impute", "drop_rows", "keep"),
            selected = "keep"
          ),
          helpText("Numeric columns can use median, mean, or zero. Categorical columns can use the mode or a Missing label."),
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
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Cleaning summary"), p(class = "panel-note", "Review the effect of your selected cleaning steps."), verbatimTextOutput("cleaning_summary")),
        div(class = "section-card", h4("Missing-value profile"), p(class = "panel-note", "Inspect missingness and cardinality by column after cleaning."), DTOutput("missing_profile_table")),
        div(class = "section-card", h4("Cleaned data preview"), p(class = "panel-note", "Preview the cleaned dataset before moving to preprocessing."), DTOutput("cleaned_preview"))
      )
    )
  ),

  tabPanel(
    "Preprocessing",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_pre")),
      column(3, metric_card("Columns", "metric_cols_pre")),
      column(3, metric_card("Numeric Columns", "metric_numeric_pre")),
      column(3, metric_card("Missing Values", "metric_missing_pre"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Preprocessing controls."), br(), "Apply transformations that prepare variables for analysis, modeling, and clearer comparisons."),
        div(
          class = "control-group",
          h5("Outlier handling"),
          p(class = "panel-note", "Use the IQR rule to cap extreme values or remove rows that contain outliers."),
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
          )
        ),
        div(
          class = "control-group",
          h5("Scaling"),
          p(class = "panel-note", "Standard scaling, min-max scaling, and robust scaling are available for selected numeric columns."),
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
          )
        ),
        div(
          class = "control-group",
          h5("Categorical encoding"),
          p(class = "panel-note", "Convert selected categorical variables to label encoding or one-hot encoded columns."),
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
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Preprocessing summary"), p(class = "panel-note", "Review your selected transformations before continuing."), verbatimTextOutput("preprocessing_summary")),
        div(class = "section-card", h4("Processed data preview"), p(class = "panel-note", "This preview reflects the dataset after outlier handling, scaling, and encoding."), DTOutput("processed_preview"))
      )
    )
  ),

  tabPanel(
    "Feature Engineering",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_feat")),
      column(3, metric_card("Columns", "metric_cols_feat")),
      column(3, metric_card("Saved Feature Rules", "metric_feature_rules")),
      column(3, metric_card("Missing Values", "metric_missing_feat"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Create derived variables."), br(), "Combine or transform numeric columns to build new features, then compare the original column with the engineered result."),
        div(
          class = "control-group",
          h5("Feature setup"),
          p(class = "panel-note", "You can enter a custom name or let the app generate one automatically."),
          textInput("feature_name", "New feature name", placeholder = "example: petal_area"),
          selectInput(
            "feature_operation",
            "Operation",
            choices = c("add", "subtract", "multiply", "divide", "log", "square"),
            selected = "multiply"
          ),
          selectInput("feature_col1", "Primary numeric column", choices = NULL),
          selectInput("feature_col2", "Second numeric column", choices = NULL),
          helpText("For log and square, only the primary numeric column is used.")
        ),
        div(
          class = "control-group",
          h5("Actions"),
          p(class = "panel-note", "Add a feature to save it into the workflow, or reset all engineered features."),
          actionButton("add_feature", "Add Feature", class = "btn-primary"),
          tags$span(style = "display:inline-block; width: 8px;"),
          actionButton("reset_features", "Reset Engineered Features", class = "btn-warning")
        ),
        div(
          class = "control-group",
          h5("Inspect a feature"),
          p(class = "panel-note", "Choose an engineered numeric feature to compare before and after the transformation."),
          selectInput("feature_focus", "Feature to inspect", choices = NULL)
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Feature engineering status"), p(class = "panel-note", "See whether a feature was added successfully and how many rules are currently saved."), verbatimTextOutput("feature_summary")),
        div(class = "section-card", h4("Feature recipe list"), p(class = "panel-note", "This table records the feature rules you have created so far, including an explanation label for each feature."), DTOutput("feature_recipe_table")),
        div(class = "section-card", h4("Feature preview"), p(class = "panel-note", "Preview the dataset after feature engineering has been applied."), DTOutput("featured_preview")),
        div(class = "section-card", h4("Before vs After summary"), p(class = "panel-note", "Compare summary statistics for the original input column and the engineered feature."), DTOutput("feature_compare_table")),
        div(
          class = "section-card",
          h4("Before vs After distribution"),
          p(class = "panel-note", "Use these side-by-side plots to see how the feature transformation changes the distribution."),
          fluidRow(
            column(6, plotlyOutput("feature_before_plot", height = "340px")),
            column(6, plotlyOutput("feature_after_plot", height = "340px"))
          )
        )
      )
    )
  ),

  tabPanel(
    "EDA / Visualization",
    fluidRow(
      column(3, metric_card("Rows After Filter", "metric_rows_eda")),
      column(3, metric_card("Columns", "metric_cols_eda")),
      column(3, metric_card("Numeric Columns", "metric_numeric_eda")),
      column(3, metric_card("Missing Values", "metric_missing_eda"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Explore the data interactively."), br(), "Set up a plot, optionally group by color, and use filters to focus on a subset of the dataset."),
        div(
          class = "control-group",
          h5("Plot setup"),
          p(class = "panel-note", "Choose the plot type and the variables you want to display."),
          selectInput(
            "plot_type",
            "Plot type",
            choices = c("Histogram", "Box", "Scatter", "Bar"),
            selected = "Histogram"
          ),
          selectInput("x_var", "X variable", choices = NULL),
          selectInput("y_var", "Y variable", choices = "None", selected = "None")
        ),
        div(
          class = "control-group",
          h5("Grouping"),
          p(class = "panel-note", "Optionally color the plot by another variable to compare groups."),
          selectInput("color_var", "Color grouping", choices = "None", selected = "None")
        ),
        div(
          class = "control-group",
          h5("Filter"),
          p(class = "panel-note", "Filter the dataset before plotting to focus on a range or selected categories."),
          selectInput("filter_col", "Optional filter column", choices = "None", selected = "None"),
          uiOutput("filter_ui")
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Interactive visualization"), p(class = "panel-note", "Use this chart to explore the current filtered dataset."), plotlyOutput("eda_plot", height = "460px")),
        div(class = "section-card", h4("Summary statistics"), p(class = "panel-note", "Review summary measures for the filtered data currently in view."), DTOutput("summary_stats_table")),
        div(class = "section-card", h4("Correlation heatmap"), p(class = "panel-note", "This heatmap is available when at least two numeric variables are present."), plotlyOutput("correlation_heatmap", height = "500px"))
      )
    )
  ),

  tabPanel(
    "Export / Download",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_export")),
      column(3, metric_card("Columns", "metric_cols_export")),
      column(3, metric_card("Engineered Features", "metric_features_export")),
      column(3, metric_card("Missing Values", "metric_missing_export"))
    ),
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("Export the transformed dataset"),
          p(class = "panel-note", "Download includes all selected cleaning, preprocessing, and feature-engineering steps from the current workflow."),
          downloadButton("download_processed_data", "Download Current Data as CSV", class = "btn-primary"),
          tags$br(), tags$br(),
          verbatimTextOutput("export_summary")
        )
      ),
      column(
        12,
        div(class = "section-card", h4("Current export preview"), p(class = "panel-note", "Preview the dataset that will be downloaded if you export now."), DTOutput("export_preview"))
      )
    )
  )
)

# Server Logic 
server <- function(input, output, session) {
  raw_data <- reactiveVal(load_builtin_dataset("test1"))
  source_name <- reactiveVal("Built-in dataset: test1 (iris)")
  status_message <- reactiveVal("Loaded the built-in test1 (iris) dataset.")
  feature_recipes <- reactiveVal(list())
  feature_message <- reactiveVal("No engineered features yet.")

  observeEvent(input$load_builtin, {
    df <- load_builtin_dataset(input$builtin_dataset)
    raw_data(df)
    display_name <- switch(
      input$builtin_dataset,
      "test1" = "test1 (iris)",
      "test2" = "test2 (mtcars)",
      "test3" = "test3 (ToothGrowth)",
      input$builtin_dataset
    )
    source_name(paste("Built-in dataset:", display_name))
    status_message(paste("Loaded the built-in", display_name, "dataset successfully."))
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

    apply_preprocessing(
      df = df,
      outlier_method = input$outlier_method,
      outlier_columns = input$outlier_cols,
      scaling_method = input$scaling_method,
      scaling_columns = input$scale_cols,
      encoding_method = input$encoding_method,
      encoding_columns = input$encoding_cols
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
    recipe_feature_names <- intersect(
      vapply(feature_recipes(), function(x) x$name, character(1)),
      numeric_cols
    )

    updateSelectInput(
      session,
      "feature_focus",
      choices = recipe_feature_names,
      selected = if (current_feature_focus %in% recipe_feature_names) current_feature_focus else first_or_default(recipe_feature_names)
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
    requested_name <- if (nzchar(raw_feature_name)) clean_column_name(raw_feature_name) else clean_column_name(default_name)

    existing_names <- c(names(df), vapply(feature_recipes(), function(x) x$name, character(1)))
    final_name <- make_unique_name(requested_name, existing_names)

    recipe_description <- feature_description(
      operation = input$feature_operation,
      col1 = input$feature_col1,
      col2 = input$feature_col2
    )

    recipes <- feature_recipes()
    recipes[[length(recipes) + 1]] <- list(
      name = final_name,
      operation = input$feature_operation,
      col1 = input$feature_col1,
      col2 = input$feature_col2,
      description = recipe_description
    )

    feature_recipes(recipes)
    feature_message(paste("Added feature", shQuote(final_name), "with label:", recipe_description))
  })

  observeEvent(input$reset_features, {
    feature_recipes(list())
    feature_message("All engineered features were removed.")
  })

  selected_recipe <- reactive({
    recipes <- feature_recipes()
    focus <- input$feature_focus

    if (length(recipes) == 0 || is.null(focus) || !nzchar(focus)) {
      return(NULL)
    }

    idx <- which(vapply(recipes, function(x) identical(x$name, focus), logical(1)))
    if (length(idx) == 0) {
      return(NULL)
    }

    recipes[[idx[1]]]
  })

  feature_before_data <- reactive({
    recipe <- selected_recipe()
    df <- preprocessed_data()

    if (is.null(recipe) || !(recipe$col1 %in% names(df))) {
      return(NULL)
    }

    suppressWarnings(as.numeric(df[[recipe$col1]]))
  })

  feature_after_data <- reactive({
    focus <- input$feature_focus
    df <- featured_data()

    if (is.null(focus) || !(focus %in% names(df)) || !is.numeric(df[[focus]])) {
      return(NULL)
    }

    suppressWarnings(as.numeric(df[[focus]]))
  })

  # Metric outputs 
  dataset_stats <- function(df) {
    list(
      rows = nrow(df),
      cols = ncol(df),
      missing = sum(is.na(df)),
      numeric = sum(vapply(df, is.numeric, logical(1))),
      duplicates = sum(duplicated(df))
    )
  }

  stats_raw <- reactive(dataset_stats(raw_data()))
  stats_clean <- reactive(dataset_stats(cleaned_data()))
  stats_pre <- reactive(dataset_stats(preprocessed_data()))
  stats_feat <- reactive(dataset_stats(featured_data()))
  stats_eda <- reactive(dataset_stats(filtered_data()))

  output$metric_rows <- renderText(format(stats_raw()$rows, big.mark = ","))
  output$metric_cols <- renderText(format(stats_raw()$cols, big.mark = ","))
  output$metric_missing <- renderText(format(stats_raw()$missing, big.mark = ","))
  output$metric_features <- renderText(length(feature_recipes()))

  output$metric_rows_clean <- renderText(format(stats_clean()$rows, big.mark = ","))
  output$metric_cols_clean <- renderText(format(stats_clean()$cols, big.mark = ","))
  output$metric_missing_clean <- renderText(format(stats_clean()$missing, big.mark = ","))
  output$metric_duplicates_clean <- renderText(format(stats_clean()$duplicates, big.mark = ","))

  output$metric_rows_pre <- renderText(format(stats_pre()$rows, big.mark = ","))
  output$metric_cols_pre <- renderText(format(stats_pre()$cols, big.mark = ","))
  output$metric_numeric_pre <- renderText(format(stats_pre()$numeric, big.mark = ","))
  output$metric_missing_pre <- renderText(format(stats_pre()$missing, big.mark = ","))

  output$metric_rows_feat <- renderText(format(stats_feat()$rows, big.mark = ","))
  output$metric_cols_feat <- renderText(format(stats_feat()$cols, big.mark = ","))
  output$metric_feature_rules <- renderText(length(feature_recipes()))
  output$metric_missing_feat <- renderText(format(stats_feat()$missing, big.mark = ","))

  output$metric_rows_eda <- renderText(format(stats_eda()$rows, big.mark = ","))
  output$metric_cols_eda <- renderText(format(stats_eda()$cols, big.mark = ","))
  output$metric_numeric_eda <- renderText(format(stats_eda()$numeric, big.mark = ","))
  output$metric_missing_eda <- renderText(format(stats_eda()$missing, big.mark = ","))

  output$metric_rows_export <- renderText(format(stats_feat()$rows, big.mark = ","))
  output$metric_cols_export <- renderText(format(stats_feat()$cols, big.mark = ","))
  output$metric_features_export <- renderText(length(feature_recipes()))
  output$metric_missing_export <- renderText(format(stats_feat()$missing, big.mark = ","))

  # Load Data outputs 
  output$source_summary <- renderText({
    paste(source_name(), status_message(), data_overview(raw_data()), sep = "\n")
  })

  output$raw_preview <- renderDT({
    preview_datatable(raw_data())
  })

  # Cleaning outputs 
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

  # Preprocessing outputs 
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

  # Feature Engineering outputs 
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
      lapply(recipes, function(x) {
        data.frame(
          feature = x$name,
          operation = x$operation,
          input_1 = x$col1,
          input_2 = x$col2 %||% "",
          explanation_label = x$description %||% feature_description(x$operation, x$col1, x$col2),
          stringsAsFactors = FALSE
        )
      })
    )

    preview_datatable(recipe_df, n = 20)
  })

  output$featured_preview <- renderDT({
    preview_datatable(featured_data())
  })

  output$feature_compare_table <- renderDT({
    recipe <- selected_recipe()
    if (is.null(recipe)) {
      return(
        DT::datatable(
          data.frame(message = "Select an engineered numeric feature to compare before vs after."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }

    before_x <- feature_before_data()
    after_x <- feature_after_data()

    if (is.null(before_x) || is.null(after_x)) {
      return(
        DT::datatable(
          data.frame(message = "Comparison is available for numeric engineered features."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }

    compare_df <- rbind(
      feature_stats_table(before_x, paste("Before:", recipe$col1)),
      feature_stats_table(after_x, paste("After:", recipe$name))
    )

    preview_datatable(compare_df, n = 10)
  })

  output$feature_before_plot <- renderPlotly({
    recipe <- selected_recipe()
    x <- feature_before_data()

    if (is.null(recipe) || is.null(x)) {
      return(empty_plotly("Select an engineered numeric feature to view the original distribution."))
    }

    plot_ly(x = x, type = "histogram", nbinsx = 30, marker = list(color = "#94a3b8")) |>
      layout(
        template = "plotly_white",
        title = paste("Before:", recipe$col1),
        bargap = 0.08
      )
  })

  output$feature_after_plot <- renderPlotly({
    recipe <- selected_recipe()
    x <- feature_after_data()

    if (is.null(recipe) || is.null(x)) {
      return(empty_plotly("Select an engineered numeric feature to view the transformed distribution."))
    }

    plot_ly(x = x, type = "histogram", nbinsx = 30, marker = list(color = "#1f6f78")) |>
      layout(
        template = "plotly_white",
        title = paste("After:", recipe$name),
        bargap = 0.08
      )
  })

  # EDA outputs 
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

  # Export outputs 
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

# App Entry Point 
shinyApp(ui = ui, server = server)
