
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)

# ---- 1. Global Configuration ----

# Allow larger uploads so the app can handle course-sized datasets.
options(shiny.maxRequestSize = 300 * 1024^2)

app_title <- "Data Wrangling Studio"

# a small helper for Shiny inputs that may be NULL or empty.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ---- 2. Data Loading Helpers ----

# Return one of the built-in example datasets used for testing the app.
# useful when the user wants to explore the workflow quickly without uploading a file.
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

# Clean column names so later cleaning and feature engineering steps are easier to manage.
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

# Keep engineered feature names unique if a user creates a name that already exists.
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

# ---- 3. Data Cleaning Helpers ----

# Simple mode helper used for categorical imputation.
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("Missing")
  }
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Read uploaded files based on their extension so the app supports multiple formats.
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

# Standardize text fields and column names before later cleaning steps.
#  make inconsistent user-uploaded data more uniform before imputation.
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

# Build the version of the dataset used for high-missing-column checks.
# If standardization is enabled, the check should use the standardized names and values.
prepare_cleaning_reference <- function(df, standardize_text = FALSE) {
  reference_df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  if (isTRUE(standardize_text)) {
    reference_df <- standardize_strings(reference_df)
  }

  reference_df
}

# Return columns whose missing-value ratio is greater than the chosen threshold.
high_missing_columns <- function(df, threshold = 0.8) {
  if (ncol(df) == 0) {
    return(character(0))
  }

  names(df)[vapply(df, function(x) mean(is.na(x)) > threshold, logical(1))]
}

# Apply the missing-value strategy chosen by the user.
# supports keeping missing values, dropping incomplete rows, or filling values automatically.
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

# Run the full cleaning stage selected in the Cleaning tab.
apply_cleaning <- function(
  df,
  standardize_text,
  duplicate_action,
  drop_columns,
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

  drop_columns <- intersect(drop_columns %||% character(0), names(cleaned))
  if (length(drop_columns) > 0) {
    cleaned <- cleaned[setdiff(names(cleaned), drop_columns)]
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

# ---- 4. Preprocessing Helpers ----

# Use the IQR rule to either cap extreme values or remove outlier rows.
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

# Apply the selected scaling method to chosen numeric columns.
# This lets users compare how standardization, min-max scaling, and robust scaling affect the data.
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

# Convert selected categorical variables to label-encoded or one-hot encoded columns.
# One-hot encoding expands the dataset, while label encoding keeps the original number of columns.
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

# Combine all preprocessing choices into one step of the pipeline.
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

# ---- 5. Feature Engineering Helpers ----

# Apply each saved feature recipe to the current preprocessed dataset.
# Each recipe stores the operation and input columns selected by the user in the Feature Engineering tab.
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

# ---- 6. Display and Reporting Helpers ----

# Short overview text reused across the app summaries.
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

# Column-level missing-value summary used in the Cleaning tab.
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

# Summary table for both numeric and categorical columns.
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

# Preview only the first few rows so the app stays responsive.
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

# Empty Plotly placeholder used when current selections are not valid for plotting.
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

# Plotly formulas need this format when the column name is chosen dynamically.
formula_from_col <- function(col_name) {
  as.formula(paste0("~`", col_name, "`"))
}

# Return the first available option if a current input is no longer valid.
first_or_default <- function(x, default = character(0)) {
  if (length(x) == 0) default else x[[1]]
}

# Small UI helper for the metric cards shown at the top of each tab.
# These cards give users quick feedback about how the dataset changes across the workflow.
metric_card <- function(title, value_output_id, subtitle = NULL) {
  div(
    class = "metric-box",
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(value_output_id, inline = TRUE)),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

# Small UI helper for the workflow steps in the User Guide tab.
# used this helper to keep the User Guide cleaner and more visually consistent.
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

# ---- 7. User Interface ----

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
  # Tab 1: user guide and workflow overview.
  tabPanel(
    "User Guide",
    fluidRow(
      column(
        12,
        div(
          class = "hero-box",
          h2("Data Wrangling Studio"),
          p("Upload a dataset, clean it, preprocess it, create new features, explore the data interactively, and export the final result — all in one guided workflow.")
        )
      )
    ),
    fluidRow(
      column(
        7,
        div(
          class = "section-card",
          h3("Studio Statement"),
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
  # Tab 2: dataset loading and raw preview.
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
  # Tab 3: cleaning controls and cleaned-data preview.
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
        ),
        div(
          class = "control-group",
          h5("High-missing columns"),
          p(class = "panel-note", "Columns with more than 80% missing values are listed here. You can choose whether to remove them before the app handles missing values."),
          selectizeInput(
            "high_missing_cols",
            "Columns to remove (>80% missing)",
            choices = NULL,
            multiple = TRUE
          ),
          helpText("Only columns with more than 80% missing values will appear in this list.")
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Cleaning summary"), p(class = "panel-note", "Review the effect of your selected cleaning steps."), verbatimTextOutput("cleaning_summary")),
        div(class = "section-card", h4("Missing-value profile"), p(class = "panel-note", "Inspect missingness and cardinality by column after cleaning."), DTOutput("missing_profile_table")),
        div(class = "section-card", h4("Cleaned data preview"), p(class = "panel-note", "Preview the cleaned dataset before moving to preprocessing."), DTOutput("cleaned_preview"))
      )
    )
  ),
  # Tab 4: preprocessing controls and processed-data preview.
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
  # Tab 5: feature engineering workflow and feature preview.
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
        div(class = "status-banner", strong("Create derived variables."), br(), "Combine or transform numeric columns to build new features, then inspect them on the right."),
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
          p(class = "panel-note", "Choose a numeric feature to preview its current distribution."),
          selectInput("feature_focus", "Feature to inspect", choices = NULL)
        )
      ),
      mainPanel(
        div(class = "section-card", h4("Feature engineering status"), p(class = "panel-note", "See whether a feature was added successfully and how many rules are currently saved."), verbatimTextOutput("feature_summary")),
        div(class = "section-card", h4("Feature recipe list"), p(class = "panel-note", "This table records the feature rules you have created so far."), DTOutput("feature_recipe_table")),
        div(class = "section-card", h4("Feature preview"), p(class = "panel-note", "Preview the dataset after feature engineering has been applied."), DTOutput("featured_preview")),
        div(class = "section-card", h4("Feature distribution"), p(class = "panel-note", "Inspect the selected numeric feature to see its current shape."), plotlyOutput("feature_plot", height = "380px"))
      )
    )
  ),
  # Tab 6: EDA controls, plots, and summary tables.
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
  # Tab 7: final export area.
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

# ---- 8. Server Logic ----

server <- function(input, output, session) {
  # Track the current source dataset and user-facing status text.
  raw_data <- reactiveVal(load_builtin_dataset("test1"))
  source_name <- reactiveVal("Built-in dataset: test1 (iris)")
  status_message <- reactiveVal("Loaded the built-in test1 (iris) dataset.")
  feature_recipes <- reactiveVal(list())
  feature_message <- reactiveVal("No engineered features yet.")

  # Load a built-in dataset and reset saved engineered features.
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

  # Load a user-uploaded file and reset feature state for the new dataset.
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

  # Reactive data pipeline:
  # raw_data -> cleaned_data -> preprocessed_data -> featured_data -> filtered_data
  # I separated the workflow this way so each tab corresponds to one stage of data preparation and exploration.
  high_missing_candidates <- reactive({
    df <- raw_data()
    req(df)

    reference_df <- prepare_cleaning_reference(df, input$standardize_text)
    high_missing_columns(reference_df, threshold = 0.8)
  })

  cleaned_data <- reactive({
    df <- raw_data()
    req(df)
    apply_cleaning(
      df = df,
      standardize_text = input$standardize_text,
      duplicate_action = input$duplicate_action,
      drop_columns = input$high_missing_cols %||% character(0),
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

    # If no filter is selected, use the full transformed dataset.
    filter_col <- input$filter_col
    if (is.null(filter_col) || identical(filter_col, "None") || !(filter_col %in% names(df))) {
      return(df)
    }

    # Numeric filters use a range slider.
    if (is.numeric(df[[filter_col]])) {
      rng <- input$filter_range
      if (is.null(rng) || length(rng) != 2) {
        return(df)
      }
      keep <- !is.na(df[[filter_col]]) & df[[filter_col]] >= rng[1] & df[[filter_col]] <= rng[2]
      return(df[keep, , drop = FALSE])
    }

    # Categorical filters use a multi-select list of levels.
    levels_selected <- input$filter_levels
    if (is.null(levels_selected) || length(levels_selected) == 0) {
      return(df[0, , drop = FALSE])
    }

    keep <- as.character(df[[filter_col]]) %in% levels_selected
    df[keep, , drop = FALSE]
  })

  # Keep the high-missing selector aligned with the current dataset and standardization choice.
  # This means the user only sees columns that currently satisfy the >80% missing-value rule.
  observe({
    flagged_cols <- high_missing_candidates()
    current_selected <- isolate(input$high_missing_cols)

    updateSelectizeInput(
      session,
      "high_missing_cols",
      choices = flagged_cols,
      selected = intersect(current_selected %||% character(0), flagged_cols),
      server = TRUE
    )
  })

  # Keep preprocessing selectors aligned with the current cleaned dataset.
  # This avoids offering columns that no longer exist after cleaning or column removal.
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

  # Keep feature-engineering and EDA selectors aligned with the transformed dataset.
  # This is important because encoding and feature creation can change the available columns.
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

  # Add a new feature recipe from the sidebar controls.
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

  # Remove all saved engineered features.
  observeEvent(input$reset_features, {
    feature_recipes(list())
    feature_message("All engineered features were removed.")
  })

  # ---- 9. Metric Outputs ----
  # These outputs support the metric cards shown above each major tab.
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

  # ---- 10. Load Data Outputs ----
  # Show the current source and a preview of the dataset before any transformations are applied.
  output$source_summary <- renderText({
    paste(source_name(), status_message(), data_overview(raw_data()), sep = "\n")
  })

  output$raw_preview <- renderDT({
    preview_datatable(raw_data())
  })

  # ---- 11. Cleaning Outputs ----
  # Summarize the effect of the user's cleaning choices, including high-missing-column removal.
  output$cleaning_summary <- renderText({
    df_raw <- raw_data()
    df_clean <- cleaned_data()
    duplicate_count <- sum(duplicated(df_raw))
    flagged_cols <- high_missing_candidates()
    removed_cols <- intersect(input$high_missing_cols %||% character(0), flagged_cols)

    paste(
      paste("Before cleaning ->", data_overview(df_raw)),
      paste("After cleaning  ->", data_overview(df_clean)),
      paste("Original duplicate rows detected:", duplicate_count),
      paste(
        "Columns >80% missing:",
        if (length(flagged_cols) == 0) "None" else paste(flagged_cols, collapse = ", ")
      ),
      paste(
        "Removed high-missing columns:",
        if (length(removed_cols) == 0) "None" else paste(removed_cols, collapse = ", ")
      ),
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

  # ---- 12. Preprocessing Outputs ----
  # Summarize the outlier, scaling, and encoding choices selected in the preprocessing tab.
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

  # ---- 13. Feature Engineering Outputs ----
  # These outputs let the user review the feature rules and inspect the transformed dataset.
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

  # Quick histogram to inspect the selected engineered feature.
  output$feature_plot <- renderPlotly({
    df <- featured_data()
    focus <- input$feature_focus

    if (nrow(df) == 0 || is.null(focus) || !(focus %in% names(df)) || !is.numeric(df[[focus]])) {
      return(empty_plotly("Create or select a numeric feature to inspect."))
    }

    plot_ly(data = df, x = formula_from_col(focus), type = "histogram", nbinsx = 30, marker = list(color = "#1f6f78")) |>
      layout(template = "plotly_white", bargap = 0.08)
  })

  # ---- 14. EDA Outputs ----
  # Build a filter widget based on whether the selected column is numeric or categorical.
  # This keeps the filtering interface intuitive for both continuous and categorical variables.
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

  # Main Plotly chart area for the EDA tab.
  # The plot updates based on the selected plot type, variables, grouping, and filters.
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

  # Correlation heatmap is shown only when at least two numeric columns exist.
  # Pairwise complete observations are used so the app can still work with partially missing numeric data.
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

  # ---- 15. Export Outputs ----
  # Final export section for downloading the fully transformed dataset as a CSV file.
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

# ---- 16. App Entry Point ----
shinyApp(ui = ui, server = server)
