library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)

# ---- 1. Global Configuration ----

# Allow larger uploads so the app can handle course-sized datasets.
options(shiny.maxRequestSize = 300 * 1024^2)
# Main title used in the navbar and overall app branding.
app_title <- "Data Wrangling Studio"

# a small helper for Shiny inputs that may be NULL or empty.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ---- 2. Data Loading Helpers ----

# Return one of the built-in example datasets used for testing the app.
# This is useful when the user wants to explore the workflow quickly without uploading a file.
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

# Build the version of the dataset used for all cleaning decisions.
# If standardization is enabled, the missing-value summary and later cleaning steps use the standardized names and values.
prepare_cleaning_reference <- function(df, standardize_text = FALSE) {
  reference_df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  if (isTRUE(standardize_text)) {
    reference_df <- standardize_strings(reference_df)
  }

  reference_df
}

# Input ids for the per-column missing controls are generated from the column order.
# keeps the UI and the cleaning pipeline synchronized even when the user toggles standardization.
missing_strategy_input_id <- function(position) {
  paste0("cmiss_", position)
}

# Build a missing-value summary for columns that currently contain NA values.
# use this same summary both for the sidebar controls and for the user's manual "delete this column" decision.
build_missing_column_info <- function(df) {
  if (ncol(df) == 0) {
    return(
      data.frame(
        column = character(0),
        input_id = character(0),
        missing_count = numeric(0),
        missing_pct = numeric(0),
        is_numeric = logical(0),
        stringsAsFactors = FALSE
      )
    )
  }

  missing_counts <- vapply(df, function(x) sum(is.na(x)), numeric(1))
  missing_pct <- round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2)

  info <- data.frame(
    column = names(df),
    input_id = vapply(seq_along(names(df)), missing_strategy_input_id, character(1)),
    missing_count = missing_counts,
    missing_pct = missing_pct,
    is_numeric = vapply(df, is.numeric, logical(1)),
    stringsAsFactors = FALSE
  )

  info[info$missing_count > 0, , drop = FALSE]
}

# Return the valid strategy choices for one column.
# Numeric and categorical variables are separated so the sidebar feels more natural to use.
missing_strategy_choices <- function(is_numeric) {
  if (isTRUE(is_numeric)) {
    c("keep", "drop_rows", "median", "mean", "zero")
  } else {
    c("keep", "drop_rows", "mode", "missing_label")
  }
}

# Apply the column-specific missing-value rules chosen in the sidebar.
# each column treated differently.
apply_column_missing_handling <- function(df, missing_info, strategy_lookup) {
  if (nrow(missing_info) == 0) {
    return(df)
  }

  cleaned <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  for (row_index in seq_len(nrow(missing_info))) {
    col_name <- missing_info$column[[row_index]]
    input_id <- missing_info$input_id[[row_index]]

    if (!(col_name %in% names(cleaned)) || !anyNA(cleaned[[col_name]])) {
      next
    }

    strategy <- strategy_lookup[[input_id]] %||% "keep"

    if (identical(strategy, "keep")) {
      next
    }

    if (identical(strategy, "drop_rows")) {
      cleaned <- cleaned[!is.na(cleaned[[col_name]]), , drop = FALSE]
      next
    }

    if (is.numeric(cleaned[[col_name]])) {
      fill_value <- switch(
        strategy,
        "mean" = mean(cleaned[[col_name]], na.rm = TRUE),
        "zero" = 0,
        stats::median(cleaned[[col_name]], na.rm = TRUE)
      )

      if (is.nan(fill_value)) {
        fill_value <- 0
      }

      cleaned[[col_name]][is.na(cleaned[[col_name]])] <- fill_value
    } else {
      if (is.factor(cleaned[[col_name]])) {
        cleaned[[col_name]] <- as.character(cleaned[[col_name]])
      }

      cleaned[[col_name]][is.na(cleaned[[col_name]])] <- if (identical(strategy, "mode")) {
        mode_value(cleaned[[col_name]])
      } else {
        "Missing"
      }
    }
  }

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
    ),
    class = "compact stripe"
  )
}

# Empty Plotly placeholder used when current selections are not valid for plotting.
empty_plotly <- function(message) {
  plotly::plot_ly(
    x = c(0),
    y = c(0),
    type = "scatter",
    mode = "markers",
    hoverinfo = "none",
    showlegend = FALSE,
    marker = list(opacity = 0)
  ) |>
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
          font = list(size = 15, color = "#64748b")
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

# this just used to keep the app quiet when a selected variable has no usable values.(let console not keep warning)
has_non_missing_values <- function(x) {
  any(!is.na(x))
}

has_complete_numeric_pair <- function(x, y) {
  sum(stats::complete.cases(x, y)) > 0
}

# A compact stats helper reused in the richer summary cards.make UI summaries stay easy to read.
dataset_quick_stats <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(rows = 0, cols = 0, numeric = 0, non_numeric = 0, missing = 0, duplicates = 0))
  }

  numeric_count <- sum(vapply(df, is.numeric, logical(1)))

  list(
    rows = nrow(df),
    cols = ncol(df),
    numeric = numeric_count,
    non_numeric = ncol(df) - numeric_count,
    missing = sum(is.na(df)),
    duplicates = sum(duplicated(df))
  )
}

# Small UI helper for the metric cards shown at the top of each tab.
# give users quick feedback about how the dataset changes across the workflow.
metric_card <- function(title, value_output_id, subtitle = NULL, icon_char = NULL) {
  div(
    class = "metric-box",
    if (!is.null(icon_char)) div(class = "metric-icon", icon_char),
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(value_output_id, inline = TRUE)),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

# Small UI helper for the workflow steps in the User Guide tab.
# this helper is to keep the User Guide cleaner and more visually consistent.
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

# It shows users how each stage changes the data without making them read raw text lines.
summary_comparison_ui <- function(before, after, method_tags = NULL) {
  make_delta <- function(b, a, lower_is_better = FALSE) {
    delta <- a - b

    if (delta == 0) {
      return(tags$span(class = "delta-neutral", "\u2014"))
    }

    improved <- if (lower_is_better) delta < 0 else delta > 0
    cls <- if (improved) "delta-good" else "delta-bad"
    arrow <- if (delta > 0) "\u25b2" else "\u25bc"

    tags$span(class = cls, paste(arrow, format(abs(delta), big.mark = ",")))
  }

  miss_before_style <- if (before$missing > 0) "color:#d69e2e;font-weight:700;" else "color:#374151;"
  miss_after_style <- if (after$missing > 0) "color:#d69e2e;font-weight:700;" else "color:#16a34a;font-weight:700;"

  rows_data <- list(
    list(label = "Rows", b = format(before$rows, big.mark = ","), a = format(after$rows, big.mark = ","), delta = make_delta(before$rows, after$rows)),
    list(label = "Columns", b = before$cols, a = after$cols, delta = make_delta(before$cols, after$cols)),
    list(label = "Numeric", b = before$numeric, a = after$numeric, delta = make_delta(before$numeric, after$numeric)),
    list(label = "Non-numeric", b = before$non_numeric, a = after$non_numeric, delta = make_delta(before$non_numeric, after$non_numeric)),
    list(
      label = "Missing",
      b = tags$span(style = miss_before_style, format(before$missing, big.mark = ",")),
      a = tags$span(style = miss_after_style, format(after$missing, big.mark = ",")),
      delta = make_delta(before$missing, after$missing, lower_is_better = TRUE)
    )
  )

  tagList(
    tags$table(
      class = "summary-table",
      tags$thead(
        tags$tr(
          tags$th(class = "st-metric", "Metric"),
          tags$th(class = "st-before", "Before"),
          tags$th(class = "st-after", "After"),
          tags$th(class = "st-change", "Change")
        )
      ),
      tags$tbody(
        lapply(rows_data, function(row) {
          tags$tr(
            tags$td(class = "st-metric", row$label),
            tags$td(class = "st-before", row$b),
            tags$td(class = "st-after", row$a),
            tags$td(class = "st-change", row$delta)
          )
        })
      )
    ),
    if (!is.null(method_tags) && length(method_tags) > 0) {
      div(
        class = "method-tag-row",
        lapply(names(method_tags), function(key) {
          value <- method_tags[[key]]
          cls <- if (identical(value, "none") || identical(value, "keep") || identical(value, "no")) {
            "method-tag-neutral"
          } else {
            "method-tag-active"
          }

          tags$span(class = paste("method-tag", cls), paste0(key, ": ", value))
        })
      )
    }
  )
}

# Central CSS block for the polished UI version.
APP_CSS <- "
body {
  background: #f0f4f8;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  font-size: 16px;
}
.navbar {
  box-shadow: 0 4px 14px rgba(15,23,42,0.1);
  font-size: 16px;
}
.navbar-brand {
  font-weight: 800;
  letter-spacing: -0.02em;
  font-size: 18px;
}
.navbar-nav .nav-link {
  font-size: 15px;
}
.hero-box {
  margin: 18px 0 22px;
  padding: 30px 32px;
  border-radius: 20px;
  background: linear-gradient(135deg, #0f2942 0%, #1f6f78 55%, #2ecc8b 100%);
  color: white;
  box-shadow: 0 16px 40px rgba(18,52,77,0.22);
}
.hero-box h2 {
  margin: 0 0 10px;
  font-weight: 800;
  letter-spacing: -0.03em;
  font-size: 28px;
}
.hero-box p {
  margin: 0;
  font-size: 16px;
  line-height: 1.65;
  opacity: 0.93;
}
.section-card {
  background: white;
  border-radius: 16px;
  padding: 20px 22px;
  margin-bottom: 18px;
  box-shadow: 0 4px 16px rgba(15,23,42,0.06);
  border: 1px solid #e8edf2;
}
.section-card h3 {
  margin-top: 0;
  color: #0f2942;
  font-size: 22px;
}
.section-card h4 {
  margin-top: 0;
  color: #0f2942;
  font-size: 19px;
}
.panel-note {
  color: #64748b;
  font-size: 15px;
  line-height: 1.55;
  margin-bottom: 12px;
}
.control-group {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 14px 16px;
  margin-bottom: 12px;
}
.control-group h5 {
  margin: 0 0 6px;
  color: #0f2942;
  font-weight: 700;
  font-size: 16px;
}
.control-group label {
  font-size: 15px;
}
.control-group .help-block,
.control-group .shiny-input-container small {
  font-size: 14px;
}
.status-banner {
  background: linear-gradient(90deg, rgba(31,111,120,.12), rgba(31,111,120,.04));
  border: 1px solid rgba(31,111,120,.18);
  border-radius: 12px;
  padding: 12px 16px;
  color: #0f2942;
  margin-bottom: 14px;
  font-size: 15px;
}
.metric-box {
  background: white;
  border-radius: 16px;
  padding: 18px 20px;
  box-shadow: 0 4px 16px rgba(15,23,42,0.07);
  border: 1px solid #e8edf2;
  min-height: 110px;
  margin-bottom: 18px;
  transition: box-shadow .2s;
}
.metric-box:hover {
  box-shadow: 0 8px 28px rgba(15,23,42,0.13);
}
.metric-title {
  color: #64748b;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: .07em;
  margin-bottom: 8px;
  font-weight: 700;
}
.metric-value {
  color: #0f172a;
  font-size: 2rem;
  font-weight: 800;
  line-height: 1.15;
}
.metric-subtitle {
  color: #94a3b8;
  font-size: 14px;
  margin-top: 6px;
}
.metric-icon {
  font-size: 1.4rem;
  margin-bottom: 4px;
  opacity: 0.7;
}
.workflow-step {
  display: flex;
  gap: 14px;
  align-items: flex-start;
  padding: 14px 0;
  border-bottom: 1px solid #f1f5f9;
}
.workflow-step:last-child {
  border-bottom: none;
  padding-bottom: 0;
}
.step-badge {
  width: 36px;
  height: 36px;
  border-radius: 50%;
  background: linear-gradient(135deg, #1f6f78, #2ecc8b);
  color: white;
  font-weight: 800;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  font-size: 16px;
}
.step-copy h4 {
  margin: 0 0 4px;
  font-size: 17px;
  color: #1e293b;
}
.step-copy p {
  margin: 0;
  color: #64748b;
  font-size: 15px;
}
.summary-table {
  width: 100%;
  border-collapse: collapse;
  border-radius: 12px;
  overflow: hidden;
  border: 1px solid #e2e8f0;
  margin-bottom: 14px;
  font-size: 15px;
}
.summary-table thead tr {
  background: #f1f5f9;
}
.summary-table thead th {
  padding: 10px 16px;
  font-size: 12px;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: .07em;
  color: #64748b;
  border-bottom: 2px solid #e2e8f0;
}
.summary-table tbody tr {
  border-bottom: 1px solid #f1f5f9;
}
.summary-table tbody tr:last-child {
  border-bottom: none;
}
.summary-table tbody tr:hover {
  background: #fafbfc;
}
.summary-table td {
  padding: 9px 16px;
  vertical-align: middle;
}
.st-metric {
  color: #475569;
  font-weight: 600;
  font-size: 15px;
  width: 36%;
}
.st-before {
  color: #374151;
  font-weight: 600;
  text-align: right;
  width: 20%;
}
.st-after {
  font-weight: 700;
  text-align: right;
  width: 20%;
}
.st-change {
  text-align: right;
  width: 24%;
  padding-right: 20px;
}
.delta-good {
  color: #16a34a;
  font-size: 13px;
  font-weight: 700;
}
.delta-bad {
  color: #dc2626;
  font-size: 13px;
  font-weight: 700;
}
.delta-neutral {
  color: #94a3b8;
  font-size: 13px;
}
.method-tag-row {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-top: 6px;
  margin-bottom: 4px;
}
.method-tag {
  display: inline-block;
  padding: 5px 12px;
  border-radius: 20px;
  font-size: 13px;
  font-weight: 600;
}
.method-tag-neutral {
  background: #f1f5f9;
  color: #64748b;
  border: 1px solid #e2e8f0;
}
.method-tag-active {
  background: #dbeafe;
  color: #1e40af;
  border: 1px solid #bfdbfe;
}
.stat-insight-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  margin-top: 4px;
}
.stat-insight-card {
  flex: 1;
  min-width: 105px;
  background: #f8fafc;
  border-radius: 10px;
  padding: 11px 14px;
  border: 1px solid #e2e8f0;
  text-align: center;
}
.stat-insight-label {
  font-size: 11px;
  color: #94a3b8;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .06em;
}
.stat-insight-value {
  font-size: 17px;
  font-weight: 800;
  color: #1e293b;
  margin-top: 3px;
}
.missing-warning {
  background: #fffbeb;
  border: 1px solid #fcd34d;
  border-radius: 10px;
  padding: 11px 16px;
  color: #92400e;
  font-size: 15px;
  font-weight: 600;
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 12px;
}
.source-info-card {
  display: flex;
  align-items: center;
  gap: 16px;
  padding: 14px 18px;
  background: #f0fdf4;
  border-radius: 12px;
  border: 1px solid #bbf7d0;
}
.source-info-icon {
  font-size: 2rem;
}
.source-info-name {
  font-weight: 700;
  color: #14532d;
  font-size: 16px;
}
.source-info-meta {
  color: #16a34a;
  font-size: 14px;
}
.btn-primary {
  background: linear-gradient(135deg, #1f6f78, #1a5f67);
  border: none;
  font-weight: 600;
  border-radius: 8px;
  font-size: 15px;
}
.btn-warning {
  color: #7c2d12;
  background: #ffedd5;
  border-color: #fdba74;
  font-weight: 600;
  border-radius: 8px;
  font-size: 15px;
}
.btn-success {
  font-weight: 600;
  border-radius: 8px;
  font-size: 15px;
}
.sidebar-panel .form-group {
  margin-bottom: 10px;
}
.tab-pane {
  padding-top: 6px;
}
.chart-subtitle {
  color: #64748b;
  font-size: 15px;
  margin-bottom: 12px;
}
.selectize-input,
.form-control {
  font-size: 15px !important;
}
.shiny-input-container label {
  font-size: 15px;
  font-weight: 600;
  color: #374151;
}
.help-block,
.checkbox label {
  font-size: 14px;
}
"

# ---- 7. User Interface ----

ui <- navbarPage(
  title = app_title,
  id = "main_tabs",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f78"),
  header = tagList(tags$head(tags$style(HTML(APP_CSS)))),

  # Tab 1: user guide and workflow overview.
  tabPanel(
    "User Guide",
    fluidRow(
      column(
        12,
        div(
          class = "hero-box",
          h2("Data Wrangling Studio"),
          p("Upload a dataset, clean it, preprocess it, create new features, explore it interactively, and export the final result in one guided workflow.")
        )
      )
    ),
    fluidRow(
      column(
        7,
        div(
          class = "section-card",
          h3("Studio Statement"),
          workflow_step("1", "Load data", "Start from a built-in dataset or upload CSV, Excel, JSON, or RDS data so the workflow is easy to test."),
          workflow_step("2", "Clean the dataset", "Standardize names and text, inspect missing-value percentages, choose whether to delete selected columns, and set column-specific missing-value rules."),
          workflow_step("3", "Preprocess variables", "Apply outlier handling, scaling, and encoding to selected columns so the data is ready for analysis."),
          workflow_step("4", "Create new features", "Build derived variables from numeric columns and immediately inspect the updated data."),
          workflow_step("5", "Explore and export", "Use interactive plots, summary cards, filters, and the correlation heatmap before downloading the final dataset.")
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
            tags$li("Missing-value percentage summary with user-selected column removal"),
            tags$li("Interactive cleaning and preprocessing controls"),
            tags$li("Feature engineering with instant preview"),
            tags$li("Interactive Plotly charts, summary statistics, and a correlation heatmap"),
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
      column(3, metric_card("Rows", "metric_rows", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols", icon_char = "\U0001F5C2")),
      column(3, metric_card("Missing Values", "metric_missing", icon_char = "\u26A0\uFE0F")),
      column(3, metric_card("Engineered Features", "metric_features", icon_char = "\u2728"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Start here."), br(), "Choose a built-in dataset or upload your own file to begin the workflow."),
        div(
          class = "control-group",
          h5("\U0001F4C2 Upload a dataset"),
          p(class = "panel-note", "Supported file types: CSV, XLSX, XLS, JSON, and RDS. The app allows larger uploads for course project datasets."),
          fileInput("upload_file", "Choose file", accept = c(".csv", ".xlsx", ".xls", ".json", ".rds"))
        ),
        div(
          class = "control-group",
          h5("\U0001F4E6 Use a built-in dataset"),
          p(class = "panel-note", "Useful for testing the workflow quickly without preparing a file first."),
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
          actionButton("load_builtin", "Load Built-in Dataset", class = "btn-primary btn-sm")
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h4("Current dataset"),
          p(class = "panel-note", "This card shows the current source, status message, and a quick snapshot of the loaded data."),
          uiOutput("source_summary_ui")
        ),
        div(
          class = "section-card",
          h4("Raw data preview"),
          p(class = "panel-note", "Preview the first rows before any transformations are applied."),
          DTOutput("raw_preview")
        )
      )
    )
  ),

  # Tab 3: cleaning controls and cleaned-data preview.
  tabPanel(
    "Cleaning",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_clean", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols_clean", icon_char = "\U0001F5C2")),
      column(3, metric_card("Missing Values", "metric_missing_clean", icon_char = "\u26A0\uFE0F")),
      column(3, metric_card("Duplicates", "metric_duplicates_clean", icon_char = "\U0001F4CB"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Cleaning controls."), br(), "Standardize messy text, inspect missingness, and decide how duplicates and missing values should be handled."),
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
          p(class = "panel-note", "Columns with missing values will appear here automatically. You can review each column's missing percentage, optionally delete selected columns, and choose different strategies for numeric and categorical variables."),
          uiOutput("per_col_missing_ui")
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h4("Cleaning summary"),
          p(class = "panel-note", "This summary compares the dataset before and after the current cleaning choices."),
          uiOutput("cleaning_summary_ui")
        ),
        div(
          class = "section-card",
          h4("Missing-value profile"),
          p(class = "panel-note", "Use the chart and table below to see which variables still have missing values after cleaning."),
          plotlyOutput("missing_bar_plot", height = "260px"),
          tags$br(),
          DTOutput("missing_profile_table")
        ),
        div(
          class = "section-card",
          h4("Cleaned data preview"),
          p(class = "panel-note", "Preview the cleaned dataset before moving to preprocessing."),
          DTOutput("cleaned_preview")
        )
      )
    )
  ),

  # Tab 4: preprocessing controls and processed-data preview.
  tabPanel(
    "Preprocessing",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_pre", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols_pre", icon_char = "\U0001F5C2")),
      column(3, metric_card("Numeric Columns", "metric_numeric_pre", icon_char = "\U0001F522")),
      column(3, metric_card("Missing Values", "metric_missing_pre", icon_char = "\u26A0\uFE0F"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Preprocessing controls."), br(), "Apply transformations that prepare variables for analysis, modeling, and clearer comparisons."),
        div(
          class = "control-group",
          h5("\U0001F4A5 Outlier handling"),
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
          h5("\u2696\uFE0F Scaling"),
          p(class = "panel-note", "Standard, min-max, and robust scaling are available for selected numeric columns."),
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
          h5("\U0001F3F7\uFE0F Categorical encoding"),
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
        div(
          class = "section-card",
          h4("Preprocessing summary"),
          p(class = "panel-note", "This card gives a before-and-after view of outlier handling, scaling, and encoding."),
          uiOutput("preprocessing_summary_ui")
        ),
        div(
          class = "section-card",
          h4("Processed data preview"),
          p(class = "panel-note", "This preview reflects the dataset after outlier handling, scaling, and encoding."),
          DTOutput("processed_preview")
        )
      )
    )
  ),

  # Tab 5: feature engineering workflow and feature preview.
  tabPanel(
    "Feature Engineering",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_feat", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols_feat", icon_char = "\U0001F5C2")),
      column(3, metric_card("Saved Feature Rules", "metric_feature_rules", icon_char = "\u2728")),
      column(3, metric_card("Missing Values", "metric_missing_feat", icon_char = "\u26A0\uFE0F"))
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
          actionButton("add_feature", "Add Feature", class = "btn-primary btn-sm"),
          tags$span(style = "display:inline-block; width: 8px;"),
          actionButton("reset_features", "Reset Engineered Features", class = "btn-warning btn-sm")
        ),
        div(
          class = "control-group",
          h5("Inspect a feature"),
          p(class = "panel-note", "Choose a numeric feature to preview its current distribution."),
          selectInput("feature_focus", "Feature to inspect", choices = NULL)
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h4("Feature engineering status"),
          p(class = "panel-note", "This panel confirms whether a feature was added and how many saved feature rules are active."),
          uiOutput("feature_summary_ui")
        ),
        div(
          class = "section-card",
          h4("Feature recipe list"),
          p(class = "panel-note", "This list records the feature rules you have created so far, and each one can be removed individually."),
          uiOutput("feature_recipe_table")
        ),
        div(
          class = "section-card",
          h4("Feature preview"),
          p(class = "panel-note", "Preview the dataset after feature engineering has been applied."),
          DTOutput("featured_preview")
        ),
        div(
          class = "section-card",
          h4("Feature distribution"),
          p(class = "panel-note", "Inspect the selected numeric feature to see its current shape."),
          plotlyOutput("feature_plot", height = "380px")
        )
      )
    )
  ),

  # Tab 6: EDA controls, plots, and summary tables.
  tabPanel(
    "EDA / Visualization",
    fluidRow(
      column(3, metric_card("Rows After Filter", "metric_rows_eda", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols_eda", icon_char = "\U0001F5C2")),
      column(3, metric_card("Numeric Columns", "metric_numeric_eda", icon_char = "\U0001F522")),
      column(3, metric_card("Missing Values", "metric_missing_eda", icon_char = "\u26A0\uFE0F"))
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "status-banner", strong("Explore the data interactively."), br(), "Set up a plot, optionally group by color, and use filters to focus on a subset of the dataset."),
        div(
          class = "control-group",
          h5("\U0001F4C8 Plot setup"),
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
          h5("\U0001F3A8 Grouping"),
          p(class = "panel-note", "Optionally color the plot by another variable to compare groups."),
          selectInput("color_var", "Color grouping", choices = "None", selected = "None")
        ),
        div(
          class = "control-group",
          h5("\U0001F50D Filter"),
          p(class = "panel-note", "Filter the dataset before plotting to focus on a range or selected categories."),
          selectInput("filter_col", "Optional filter column", choices = "None", selected = "None"),
          uiOutput("filter_ui")
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h4("Interactive visualization"),
          p(class = "panel-note chart-subtitle", "Hover for details, drag to zoom, and double-click to reset the view."),
          plotlyOutput("eda_plot", height = "460px")
        ),
        div(
          class = "section-card",
          h4("Statistical insights"),
          p(class = "panel-note", "This panel summarizes the currently selected X variable inside the filtered dataset."),
          uiOutput("stat_insight_ui")
        ),
        div(
          class = "section-card",
          h4("Summary statistics"),
          p(class = "panel-note", "Review summary measures for the filtered data currently in view."),
          DTOutput("summary_stats_table")
        ),
        div(
          class = "section-card",
          h4("Correlation heatmap"),
          p(class = "panel-note", "This heatmap is available when at least two numeric variables are present."),
          plotlyOutput("correlation_heatmap", height = "500px")
        )
      )
    )
  ),

  # Tab 7: final export area.
  tabPanel(
    "Export / Download",
    fluidRow(
      column(3, metric_card("Rows", "metric_rows_export", icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns", "metric_cols_export", icon_char = "\U0001F5C2")),
      column(3, metric_card("Engineered Features", "metric_features_export", icon_char = "\u2728")),
      column(3, metric_card("Missing Values", "metric_missing_export", icon_char = "\u26A0\uFE0F"))
    ),
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("Export the transformed dataset"),
          p(class = "panel-note", "Download includes all selected cleaning, preprocessing, and feature-engineering steps from the current workflow."),
          uiOutput("export_summary_ui"),
          tags$br(),
          downloadButton("download_processed_data", "Download Current Data as CSV", class = "btn-success")
        )
      )
    ),
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h4("Current export preview"),
          p(class = "panel-note", "Preview the dataset that will be downloaded if you export now."),
          DTOutput("export_preview")
        )
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
  # each tab corresponds to one stage of data preparation and exploration.
  cleaning_reference_data <- reactive({
    df <- raw_data()
    req(df)
    prepare_cleaning_reference(df, input$standardize_text)
  })

  # This summary drives the sidebar missing-value controls.
  # let the user see how much missing data each column has before deciding whether to delete or impute it.
  missing_column_info <- reactive({
    reference_df <- cleaning_reference_data()
    build_missing_column_info(reference_df)
  })

  cleaned_data <- reactive({
    cleaned <- cleaning_reference_data()
    info <- missing_column_info()

    if (identical(input$duplicate_action, "remove")) {
      cleaned <- unique(cleaned)
    } else if (identical(input$duplicate_action, "flag")) {
      cleaned$duplicate_flag <- duplicated(cleaned)
    }

    drop_columns <- intersect(input$drop_missing_cols %||% character(0), names(cleaned))
    if (length(drop_columns) > 0) {
      cleaned <- cleaned[setdiff(names(cleaned), drop_columns)]
    }

    strategy_lookup <- setNames(
      lapply(info$input_id, function(id) input[[id]] %||% "keep"),
      info$input_id
    )

    apply_column_missing_handling(cleaned, info, strategy_lookup)
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
      return(df)
    }

    keep <- as.character(df[[filter_col]]) %in% levels_selected
    df[keep, , drop = FALSE]
  })


  # avoids offering columns that no longer exist after cleaning or column removal.
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

  # Register one delete button per saved feature recipe.
  observe({
    recipes <- feature_recipes()

    lapply(seq_along(recipes), function(i) {
      local({
        idx <- i

        observeEvent(input[[paste0("del_feat_", idx)]], {
          current_recipes <- feature_recipes()

          if (idx > length(current_recipes)) {
            return()
          }

          removed_name <- current_recipes[[idx]]$name
          current_recipes[[idx]] <- NULL
          feature_recipes(current_recipes)
          feature_message(paste("Removed feature", shQuote(removed_name), "from the workflow."))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
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
  # The final UI uses a richer summary card
  output$source_summary_ui <- renderUI({
    stats <- dataset_quick_stats(raw_data())

    div(
      class = "source-info-card",
      div(class = "source-info-icon", "\U0001F4BE"),
      div(
        div(class = "source-info-name", source_name()),
        div(class = "source-info-meta", status_message()),
        tags$br(),
        div(
          class = "method-tag-row",
          tags$span(class = "method-tag method-tag-active", paste("Rows:", format(stats$rows, big.mark = ","))),
          tags$span(class = "method-tag method-tag-active", paste("Cols:", stats$cols)),
          tags$span(class = "method-tag method-tag-neutral", paste("Numeric:", stats$numeric)),
          tags$span(class = "method-tag method-tag-neutral", paste("Non-numeric:", stats$non_numeric)),
          if (stats$missing > 0) {
            tags$span(
              class = "method-tag",
              style = "background:#fef3c7;color:#92400e;border:1px solid #fcd34d;",
              paste("\u26A0\uFE0F Missing:", format(stats$missing, big.mark = ","))
            )
          } else {
            tags$span(class = "method-tag method-tag-neutral", "\u2705 No missing values")
          }
        )
      )
    )
  })

  output$raw_preview <- renderDT({
    preview_datatable(raw_data())
  })

  # ---- 11. Cleaning Outputs ----
  # Build the dynamic missing-value controls shown in the sidebar.
  # separate numeric and categorical columns so the user can pick more sensible strategies for each type.
  output$per_col_missing_ui <- renderUI({
    info <- missing_column_info()

    if (nrow(info) == 0) {
      return(p(class = "panel-note", "\u2705 No missing values found."))
    }

    delete_choices <- setNames(
      info$column,
      paste0(info$column, " (", info$missing_count, " missing, ", info$missing_pct, "% missing)")
    )

    numeric_info <- info[info$is_numeric, , drop = FALSE]
    categorical_info <- info[!info$is_numeric, , drop = FALSE]

    build_inputs <- function(info_df) {
      lapply(seq_len(nrow(info_df)), function(i) {
        row <- info_df[i, , drop = FALSE]
        selectInput(
          row$input_id[[1]],
          label = paste0(row$column[[1]], " (", row$missing_count[[1]], " missing, ", row$missing_pct[[1]], "%)"),
          choices = missing_strategy_choices(row$is_numeric[[1]]),
          selected = input[[row$input_id[[1]]]] %||% "keep"
        )
      })
    }

    tagList(
      selectizeInput(
        "drop_missing_cols",
        "Optional columns to remove based on missing ratio",
        choices = delete_choices,
        selected = intersect(input$drop_missing_cols %||% character(0), info$column),
        multiple = TRUE
      ),
      helpText("The percentages above help you decide whether a column should be removed before the app handles the remaining missing values."),
      if (nrow(numeric_info) > 0) {
        tagList(
          tags$hr(),
          h5("Numeric columns"),
          build_inputs(numeric_info)
        )
      },
      if (nrow(categorical_info) > 0) {
        tagList(
          tags$hr(),
          h5("Categorical columns"),
          build_inputs(categorical_info)
        )
      }
    )
  })

  # This richer comparison card explains what changed after cleaning and summarizes the custom missing-value decisions.
  output$cleaning_summary_ui <- renderUI({
    before <- dataset_quick_stats(raw_data())
    after <- dataset_quick_stats(cleaned_data())
    info <- missing_column_info()
    removed_cols <- intersect(input$drop_missing_cols %||% character(0), info$column)
    active_missing_rules <- if (nrow(info) == 0) {
      0
    } else {
      sum(vapply(info$input_id, function(id) !identical(input[[id]] %||% "keep", "keep"), logical(1)))
    }

    tagList(
      summary_comparison_ui(
        before,
        after,
        method_tags = list(
          "Text std." = if (isTRUE(input$standardize_text)) "yes" else "no",
          "Duplicates" = input$duplicate_action,
          "Missing rules" = if (nrow(info) == 0 || active_missing_rules == 0) "none" else paste(active_missing_rules, "customized")
        )
      ),
      div(
        class = "method-tag-row",
        tags$span(
          class = "method-tag method-tag-neutral",
          paste(
            "Columns with missing values:",
            if (nrow(info) == 0) "None" else nrow(info)
          )
        ),
        tags$span(
          class = if (length(removed_cols) > 0) "method-tag method-tag-active" else "method-tag method-tag-neutral",
          paste(
            "Removed columns:",
            if (length(removed_cols) == 0) "None" else paste(removed_cols, collapse = ", ")
          )
        )
      )
    )
  })

  output$missing_bar_plot <- renderPlotly({
    profile_df <- build_missing_profile(cleaned_data())
    profile_missing <- profile_df[profile_df$missing > 0, , drop = FALSE]

    if (nrow(profile_missing) == 0) {
      return(empty_plotly("\u2705 No missing values detected in the cleaned dataset."))
    }

    profile_missing <- profile_missing[order(profile_missing$missing_pct, decreasing = TRUE), , drop = FALSE]

    plot_ly(
      data = profile_missing,
      x = ~reorder(column, missing_pct),
      y = ~missing_pct,
      type = "bar",
      marker = list(color = "#f59e0b", line = list(color = "#d97706", width = 1)),
      text = ~paste0(missing, " rows (", missing_pct, "%)"),
      hovertemplate = "<b>%{x}</b><br>Missing: %{text}<extra></extra>"
    ) |>
      layout(
        template = "plotly_white",
        xaxis = list(title = "Column", tickangle = -30),
        yaxis = list(title = "Missing %", rangemode = "tozero"),
        title = list(text = "Missing Values by Column", font = list(size = 14)),
        bargap = 0.25,
        margin = list(b = 80)
      )
  })

  output$missing_profile_table <- renderDT({
    preview_datatable(build_missing_profile(cleaned_data()), n = 20)
  })

  output$cleaned_preview <- renderDT({
    preview_datatable(cleaned_data())
  })

  # ---- 12. Preprocessing Outputs ----
  # This summary emphasizes how preprocessing changes the dataset and also warns if missing values still remain.
  output$preprocessing_summary_ui <- renderUI({
    before <- dataset_quick_stats(cleaned_data())
    after <- dataset_quick_stats(preprocessed_data())

    missing_warning <- if (after$missing > 0) {
      div(
        class = "missing-warning",
        "\u26A0\uFE0F",
        paste(
          format(after$missing, big.mark = ","),
          "missing value(s) still remain after preprocessing. You may want to revisit the Cleaning tab first."
        )
      )
    } else {
      NULL
    }

    tagList(
      missing_warning,
      summary_comparison_ui(
        before,
        after,
        method_tags = list(
          "Outlier" = input$outlier_method,
          "Scaling" = input$scaling_method,
          "Encoding" = input$encoding_method
        )
      )
    )
  })

  output$processed_preview <- renderDT({
    preview_datatable(preprocessed_data())
  })

  # ---- 13. Feature Engineering Outputs ----
  # These outputs let the user review the feature rules and inspect the transformed dataset.
  output$feature_summary_ui <- renderUI({
    recipes <- feature_recipes()
    current_stats <- dataset_quick_stats(featured_data())

    div(
      class = if (length(recipes) > 0) "source-info-card" else "status-banner",
      div(if (length(recipes) > 0) "\u2728" else "\U0001F4CB", style = "font-size:1.5rem;"),
      div(
        div(style = "font-weight:700;color:#1e293b;", feature_message()),
        div(
          style = "color:#64748b;font-size:.85rem;margin-top:4px;",
          paste0(
            "Current dataset: ",
            format(current_stats$rows, big.mark = ","),
            " rows x ",
            current_stats$cols,
            " columns | ",
            length(recipes),
            " feature rule(s) active"
          )
        )
      )
    )
  })

  output$feature_recipe_table <- renderUI({
    recipes <- feature_recipes()

    if (length(recipes) == 0) {
      return(p(class = "panel-note", "No feature recipes have been added yet."))
    }

    tagList(
      lapply(seq_along(recipes), function(i) {
        recipe <- recipes[[i]]

        div(
          style = "display:flex;align-items:center;justify-content:space-between;padding:8px 12px;margin-bottom:6px;background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;",
          div(
            tags$b(recipe$name),
            tags$span(
              style = "color:#64748b;font-size:13px;margin-left:10px;",
              paste0(
                recipe$operation,
                "(",
                recipe$col1,
                if (!is.null(recipe$col2) && nzchar(recipe$col2)) paste0(", ", recipe$col2) else "",
                ")"
              )
            )
          ),
          actionButton(
            paste0("del_feat_", i),
            "\u2715",
            class = "btn btn-sm btn-danger",
            style = "padding:2px 8px;font-size:12px;"
          )
        )
      })
    )
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

    if (!has_non_missing_values(df[[focus]])) {
      return(empty_plotly("The selected feature has no non-missing values to plot."))
    }

    plot_ly(
      data = df,
      x = formula_from_col(focus),
      type = "histogram",
      nbinsx = 30,
      marker = list(color = "#1f6f78", line = list(color = "#145d65", width = 0.8))
    ) |>
      layout(
        template = "plotly_white",
        bargap = 0.06,
        title = list(text = paste("Distribution of", focus), font = list(size = 14)),
        xaxis = list(title = focus),
        yaxis = list(title = "Count")
      )
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
      if (!has_non_missing_values(df[[x_var]])) {
        return(empty_plotly("The selected X variable has no non-missing values to plot."))
      }

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
      if (!has_non_missing_values(df[[target_y]])) {
        return(empty_plotly("The selected numeric variable has no non-missing values for a box plot."))
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
      if (!is.numeric(df[[x_var]]) || !is.numeric(df[[y_var]])) {
        return(empty_plotly("Scatter plots work best when both X and Y variables are numeric."))
      }
      if (!has_complete_numeric_pair(df[[x_var]], df[[y_var]])) {
        return(empty_plotly("The selected X and Y variables do not have enough complete observations for a scatter plot."))
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
      if (!has_non_missing_values(df[[x_var]])) {
        return(empty_plotly("The selected X variable has no non-missing values for a bar chart."))
      }

      counts <- as.data.frame(table(df[[x_var]], useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c(x_var, "count")
      p <- plot_ly(data = counts, x = formula_from_col(x_var), y = ~count, type = "bar")
      return(layout(p, template = "plotly_white"))
    }

    if (!has_non_missing_values(df[[y_var]])) {
      return(empty_plotly("The selected Y variable has no non-missing values for a bar chart."))
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

  # The insight panel explain one selected variable without forcing the user to read the whole summary table.
  output$stat_insight_ui <- renderUI({
    df <- filtered_data()
    x_var <- input$x_var

    if (is.null(x_var) || !(x_var %in% names(df))) {
      return(helpText("Select a variable to see quick insights."))
    }

    values <- df[[x_var]]

    if (is.numeric(values) && any(!is.na(values))) {
      valid_values <- values[!is.na(values)]
      variance <- mean((valid_values - mean(valid_values))^2)
      skewness <- if (variance == 0) 0 else mean((valid_values - mean(valid_values))^3) / (variance^(3 / 2))

      return(
        div(
          class = "stat-insight-grid",
          div(class = "stat-insight-card", div(class = "stat-insight-label", "N"), div(class = "stat-insight-value", format(length(valid_values), big.mark = ","))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Mean"), div(class = "stat-insight-value", round(mean(valid_values), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Median"), div(class = "stat-insight-value", round(stats::median(valid_values), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Std Dev"), div(class = "stat-insight-value", round(stats::sd(valid_values), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Min"), div(class = "stat-insight-value", round(min(valid_values), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Max"), div(class = "stat-insight-value", round(max(valid_values), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Q1"), div(class = "stat-insight-value", round(stats::quantile(valid_values, 0.25), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Q3"), div(class = "stat-insight-value", round(stats::quantile(valid_values, 0.75), 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Skewness"), div(class = "stat-insight-value", round(skewness, 3))),
          div(class = "stat-insight-card", div(class = "stat-insight-label", "Missing"), div(class = "stat-insight-value", sum(is.na(values))))
        )
      )
    }

    freq_table <- sort(table(as.character(values[!is.na(values)])), decreasing = TRUE)
    top_value <- if (length(freq_table) > 0) names(freq_table)[1] else "N/A"
    top_count <- if (length(freq_table) > 0) unname(freq_table[[1]]) else 0

    div(
      class = "stat-insight-grid",
      div(class = "stat-insight-card", div(class = "stat-insight-label", "N"), div(class = "stat-insight-value", format(sum(!is.na(values)), big.mark = ","))),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Unique"), div(class = "stat-insight-value", length(freq_table))),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Missing"), div(class = "stat-insight-value", sum(is.na(values)))),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Mode"), div(class = "stat-insight-value", top_value)),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Mode Freq"), div(class = "stat-insight-value", top_count))
    )
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

    usable_numeric <- numeric_df[
      vapply(
        numeric_df,
        function(x) sum(!is.na(x)) >= 2 && !isTRUE(all(stats::sd(x, na.rm = TRUE) %in% c(0, NA))),
        logical(1)
      )
    ]

    if (ncol(usable_numeric) < 2) {
      return(empty_plotly("The numeric columns do not contain enough usable variation for a correlation heatmap."))
    }

    corr_matrix <- round(stats::cor(usable_numeric, use = "pairwise.complete.obs"), 2)

    if (all(is.na(corr_matrix))) {
      return(empty_plotly("The correlation matrix could not be computed because the selected numeric variables do not overlap enough."))
    }

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
  output$export_summary_ui <- renderUI({
    stats <- dataset_quick_stats(featured_data())

    div(
      class = "stat-insight-grid",
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Source"), div(class = "stat-insight-value", style = "font-size:.85rem;", source_name())),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Rows"), div(class = "stat-insight-value", format(stats$rows, big.mark = ","))),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Columns"), div(class = "stat-insight-value", stats$cols)),
      div(class = "stat-insight-card", div(class = "stat-insight-label", "Features"), div(class = "stat-insight-value", length(feature_recipes()))),
      div(
        class = "stat-insight-card",
        div(class = "stat-insight-label", "Missing"),
        div(
          class = "stat-insight-value",
          style = if (stats$missing > 0) "color:#d69e2e;" else "color:#16a34a;",
          format(stats$missing, big.mark = ",")
        )
      )
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
