
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)

options(shiny.maxRequestSize = 300 * 1024^2)
app_title <- "Interactive Data Wrangling Studio2"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Data Loading Helpers 
load_builtin_dataset <- function(name) {
  switch(tolower(name %||% "test1"),
    "test2" = mtcars, "mtcars" = mtcars,
    "test3" = ToothGrowth, "toothgrowth" = ToothGrowth,
    iris
  )
}

clean_column_name <- function(x) {
  x <- trimws(as.character(x)); x <- tolower(x)
  x <- gsub("[^[:alnum:]]+", "_", x); x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x); x[x == ""] <- "column"
  needs_prefix <- grepl("^[0-9]", x); x[needs_prefix] <- paste0("x_", x[needs_prefix]); x
}

make_unique_name <- function(name, existing) {
  if (!(name %in% existing)) return(name)
  counter <- 2; candidate <- paste0(name, "_", counter)
  while (candidate %in% existing) { counter <- counter + 1; candidate <- paste0(name, "_", counter) }
  candidate
}

mode_value <- function(x) {
  x <- x[!is.na(x)]; if (length(x) == 0) return("Missing")
  unique_x <- unique(x); unique_x[which.max(tabulate(match(x, unique_x)))]
}

read_uploaded_data <- function(path, original_name) {
  ext <- tolower(tools::file_ext(original_name))
  if (ext == "csv") return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  if (ext %in% c("xlsx","xls")) return(as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE))
  if (ext == "json") {
    payload <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.data.frame(payload)) return(as.data.frame(payload, stringsAsFactors = FALSE))
    if (is.list(payload) && "data" %in% names(payload) && is.data.frame(payload$data))
      return(as.data.frame(payload$data, stringsAsFactors = FALSE))
    candidate <- tryCatch(as.data.frame(payload, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(candidate)) return(candidate)
    stop("JSON could not be converted to a data frame.")
  }
  if (ext == "rds") {
    obj <- readRDS(path)
    if (!inherits(obj, "data.frame")) stop("RDS does not contain a data frame.")
    return(as.data.frame(obj, stringsAsFactors = FALSE))
  }
  stop("Unsupported format. Please upload CSV, Excel, JSON, or RDS.")
}

standardize_strings <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- make.unique(clean_column_name(names(df)), sep = "_")
  missing_tokens <- c("", "na", "n/a", "null", "none", "nan")
  yes_tokens <- c("y", "yes", "true", "1"); no_tokens <- c("n", "no", "false", "0")
  for (col in names(df)) {
    if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
    if (is.character(df[[col]])) {
      values <- trimws(df[[col]]); lowered <- tolower(values)
      values[lowered %in% missing_tokens] <- NA_character_
      non_missing <- unique(lowered[!is.na(values)])
      if (length(non_missing) > 0 && all(non_missing %in% c(yes_tokens, no_tokens))) {
        values[lowered %in% yes_tokens] <- "Yes"; values[lowered %in% no_tokens] <- "No"
      }
      df[[col]] <- values
    }
  }
  df
}

apply_missing_handling <- function(df, strategy, numeric_strategy, categorical_strategy) {
  if (identical(strategy, "keep")) return(df)
  if (identical(strategy, "drop_rows")) return(df[stats::complete.cases(df), , drop = FALSE])
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  categorical_cols <- setdiff(names(df), numeric_cols)
  for (col in numeric_cols) {
    if (!anyNA(df[[col]])) next
    fill_value <- switch(numeric_strategy,
      "mean" = mean(df[[col]], na.rm = TRUE),
      "zero" = 0,
      stats::median(df[[col]], na.rm = TRUE)
    )
    if (is.nan(fill_value)) fill_value <- 0
    df[[col]][is.na(df[[col]])] <- fill_value
  }
  for (col in categorical_cols) {
    if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
    if (!anyNA(df[[col]])) next
    fill_value <- if (identical(categorical_strategy, "missing_label")) "Missing" else mode_value(df[[col]])
    df[[col]][is.na(df[[col]])] <- fill_value
  }
  df
}

apply_cleaning <- function(df, standardize_text, duplicate_action, missing_strategy, numeric_strategy, categorical_strategy) {
  cleaned <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (isTRUE(standardize_text)) cleaned <- standardize_strings(cleaned)
  if (identical(duplicate_action, "remove")) cleaned <- unique(cleaned)
  else if (identical(duplicate_action, "flag")) cleaned$duplicate_flag <- duplicated(cleaned)
  cleaned <- apply_missing_handling(cleaned, strategy = missing_strategy,
    numeric_strategy = numeric_strategy, categorical_strategy = categorical_strategy)
  rownames(cleaned) <- NULL; cleaned
}

#Preprocessing Helpers 
apply_outlier_handling <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) return(df)
  keep_rows <- rep(TRUE, nrow(df))
  numeric_cols <- intersect(target_columns, names(df)[vapply(df, is.numeric, logical(1))])
  for (col in numeric_cols) {
    x <- df[[col]]; x_non_missing <- x[!is.na(x)]
    if (length(x_non_missing) < 4) next
    q1 <- stats::quantile(x_non_missing, 0.25, na.rm = TRUE, names = FALSE)
    q3 <- stats::quantile(x_non_missing, 0.75, na.rm = TRUE, names = FALSE)
    iqr_value <- q3 - q1
    if (is.na(iqr_value) || iqr_value == 0) next
    lower <- q1 - 1.5 * iqr_value; upper <- q3 + 1.5 * iqr_value
    if (identical(method, "cap")) {
      x[x < lower] <- lower; x[x > upper] <- upper; df[[col]] <- x
    } else if (identical(method, "remove")) {
      keep_rows <- keep_rows & (is.na(x) | (x >= lower & x <= upper))
    }
  }
  if (identical(method, "remove")) df <- df[keep_rows, , drop = FALSE]
  rownames(df) <- NULL; df
}

scale_numeric_columns <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) return(df)
  numeric_cols <- intersect(target_columns, names(df)[vapply(df, is.numeric, logical(1))])
  for (col in numeric_cols) {
    x <- df[[col]]; non_missing <- !is.na(x)
    if (!any(non_missing)) next
    if (identical(method, "standard")) {
      center <- mean(x[non_missing]); spread <- stats::sd(x[non_missing])
      df[[col]][non_missing] <- if (is.na(spread) || spread == 0) 0 else (x[non_missing] - center) / spread
    } else if (identical(method, "minmax")) {
      min_x <- min(x[non_missing]); max_x <- max(x[non_missing])
      df[[col]][non_missing] <- if (max_x == min_x) 0 else (x[non_missing] - min_x) / (max_x - min_x)
    } else if (identical(method, "robust")) {
      center <- stats::median(x[non_missing]); spread <- stats::IQR(x[non_missing])
      df[[col]][non_missing] <- if (is.na(spread) || spread == 0) 0 else (x[non_missing] - center) / spread
    }
  }
  df
}

encode_categorical_columns <- function(df, method, target_columns) {
  if (identical(method, "none") || nrow(df) == 0) return(df)
  categorical_candidates <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  categorical_cols <- intersect(target_columns, categorical_candidates)
  if (length(categorical_cols) == 0) return(df)
  if (identical(method, "label")) {
    for (col in categorical_cols) {
      values <- as.character(df[[col]]); values[is.na(values)] <- "Missing"
      df[[col]] <- as.integer(factor(values, levels = unique(values)))
    }
    return(df)
  }
  other_cols <- setdiff(names(df), categorical_cols)
  encoded_parts <- lapply(categorical_cols, function(col) {
    values <- as.character(df[[col]]); values[is.na(values)] <- "Missing"
    levels_found <- unique(values)
    out <- lapply(levels_found, function(lv) as.integer(values == lv))
    level_names <- vapply(levels_found, function(lv) clean_column_name(paste(col, lv, sep = "_")), character(1))
    out_df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    names(out_df) <- make.unique(level_names, sep = "_"); out_df
  })
  combined <- cbind(df[other_cols], do.call(cbind, encoded_parts))
  rownames(combined) <- NULL; combined
}

apply_preprocessing <- function(df, outlier_method, outlier_columns, scaling_method, scaling_columns, encoding_method, encoding_columns) {
  processed <- df
  processed <- apply_outlier_handling(processed, outlier_method, outlier_columns)
  processed <- scale_numeric_columns(processed, scaling_method, scaling_columns)
  processed <- encode_categorical_columns(processed, encoding_method, encoding_columns)
  processed
}

#Feature Engineering Helpers 
apply_feature_recipes <- function(df, recipes) {
  if (length(recipes) == 0 || nrow(df) == 0) return(df)
  for (recipe in recipes) {
    name <- recipe$name; operation <- recipe$operation
    col1 <- recipe$col1; col2 <- recipe$col2
    if (!(col1 %in% names(df))) next
    x1 <- suppressWarnings(as.numeric(df[[col1]]))
    if (operation %in% c("add","subtract","multiply","divide")) {
      if (!(col2 %in% names(df))) next
      x2 <- suppressWarnings(as.numeric(df[[col2]]))
      df[[name]] <- switch(operation,
        "add"      = x1 + x2,
        "subtract" = x1 - x2,
        "multiply" = x1 * x2,
        "divide"   = { r <- x1 / x2; r[x2 == 0] <- NA_real_; r }
      )
    } else if (identical(operation, "log")) {
      result <- rep(NA_real_, length(x1)); valid <- !is.na(x1) & x1 > -1
      result[valid] <- log1p(x1[valid]); df[[name]] <- result
    } else if (identical(operation, "square")) {
      df[[name]] <- x1^2
    } else if (identical(operation, "sqrt")) {
      result <- rep(NA_real_, length(x1)); valid <- !is.na(x1) & x1 >= 0
      result[valid] <- sqrt(x1[valid]); df[[name]] <- result
    } else if (identical(operation, "abs")) {
      df[[name]] <- abs(x1)
    }
  }
  df
}

# Display and Reporting Helpers 
dataset_quick_stats <- function(df) {
  if (is.null(df) || nrow(df) == 0)
    return(list(rows = 0, cols = 0, numeric = 0, non_numeric = 0, missing = 0, duplicates = 0))
  list(
    rows       = nrow(df),
    cols       = ncol(df),
    numeric    = sum(vapply(df, is.numeric, logical(1))),
    non_numeric= ncol(df) - sum(vapply(df, is.numeric, logical(1))),
    missing    = sum(is.na(df)),
    duplicates = sum(duplicated(df))
  )
}

build_missing_profile <- function(df) {
  if (ncol(df) == 0) return(data.frame())
  data.frame(
    Column      = names(df),
    Type        = vapply(df, function(x) class(x)[1], character(1)),
    Missing     = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    Missing_Pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
    Unique      = vapply(df, function(x) length(unique(x[!is.na(x)])), numeric(1)),
    stringsAsFactors = FALSE
  )
}

build_summary_table <- function(df) {
  if (ncol(df) == 0) return(data.frame())
  rows <- lapply(names(df), function(col) {
    x <- df[[col]]
    row <- list(Column = col, Type = class(x)[1], Missing = sum(is.na(x)),
      Unique = length(unique(x[!is.na(x)])), Mean = NA, SD = NA, Min = NA, Median = NA, Max = NA, Top = NA)
    if (is.numeric(x) && any(!is.na(x))) {
      row$Mean <- round(mean(x, na.rm = TRUE), 4); row$SD <- round(stats::sd(x, na.rm = TRUE), 4)
      row$Min <- round(min(x, na.rm = TRUE), 4); row$Median <- round(stats::median(x, na.rm = TRUE), 4)
      row$Max <- round(max(x, na.rm = TRUE), 4)
    } else {
      row$Top <- as.character(mode_value(as.character(x)))
    }
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

preview_datatable <- function(df, n = 12) {
  DT::datatable(utils::head(df, n), rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = n, dom = "tip"),
    class = "compact stripe")
}

empty_plotly <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(template = "plotly_white",
      annotations = list(list(text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 15, color = "#64748b"))),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

formula_from_col <- function(col_name) as.formula(paste0("~`", col_name, "`"))

first_or_default <- function(x, default = character(0)) if (length(x) == 0) default else x[[1]]

# UI Component Helpers 
metric_card <- function(title, value_output_id, subtitle = NULL, icon_char = NULL) {
  div(class = "metric-box",
    if (!is.null(icon_char)) div(class = "metric-icon", icon_char),
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(value_output_id, inline = TRUE)),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

workflow_step <- function(number, title, text) {
  div(class = "workflow-step",
    div(class = "step-badge", number),
    div(class = "step-copy", h4(title), p(text))
  )
}

stat_delta <- function(before_val, after_val, lower_is_better = FALSE) {
  delta <- after_val - before_val
  if (delta == 0) return(tags$span(class = "delta-neutral", "—"))
  improved <- if (lower_is_better) delta < 0 else delta > 0
  cls <- if (improved) "delta-good" else "delta-bad"
  arrow <- if (delta > 0) "▲" else "▼"
  tags$span(class = cls, paste(arrow, abs(delta)))
}

summary_comparison_ui <- function(before, after, method_tags = NULL) {

  make_delta <- function(b, a, lower_is_better = FALSE) {
    delta <- a - b
    if (delta == 0) return(tags$span(class = "delta-neutral", "—"))
    improved <- if (lower_is_better) delta < 0 else delta > 0
    cls   <- if (improved) "delta-good" else "delta-bad"
    arrow <- if (delta > 0) "▲" else "▼"
    tags$span(class = cls, paste(arrow, format(abs(delta), big.mark=",")))
  }

  miss_b_style <- if (before$missing > 0) "color:#d69e2e;font-weight:700;" else "color:#374151;"
  miss_a_style <- if (after$missing  > 0) "color:#d69e2e;font-weight:700;" else "color:#16a34a;font-weight:700;"

  rows_data <- list(
    list(label = "Rows",        b = format(before$rows, big.mark=","),  a = format(after$rows, big.mark=","),  delta = make_delta(before$rows,    after$rows,    lower_is_better=FALSE), b_style="", a_style=""),
    list(label = "Columns",     b = before$cols,   a = after$cols,    delta = make_delta(before$cols,    after$cols,    lower_is_better=FALSE), b_style="", a_style=""),
    list(label = "Numeric",     b = before$numeric,a = after$numeric, delta = make_delta(before$numeric, after$numeric, lower_is_better=FALSE), b_style="", a_style=""),
    list(label = "Non-numeric", b = before$non_numeric, a = after$non_numeric, delta = make_delta(before$non_numeric, after$non_numeric, lower_is_better=FALSE), b_style="", a_style=""),
    list(label = "Missing",     b = format(before$missing, big.mark=","), a = format(after$missing, big.mark=","), delta = make_delta(before$missing, after$missing, lower_is_better=TRUE), b_style=miss_b_style, a_style=miss_a_style)
  )

  tagList(
    tags$table(class = "summary-table",
      tags$thead(
        tags$tr(
          tags$th(class = "st-metric", "Metric"),
          tags$th(class = "st-before", "Before"),
          tags$th(class = "st-after",  "After"),
          tags$th(class = "st-change", "Change")
        )
      ),
      tags$tbody(
        lapply(rows_data, function(r) {
          tags$tr(
            tags$td(class = "st-metric", r$label),
            tags$td(class = "st-before", tags$span(style = r$b_style, r$b)),
            tags$td(class = "st-after",  tags$span(style = r$a_style, r$a)),
            tags$td(class = "st-change", r$delta)
          )
        })
      )
    ),
    if (!is.null(method_tags) && length(method_tags) > 0) {
      div(class = "method-tag-row",
        lapply(names(method_tags), function(k) {
          val <- method_tags[[k]]
          cls <- if (identical(val, "none") || identical(val, "keep") || identical(val, "no")) "method-tag-neutral" else "method-tag-active"
          tags$span(class = paste("method-tag", cls), paste0(k, ": ", val))
        })
      )
    }
  )
}

# CSS 
APP_CSS <- "
/*Global base: bump font 2px*/
body {
  background: #f0f4f8;
  font-family: 'Inter', -apple-system, sans-serif;
  font-size: 16px;
}
.navbar { box-shadow: 0 4px 14px rgba(15,23,42,0.1); font-size: 16px; }
.navbar-brand { font-weight: 800; letter-spacing: -0.02em; font-size: 18px; }
.navbar-nav .nav-link { font-size: 15px; }

/* Hero */
.hero-box {
  margin: 18px 0 22px;
  padding: 30px 32px;
  border-radius: 20px;
  background: linear-gradient(135deg, #0f2942 0%, #1f6f78 55%, #2ecc8b 100%);
  color: white;
  box-shadow: 0 16px 40px rgba(18,52,77,0.22);
}
.hero-box h2 { margin: 0 0 10px; font-weight: 800; letter-spacing: -0.03em; font-size: 28px; }
.hero-box p  { margin: 0; font-size: 16px; line-height: 1.65; opacity: .93; }

/* Section cards */
.section-card {
  background: white;
  border-radius: 16px;
  padding: 20px 22px;
  margin-bottom: 18px;
  box-shadow: 0 4px 16px rgba(15,23,42,0.06);
  border: 1px solid #e8edf2;
}
.section-card h3 { margin-top: 0; color: #0f2942; font-size: 22px; }
.section-card h4 { margin-top: 0; color: #0f2942; font-size: 19px; }
.panel-note { color: #64748b; font-size: 15px; line-height: 1.55; margin-bottom: 12px; }

/* Control groups */
.control-group {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 14px 16px;
  margin-bottom: 12px;
}
.control-group h5 { margin: 0 0 6px; color: #0f2942; font-weight: 700; font-size: 16px; }
.control-group label { font-size: 15px; }
.control-group .help-block, .control-group .shiny-input-container small { font-size: 14px; }

/* Status banner */
.status-banner {
  background: linear-gradient(90deg,rgba(31,111,120,.12),rgba(31,111,120,.04));
  border: 1px solid rgba(31,111,120,.18);
  border-radius: 12px;
  padding: 12px 16px;
  color: #0f2942;
  margin-bottom: 14px;
  font-size: 15px;
}

/* Metric cards */
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
.metric-box:hover { box-shadow: 0 8px 28px rgba(15,23,42,0.13); }
.metric-title {
  color: #64748b; font-size: 12px; text-transform: uppercase;
  letter-spacing: .07em; margin-bottom: 8px; font-weight: 700;
}
.metric-value { color: #0f172a; font-size: 2rem; font-weight: 800; line-height: 1.15; }
.metric-subtitle { color: #94a3b8; font-size: 14px; margin-top: 6px; }
.metric-icon { font-size: 1.4rem; margin-bottom: 4px; opacity: .7; }

/* Workflow steps */
.workflow-step {
  display: flex; gap: 14px; align-items: flex-start;
  padding: 14px 0; border-bottom: 1px solid #f1f5f9;
}
.workflow-step:last-child { border-bottom: none; padding-bottom: 0; }
.step-badge {
  width: 36px; height: 36px; border-radius: 50%;
  background: linear-gradient(135deg, #1f6f78, #2ecc8b);
  color: white; font-weight: 800; display: flex;
  align-items: center; justify-content: center; flex-shrink: 0;
  font-size: 16px;
}
.step-copy h4 { margin: 0 0 4px; font-size: 17px; color: #1e293b; }
.step-copy p  { margin: 0; color: #64748b; font-size: 15px; }

/*Summary 4-column table*/
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
.summary-table tbody tr { border-bottom: 1px solid #f1f5f9; }
.summary-table tbody tr:last-child { border-bottom: none; }
.summary-table tbody tr:hover { background: #fafbfc; }
.summary-table td { padding: 9px 16px; vertical-align: middle; }
.st-metric { color: #475569; font-weight: 600; font-size: 15px; width: 36%; }
.st-before { color: #374151; font-weight: 600; text-align: right; width: 20%; }
.st-after  { font-weight: 700; text-align: right; width: 20%; }
.st-change { text-align: right; width: 24%; padding-right: 20px; }

/* Deltas */
.delta-good    { color: #16a34a; font-size: 13px; font-weight: 700; }
.delta-bad     { color: #dc2626; font-size: 13px; font-weight: 700; }
.delta-neutral { color: #94a3b8; font-size: 13px; }

/* Method tags */
.method-tag-row { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 6px; margin-bottom: 4px; }
.method-tag {
  display: inline-block; padding: 5px 12px; border-radius: 20px;
  font-size: 13px; font-weight: 600;
}
.method-tag-neutral { background: #f1f5f9; color: #64748b; border: 1px solid #e2e8f0; }
.method-tag-active  { background: #dbeafe; color: #1e40af; border: 1px solid #bfdbfe; }

/* Stat insights panel */
.stat-insight-grid {
  display: flex; flex-wrap: wrap; gap: 10px; margin-top: 4px;
}
.stat-insight-card {
  flex: 1; min-width: 105px; background: #f8fafc; border-radius: 10px;
  padding: 11px 14px; border: 1px solid #e2e8f0; text-align: center;
}
.stat-insight-label { font-size: 11px; color: #94a3b8; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; }
.stat-insight-value { font-size: 17px; font-weight: 800; color: #1e293b; margin-top: 3px; }

/* Warning badge */
.missing-warning {
  background: #fffbeb; border: 1px solid #fcd34d; border-radius: 10px;
  padding: 11px 16px; color: #92400e; font-size: 15px; font-weight: 600;
  display: flex; align-items: center; gap: 8px; margin-bottom: 12px;
}

/* Source info card */
.source-info-card {
  display: flex; align-items: center; gap: 16px;
  padding: 14px 18px; background: #f0fdf4; border-radius: 12px;
  border: 1px solid #bbf7d0;
}
.source-info-icon { font-size: 2rem; }
.source-info-name { font-weight: 700; color: #14532d; font-size: 16px; }
.source-info-meta { color: #4ade80; font-size: 14px; }

/* Buttons */
.btn-primary { background: linear-gradient(135deg, #1f6f78, #1a5f67); border: none; font-weight: 600; border-radius: 8px; font-size: 15px; }
.btn-warning { color: #7c2d12; background: #ffedd5; border-color: #fdba74; font-weight: 600; border-radius: 8px; font-size: 15px; }
.btn-success { font-weight: 600; border-radius: 8px; font-size: 15px; }
.sidebar-panel .form-group { margin-bottom: 10px; }
.tab-pane { padding-top: 6px; }

/* EDA plot subtitle */
.chart-subtitle { color: #64748b; font-size: 15px; margin-bottom: 12px; }

/* Sidebar and form inputs */
.selectize-input, .form-control { font-size: 15px !important; }
.shiny-input-container label { font-size: 15px; font-weight: 600; color: #374151; }
.help-block { font-size: 14px; }
.checkbox label { font-size: 15px; }
"

# User Interface 
ui <- navbarPage(
  title = app_title,
  id = "main_tabs",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f6f78"),
  header = tagList(tags$head(tags$style(HTML(APP_CSS)))),

  # Tab 1: User Guide 
  tabPanel("User Guide",
    fluidRow(column(12,
      div(class = "hero-box",
        h2("\U0001F9EA Interactive Data Wrangling Studio"),
        p("Upload a dataset, clean and preprocess it, engineer new features, explore it interactively, and export the result — all in one guided workflow designed for data scientists and analysts.")
      )
    )),
    fluidRow(
      column(7, div(class = "section-card",
        h3("How to use the app"),
        workflow_step("1", "Load Data",
          "Upload a CSV, Excel, JSON, or RDS file — or start with one of the three built-in example datasets (iris, mtcars, ToothGrowth)."),
        workflow_step("2", "Clean",
          "Standardize column names and text, handle duplicate rows, and decide how to treat missing values (keep, drop, or impute with mean/median/mode)."),
        workflow_step("3", "Preprocess",
          "Apply outlier treatment (cap or remove via IQR), numeric scaling (standard / min-max / robust), and categorical encoding (one-hot or label)."),
        workflow_step("4", "Feature Engineering",
          "Create derived variables by combining or transforming existing columns with add, subtract, multiply, divide, log, sqrt, square, or abs operations."),
        workflow_step("5", "EDA & Visualize",
          "Generate interactive Plotly charts (histogram, box, scatter, bar, violin), inspect a summary statistics table, and view the correlation heatmap."),
        workflow_step("6", "Export",
          "Download the fully transformed dataset as a CSV file including all applied cleaning, preprocessing, and engineered features.")
      )),
      column(5,
        div(class = "section-card",
          h3("App capabilities"),
          tags$ul(
            tags$li("Supports CSV, Excel (.xlsx/.xls), JSON, and RDS uploads"),
            tags$li("Three built-in datasets for immediate testing"),
            tags$li("Real-time before/after comparison cards for cleaning and preprocessing"),
            tags$li("Missing value profile table with per-column counts and percentages"),
            tags$li("Interactive feature engineering with instant distribution preview"),
            tags$li("5 chart types with color grouping and dynamic filtering"),
            tags$li("Statistical insights panel showing key metrics for the selected variable"),
            tags$li("Correlation heatmap with annotated values"),
            tags$li("Export the final processed dataset as CSV")
          )
        ),
        div(class = "section-card",
          h3("Built-in datasets"),
          tags$ul(
            tags$li(tags$b("test1 (iris)"), " — 150 rows, 5 columns, flower measurements, categorical target"),
            tags$li(tags$b("test2 (mtcars)"), " — 32 rows, 11 columns, car performance metrics"),
            tags$li(tags$b("test3 (ToothGrowth)"), " — 60 rows, 3 columns, vitamin C dose experiment")
          )
        )
      )
    )
  ),

  # Tab 2: Load Data 
  tabPanel("Load Data",
    fluidRow(
      column(3, metric_card("Rows",              "metric_rows",     icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",           "metric_cols",     icon_char = "\U0001F5C2")),
      column(3, metric_card("Missing Values",    "metric_missing",  icon_char = "\u26A0\uFE0F")),
      column(3, metric_card("Engineered Features","metric_features", icon_char = "\u2728"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "status-banner", strong("Start here."), br(),
          "Choose a built-in dataset or upload your own file to begin."),
        div(class = "control-group",
          h5("\U0001F4C2 Upload a dataset"),
          p(class = "panel-note", "Supported: CSV, XLSX, XLS, JSON, RDS. Max 300 MB."),
          fileInput("upload_file", "Choose file", accept = c(".csv",".xlsx",".xls",".json",".rds"))
        ),
        div(class = "control-group",
          h5("\U0001F4E6 Use a built-in dataset"),
          p(class = "panel-note", "Useful for quick testing without preparing a file."),
          selectInput("builtin_dataset", "Dataset",
            choices = c("test1 (iris)" = "test1", "test2 (mtcars)" = "test2", "test3 (ToothGrowth)" = "test3"),
            selected = "test1"),
          actionButton("load_builtin", "Load Dataset", class = "btn-primary btn-sm")
        )
      ),
      mainPanel(
        div(class = "section-card",
          h4("Current dataset"),
          p(class = "panel-note", "Source information and quick overview of the loaded data."),
          uiOutput("source_summary_ui")
        ),
        div(class = "section-card",
          h4("Raw data preview"),
          p(class = "panel-note", "First rows of the raw dataset before any transformations."),
          DTOutput("raw_preview")
        )
      )
    )
  ),

  # Tab 3: Cleaning 
  tabPanel("Cleaning",
    fluidRow(
      column(3, metric_card("Rows",          "metric_rows_clean",       icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",       "metric_cols_clean",       icon_char = "\U0001F5C2")),
      column(3, metric_card("Missing Values","metric_missing_clean",    icon_char = "\u26A0\uFE0F")),
      column(3, metric_card("Duplicates",    "metric_duplicates_clean", icon_char = "\U0001F4CB"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "status-banner", strong("Cleaning controls."), br(),
          "Standardize messy text, handle duplicates, and treat missing values."),
        div(class = "control-group",
          h5("Text standardization"),
          p(class = "panel-note", "Clean column names and normalize Yes/No and missing tokens."),
          checkboxInput("standardize_text", "Standardize text and column names", FALSE)
        ),
        div(class = "control-group",
          h5("Duplicate rows"),
          p(class = "panel-note", "Remove, flag, or leave duplicate rows unchanged."),
          selectInput("duplicate_action", "Duplicate handling",
            choices = c("keep","remove","flag"), selected = "keep")
        ),
        div(class = "control-group",
          h5("Missing values"),
          p(class = "panel-note", "Impute, drop, or keep rows with missing values."),
          selectInput("missing_strategy", "Strategy",
            choices = c(
              "Keep as-is"       = "keep",
              "Drop rows"        = "drop_rows",
              "Impute (smart)"   = "smart_impute"
            ), selected = "keep"),
          conditionalPanel("input.missing_strategy === 'smart_impute'",
            selectInput("numeric_impute", "Numeric fill",
              choices = c("median","mean","zero"), selected = "median"),
            selectInput("categorical_impute", "Categorical fill",
              choices = c("mode","missing_label"), selected = "mode")
          )
        )
      ),
      mainPanel(
        div(class = "section-card",
          h4("Cleaning summary"),
          p(class = "panel-note", "Before vs. after comparison for the selected cleaning steps."),
          uiOutput("cleaning_summary_ui")
        ),
        div(class = "section-card",
          h4("Missing value profile"),
          p(class = "panel-note", "Per-column missingness and cardinality after cleaning."),
          plotlyOutput("missing_bar_plot", height = "260px"),
          tags$br(),
          DTOutput("missing_profile_table")
        ),
        div(class = "section-card",
          h4("Cleaned data preview"),
          p(class = "panel-note", "First rows of the cleaned dataset."),
          DTOutput("cleaned_preview")
        )
      )
    )
  ),

  # Tab 4: Preprocessing 
  tabPanel("Preprocessing",
    fluidRow(
      column(3, metric_card("Rows",           "metric_rows_pre",    icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",        "metric_cols_pre",    icon_char = "\U0001F5C2")),
      column(3, metric_card("Numeric Cols",   "metric_numeric_pre", icon_char = "\U0001F522")),
      column(3, metric_card("Missing Values", "metric_missing_pre", icon_char = "\u26A0\uFE0F"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "status-banner", strong("Preprocessing controls."), br(),
          "Apply transformations that prepare variables for analysis or modeling."),
        div(class = "control-group",
          h5("\U0001F4A5 Outlier handling"),
          p(class = "panel-note", "IQR-based: cap extreme values or remove those rows."),
          selectInput("outlier_method", "Method",
            choices = c("none","cap","remove"), selected = "none"),
          selectizeInput("outlier_cols", "Apply to columns", choices = NULL, multiple = TRUE)
        ),
        div(class = "control-group",
          h5("\u2696\uFE0F Scaling"),
          p(class = "panel-note", "Normalize numeric columns for fair comparison or modeling."),
          selectInput("scaling_method", "Method",
            choices = c("none","standard","minmax","robust"), selected = "none"),
          selectizeInput("scale_cols", "Apply to columns", choices = NULL, multiple = TRUE)
        ),
        div(class = "control-group",
          h5("\U0001F3F7\uFE0F Encoding"),
          p(class = "panel-note", "Convert categorical columns to numeric representation."),
          selectInput("encoding_method", "Method",
            choices = c("none","onehot","label"), selected = "none"),
          selectizeInput("encoding_cols", "Apply to columns", choices = NULL, multiple = TRUE)
        )
      ),
      mainPanel(
        div(class = "section-card",
          h4("Preprocessing summary"),
          p(class = "panel-note", "Real-time before vs. after comparison for the selected preprocessing steps."),
          uiOutput("preprocessing_summary_ui")
        ),
        div(class = "section-card",
          h4("Preprocessed data preview"),
          p(class = "panel-note", "First rows after preprocessing."),
          DTOutput("processed_preview")
        )
      )
    )
  ),

  # Tab 5: Feature Engineering 
  tabPanel("Feature Engineering",
    fluidRow(
      column(3, metric_card("Rows",           "metric_rows_feat",    icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",        "metric_cols_feat",    icon_char = "\U0001F5C2")),
      column(3, metric_card("Feature Rules",  "metric_feature_rules",icon_char = "\u2728")),
      column(3, metric_card("Missing Values", "metric_missing_feat", icon_char = "\u26A0\uFE0F"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "status-banner", strong("Create derived features."), br(),
          "Combine or transform numeric columns to build new analytical variables."),
        div(class = "control-group",
          h5("Feature setup"),
          p(class = "panel-note", "Enter a name or leave blank for an auto-generated one."),
          textInput("feature_name", "New feature name", placeholder = "e.g. petal_area"),
          selectInput("feature_operation", "Operation",
            choices = c("add","subtract","multiply","divide","log","sqrt","square","abs"),
            selected = "multiply"),
          selectInput("feature_col1", "Primary column", choices = NULL),
          selectInput("feature_col2", "Second column",  choices = NULL),
          helpText("log, sqrt, square, and abs use the primary column only.")
        ),
        div(class = "control-group",
          h5("Actions"),
          actionButton("add_feature",    "Add Feature",            class = "btn-primary btn-sm"),
          tags$span(style = "display:inline-block;width:6px;"),
          actionButton("reset_features", "Reset All Features",     class = "btn-warning  btn-sm")
        ),
        div(class = "control-group",
          h5("Inspect distribution"),
          selectInput("feature_focus", "Feature to inspect", choices = NULL)
        )
      ),
      mainPanel(
        div(class = "section-card",
          h4("Feature engineering status"),
          uiOutput("feature_summary_ui")
        ),
        div(class = "section-card",
          h4("Feature rules"),
          p(class = "panel-note", "All saved feature transformation recipes."),
          DTOutput("feature_recipe_table")
        ),
        div(class = "section-card",
          h4("Feature distribution"),
          p(class = "panel-note", "Distribution of the selected numeric feature."),
          plotlyOutput("feature_plot", height = "340px")
        ),
        div(class = "section-card",
          h4("Dataset preview"),
          p(class = "panel-note", "First rows after feature engineering."),
          DTOutput("featured_preview")
        )
      )
    )
  ),

  # Tab 6: EDA / Visualization 
  tabPanel("EDA / Visualization",
    fluidRow(
      column(3, metric_card("Rows (Filtered)",  "metric_rows_eda",    icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",          "metric_cols_eda",    icon_char = "\U0001F5C2")),
      column(3, metric_card("Numeric Cols",     "metric_numeric_eda", icon_char = "\U0001F522")),
      column(3, metric_card("Missing Values",   "metric_missing_eda", icon_char = "\u26A0\uFE0F"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4,
        div(class = "status-banner", strong("Explore the data interactively."), br(),
          "Configure chart type, variables, grouping, and filters."),
        div(class = "control-group",
          h5("\U0001F4C8 Plot setup"),
          selectInput("plot_type", "Chart type",
            choices = c("Histogram","Box","Violin","Scatter","Bar"), selected = "Histogram"),
          selectInput("x_var", "X variable", choices = NULL),
          selectInput("y_var", "Y variable", choices = "None", selected = "None"),
          conditionalPanel("input.plot_type === 'Histogram'",
            sliderInput("hist_bins", "Number of bins", min = 5, max = 100, value = 30, step = 5)
          ),
          conditionalPanel("input.plot_type === 'Scatter'",
            checkboxInput("scatter_trend", "Show trend line", value = FALSE)
          )
        ),
        div(class = "control-group",
          h5("\U0001F3A8 Grouping"),
          p(class = "panel-note", "Color-code the plot by a categorical or discrete variable."),
          selectInput("color_var", "Color by", choices = "None", selected = "None")
        ),
        div(class = "control-group",
          h5("\U0001F50D Filter"),
          p(class = "panel-note", "Narrow the dataset before plotting."),
          selectInput("filter_col", "Filter column", choices = "None", selected = "None"),
          uiOutput("filter_ui")
        )
      ),
      mainPanel(
        div(class = "section-card",
          h4("Interactive chart"),
          p(class = "panel-note chart-subtitle", "Hover for details, drag to zoom, double-click to reset."),
          plotlyOutput("eda_plot", height = "440px")
        ),
        div(class = "section-card",
          h4("Statistical insights"),
          p(class = "panel-note", "Key statistics for the selected X variable in the current filtered dataset."),
          uiOutput("stat_insight_ui")
        ),
        div(class = "section-card",
          h4("Summary statistics table"),
          p(class = "panel-note", "Full summary for all variables in the filtered dataset."),
          DTOutput("summary_stats_table")
        ),
        div(class = "section-card",
          h4("Correlation heatmap"),
          p(class = "panel-note", "Pairwise Pearson correlations for numeric columns. Requires at least 2 numeric columns."),
          plotlyOutput("correlation_heatmap", height = "480px")
        )
      )
    )
  ),

  # Tab 7: Export 
  tabPanel("Export / Download",
    fluidRow(
      column(3, metric_card("Rows",               "metric_rows_export",     icon_char = "\U0001F4CA")),
      column(3, metric_card("Columns",            "metric_cols_export",     icon_char = "\U0001F5C2")),
      column(3, metric_card("Engineered Features","metric_features_export", icon_char = "\u2728")),
      column(3, metric_card("Missing Values",     "metric_missing_export",  icon_char = "\u26A0\uFE0F"))
    ),
    fluidRow(column(12,
      div(class = "section-card",
        h4("Export the transformed dataset"),
        p(class = "panel-note",
          "The download includes all cleaning, preprocessing, and feature engineering steps applied in this session."),
        uiOutput("export_summary_ui"),
        tags$br(),
        downloadButton("download_processed_data", "\u2B07\uFE0F  Download as CSV", class = "btn-success")
      )
    )),
    fluidRow(column(12,
      div(class = "section-card",
        h4("Export preview"),
        p(class = "panel-note", "Preview of the dataset that will be downloaded."),
        DTOutput("export_preview")
      )
    ))
  )
)

# Server 
server <- function(input, output, session) {

  raw_data        <- reactiveVal(load_builtin_dataset("test1"))
  source_name     <- reactiveVal("Built-in dataset: test1 (iris)")
  status_message  <- reactiveVal("Loaded the built-in test1 (iris) dataset.")
  feature_recipes <- reactiveVal(list())
  feature_message <- reactiveVal("No engineered features yet.")

  # Data load events 
  observeEvent(input$load_builtin, {
    df <- load_builtin_dataset(input$builtin_dataset)
    raw_data(df)
    display_name <- switch(input$builtin_dataset,
      "test1" = "test1 (iris)", "test2" = "test2 (mtcars)", "test3" = "test3 (ToothGrowth)",
      input$builtin_dataset)
    source_name(paste("Built-in:", display_name))
    status_message(paste("Loaded", display_name, "successfully."))
    feature_recipes(list()); feature_message("Feature recipes reset for new dataset.")
  })

  observeEvent(input$upload_file, {
    req(input$upload_file); info <- input$upload_file
    tryCatch({
      df <- read_uploaded_data(info$datapath, info$name)
      raw_data(df); source_name(paste("Uploaded:", info$name))
      status_message(paste("File", info$name, "loaded successfully."))
      feature_recipes(list()); feature_message("Feature recipes reset for new dataset.")
    }, error = function(e) {
      status_message(paste("Error:", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  })

  # Reactive data pipeline 
  cleaned_data <- reactive({
    df <- raw_data(); req(df)
    apply_cleaning(df, standardize_text = input$standardize_text,
      duplicate_action = input$duplicate_action, missing_strategy = input$missing_strategy,
      numeric_strategy = input$numeric_impute %||% "median",
      categorical_strategy = input$categorical_impute %||% "mode")
  })

  preprocessed_data <- reactive({
    df <- cleaned_data(); req(df)
    apply_preprocessing(df,
      outlier_method   = input$outlier_method,   outlier_columns   = input$outlier_cols,
      scaling_method   = input$scaling_method,   scaling_columns   = input$scale_cols,
      encoding_method  = input$encoding_method,  encoding_columns  = input$encoding_cols)
  })

  featured_data <- reactive({
    df <- preprocessed_data(); req(df)
    apply_feature_recipes(df, feature_recipes())
  })

  filtered_data <- reactive({
    df <- featured_data(); req(df)
    filter_col <- input$filter_col
    if (is.null(filter_col) || identical(filter_col, "None") || !(filter_col %in% names(df))) return(df)
    if (is.numeric(df[[filter_col]])) {
      rng <- input$filter_range
      if (is.null(rng) || length(rng) != 2) return(df)
      return(df[!is.na(df[[filter_col]]) & df[[filter_col]] >= rng[1] & df[[filter_col]] <= rng[2], , drop = FALSE])
    }
    levels_sel <- input$filter_levels
    if (is.null(levels_sel) || length(levels_sel) == 0) return(df[0, , drop = FALSE])
    df[as.character(df[[filter_col]]) %in% levels_sel, , drop = FALSE]
  })

  # Dynamic select updates 
  observe({
    df <- cleaned_data()
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    updateSelectizeInput(session, "outlier_cols",  choices = num_cols, selected = intersect(isolate(input$outlier_cols),  num_cols), server = TRUE)
    updateSelectizeInput(session, "scale_cols",    choices = num_cols, selected = intersect(isolate(input$scale_cols),    num_cols), server = TRUE)
    updateSelectizeInput(session, "encoding_cols", choices = cat_cols, selected = intersect(isolate(input$encoding_cols), cat_cols), server = TRUE)
  })

  observe({
    df <- featured_data()
    cols <- names(df); num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    cur_c1 <- isolate(input$feature_col1); cur_c2 <- isolate(input$feature_col2)
    cur_ff <- isolate(input$feature_focus)
    cur_x  <- isolate(input$x_var);   cur_y  <- isolate(input$y_var)
    cur_col <- isolate(input$color_var); cur_f <- isolate(input$filter_col)

    updateSelectInput(session, "feature_col1", choices = num_cols,
      selected = if (cur_c1 %in% num_cols) cur_c1 else first_or_default(num_cols))
    updateSelectInput(session, "feature_col2", choices = num_cols,
      selected = if (cur_c2 %in% num_cols) cur_c2 else first_or_default(num_cols))
    updateSelectInput(session, "feature_focus", choices = num_cols,
      selected = if (cur_ff %in% num_cols) cur_ff else first_or_default(num_cols))
    updateSelectInput(session, "x_var", choices = cols,
      selected = if (cur_x %in% cols) cur_x else first_or_default(cols))
    updateSelectInput(session, "y_var", choices = c("None", num_cols),
      selected = if (cur_y %in% c("None", num_cols)) cur_y else "None")
    updateSelectInput(session, "color_var", choices = c("None", cols),
      selected = if (cur_col %in% c("None", cols)) cur_col else "None")
    updateSelectInput(session, "filter_col", choices = c("None", cols),
      selected = if (cur_f %in% c("None", cols)) cur_f else "None")
  })

  #Feature engineering events 
  observeEvent(input$add_feature, {
    df <- preprocessed_data()
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (!(input$feature_col1 %in% num_cols)) {
      feature_message("Please choose a valid numeric primary column."); return()
    }
    if (input$feature_operation %in% c("add","subtract","multiply","divide") &&
        !(input$feature_col2 %in% num_cols)) {
      feature_message("Please choose a valid second numeric column."); return()
    }
    default_name <- if (input$feature_operation %in% c("add","subtract","multiply","divide")) {
      paste(input$feature_col1, input$feature_operation, input$feature_col2, sep = "_")
    } else {
      paste(input$feature_operation, input$feature_col1, sep = "_")
    }
    raw_name <- trimws(input$feature_name %||% "")
    requested_name <- if (nzchar(raw_name)) clean_column_name(raw_name) else clean_column_name(default_name)
    existing <- c(names(df), vapply(feature_recipes(), function(x) x$name, character(1)))
    final_name <- make_unique_name(requested_name, existing)
    recipes <- feature_recipes()
    recipes[[length(recipes) + 1]] <- list(
      name = final_name, operation = input$feature_operation,
      col1 = input$feature_col1, col2 = input$feature_col2)
    feature_recipes(recipes)
    feature_message(paste("Added feature", shQuote(final_name), "via", input$feature_operation, "operation."))
  })

  observeEvent(input$reset_features, {
    feature_recipes(list())
    feature_message("All engineered features have been removed.")
  })

  #Filter UI 
  output$filter_ui <- renderUI({
    df <- featured_data(); filter_col <- input$filter_col
    if (is.null(filter_col) || identical(filter_col, "None") || !(filter_col %in% names(df)))
      return(helpText("No filter selected."))
    if (is.numeric(df[[filter_col]])) {
      values <- df[[filter_col]][!is.na(df[[filter_col]])]
      if (length(values) == 0) return(helpText("Column has no non-missing values."))
      current <- isolate(input$filter_range)
      dmin <- min(values); dmax <- max(values)
      if (is.null(current) || length(current) != 2) current <- c(dmin, dmax)
      sliderInput("filter_range", paste(filter_col, "range"),
        min = floor(dmin*100)/100, max = ceiling(dmax*100)/100,
        value = c(max(dmin, current[1]), min(dmax, current[2])))
    } else {
      lvls <- sort(unique(as.character(df[[filter_col]][!is.na(df[[filter_col]])])))
      current <- intersect(isolate(input$filter_levels) %||% lvls, lvls)
      selectizeInput("filter_levels", paste(filter_col, "values"),
        choices = lvls, selected = current, multiple = TRUE)
    }
  })

  #Metric outputs 
  stats_raw   <- reactive(dataset_quick_stats(raw_data()))
  stats_clean <- reactive(dataset_quick_stats(cleaned_data()))
  stats_pre   <- reactive(dataset_quick_stats(preprocessed_data()))
  stats_feat  <- reactive(dataset_quick_stats(featured_data()))
  stats_eda   <- reactive(dataset_quick_stats(filtered_data()))

  output$metric_rows     <- renderText(format(stats_raw()$rows,    big.mark=","))
  output$metric_cols     <- renderText(format(stats_raw()$cols,    big.mark=","))
  output$metric_missing  <- renderText(format(stats_raw()$missing, big.mark=","))
  output$metric_features <- renderText(length(feature_recipes()))

  output$metric_rows_clean       <- renderText(format(stats_clean()$rows,       big.mark=","))
  output$metric_cols_clean       <- renderText(format(stats_clean()$cols,       big.mark=","))
  output$metric_missing_clean    <- renderText(format(stats_clean()$missing,    big.mark=","))
  output$metric_duplicates_clean <- renderText(format(stats_clean()$duplicates, big.mark=","))

  output$metric_rows_pre    <- renderText(format(stats_pre()$rows,    big.mark=","))
  output$metric_cols_pre    <- renderText(format(stats_pre()$cols,    big.mark=","))
  output$metric_numeric_pre <- renderText(format(stats_pre()$numeric, big.mark=","))
  output$metric_missing_pre <- renderText(format(stats_pre()$missing, big.mark=","))

  output$metric_rows_feat     <- renderText(format(stats_feat()$rows,    big.mark=","))
  output$metric_cols_feat     <- renderText(format(stats_feat()$cols,    big.mark=","))
  output$metric_feature_rules <- renderText(length(feature_recipes()))
  output$metric_missing_feat  <- renderText(format(stats_feat()$missing, big.mark=","))

  output$metric_rows_eda     <- renderText(format(stats_eda()$rows,    big.mark=","))
  output$metric_cols_eda     <- renderText(format(stats_eda()$cols,    big.mark=","))
  output$metric_numeric_eda  <- renderText(format(stats_eda()$numeric, big.mark=","))
  output$metric_missing_eda  <- renderText(format(stats_eda()$missing, big.mark=","))

  output$metric_rows_export     <- renderText(format(stats_feat()$rows,    big.mark=","))
  output$metric_cols_export     <- renderText(format(stats_feat()$cols,    big.mark=","))
  output$metric_features_export <- renderText(length(feature_recipes()))
  output$metric_missing_export  <- renderText(format(stats_feat()$missing, big.mark=","))

  #Load Data outputs 
  output$source_summary_ui <- renderUI({
    df <- raw_data()
    s  <- dataset_quick_stats(df)
    div(class = "source-info-card",
      div(class = "source-info-icon", "\U0001F4BE"),
      div(
        div(class = "source-info-name", source_name()),
        div(class = "source-info-meta", status_message()),
        tags$br(),
        div(class = "method-tag-row",
          tags$span(class = "method-tag method-tag-active", paste("Rows:", format(s$rows, big.mark=","))),
          tags$span(class = "method-tag method-tag-active", paste("Cols:", s$cols)),
          tags$span(class = "method-tag method-tag-neutral", paste("Numeric:", s$numeric)),
          tags$span(class = "method-tag method-tag-neutral", paste("Non-numeric:", s$non_numeric)),
          if (s$missing > 0)
            tags$span(class = "method-tag", style = "background:#fef3c7;color:#92400e;border:1px solid #fcd34d;",
              paste("\u26A0\uFE0F Missing:", s$missing))
          else
            tags$span(class = "method-tag method-tag-neutral", "\u2705 No missing values")
        )
      )
    )
  })

  output$raw_preview <- renderDT({ preview_datatable(raw_data()) })

  #Cleaning outputs 
  output$cleaning_summary_ui <- renderUI({
    before <- dataset_quick_stats(raw_data())
    after  <- dataset_quick_stats(cleaned_data())
    summary_comparison_ui(before, after, method_tags = list(
      "Text std."    = if (isTRUE(input$standardize_text)) "yes" else "no",
      "Duplicates"   = input$duplicate_action,
      "Missing"      = input$missing_strategy
    ))
  })

  output$missing_bar_plot <- renderPlotly({
    df <- cleaned_data()
    prof <- build_missing_profile(df)
    prof_miss <- prof[prof$Missing > 0, ]
    if (nrow(prof_miss) == 0) {
      return(empty_plotly("\u2705  No missing values detected in the cleaned dataset."))
    }
    prof_miss <- prof_miss[order(prof_miss$Missing_Pct, decreasing = TRUE), ]
    plot_ly(data = prof_miss,
      x = ~reorder(Column, Missing_Pct),
      y = ~Missing_Pct,
      type = "bar",
      marker = list(color = "#f59e0b", line = list(color = "#d97706", width = 1)),
      text  = ~paste0(Missing, " rows (", Missing_Pct, "%)"),
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

  output$missing_profile_table <- renderDT({ preview_datatable(build_missing_profile(cleaned_data()), n = 20) })
  output$cleaned_preview       <- renderDT({ preview_datatable(cleaned_data()) })

  #Preprocessing outputs 
  output$preprocessing_summary_ui <- renderUI({
    before <- dataset_quick_stats(cleaned_data())
    after  <- dataset_quick_stats(preprocessed_data())
    warn <- if (after$missing > 0)
      div(class = "missing-warning",
        "\u26A0\uFE0F", paste(after$missing, "missing value(s) remain after preprocessing.",
          "Consider using the Cleaning tab to impute or drop them first."))
    else NULL
    tagList(
      warn,
      summary_comparison_ui(before, after, method_tags = list(
        "Outlier"  = input$outlier_method,
        "Scaling"  = input$scaling_method,
        "Encoding" = input$encoding_method
      ))
    )
  })

  output$processed_preview <- renderDT({ preview_datatable(preprocessed_data()) })

  #Feature Engineering outputs 
  output$feature_summary_ui <- renderUI({
    recipes <- feature_recipes()
    msg <- feature_message()
    df  <- featured_data()
    s   <- dataset_quick_stats(df)
    tagList(
      div(class = if (length(recipes) > 0) "source-info-card" else "status-banner",
        div(if (length(recipes) > 0) "\u2728" else "\U0001F4CB", style="font-size:1.5rem;"),
        div(
          div(style = "font-weight:700;color:#1e293b;", msg),
          div(style = "color:#64748b;font-size:.85rem;margin-top:4px;",
            paste0("Current dataset: ", format(s$rows, big.mark=","), " rows × ", s$cols, " columns | ",
                   length(recipes), " feature rule(s) active"))
        )
      )
    )
  })

  output$feature_recipe_table <- renderDT({
    recipes <- feature_recipes()
    if (length(recipes) == 0)
      return(DT::datatable(data.frame(message = "No feature recipes yet."),
        rownames = FALSE, options = list(dom = "t")))
    recipe_df <- do.call(rbind, lapply(recipes, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    preview_datatable(recipe_df, n = 20)
  })

  output$featured_preview <- renderDT({ preview_datatable(featured_data()) })

  output$feature_plot <- renderPlotly({
    df <- featured_data(); focus <- input$feature_focus
    if (nrow(df) == 0 || is.null(focus) || !(focus %in% names(df)) || !is.numeric(df[[focus]]))
      return(empty_plotly("Create or select a numeric feature to inspect."))
    x_vals <- df[[focus]][!is.na(df[[focus]])]
    plot_ly(x = x_vals, type = "histogram", nbinsx = 30,
      marker = list(color = "#1f6f78", line = list(color = "#145d65", width = 0.8))) |>
      layout(template = "plotly_white", bargap = 0.06,
        title = list(text = paste("Distribution of", focus), font = list(size = 14)),
        xaxis = list(title = focus), yaxis = list(title = "Count"))
  })

  # EDA outputs
  PALETTE <- c("#1f6f78","#f59e0b","#e74c3c","#9b59b6","#2ecc71","#3498db","#e67e22","#1abc9c")

  output$eda_plot <- renderPlotly({
    df         <- filtered_data()
    x_var      <- input$x_var
    y_var      <- input$y_var
    color_var  <- input$color_var
    plot_type  <- input$plot_type
    hist_bins  <- input$hist_bins %||% 30
    trend_line <- isTRUE(input$scatter_trend)

    if (nrow(df) == 0) return(empty_plotly("No rows match the current filter selection."))
    if (is.null(x_var) || !(x_var %in% names(df))) return(empty_plotly("Choose a valid X variable."))

    has_color <- !is.null(color_var) && !identical(color_var, "None") && color_var %in% names(df)
    has_y     <- !is.null(y_var) && !identical(y_var, "None") && y_var %in% names(df)
    col_f     <- if (has_color) formula_from_col(color_var) else NULL

    base_layout <- list(
      template = "plotly_white",
      font     = list(family = "Inter, sans-serif", size = 13),
      margin   = list(l = 60, r = 20, t = 50, b = 60),
      xaxis    = list(title = x_var, gridcolor = "#f1f5f9"),
      yaxis    = list(gridcolor = "#f1f5f9"),
      legend   = list(bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#e2e8f0", borderwidth = 1)
    )

    p <- if (identical(plot_type, "Histogram")) {
      base_layout$yaxis$title <- "Count"
      base_layout$title <- list(text = paste("Distribution of", x_var), font = list(size = 15))
      plot_ly(data = df, x = formula_from_col(x_var), color = col_f,
        type = "histogram", nbinsx = hist_bins,
        marker = list(line = list(width = 0.7, color = "white")),
        colors = PALETTE) |>
        layout(bargap = 0.05, barmode = "overlay")

    } else if (identical(plot_type, "Box")) {
      target_y <- if (has_y && is.numeric(df[[y_var]])) y_var else x_var
      if (!is.numeric(df[[target_y]])) return(empty_plotly("Box plot requires a numeric variable."))
      base_layout$yaxis$title <- target_y
      base_layout$title <- list(text = paste("Box Plot:", target_y), font = list(size = 15))
      if (has_color) {
        plot_ly(data = df, x = formula_from_col(color_var), y = formula_from_col(target_y),
          color = col_f, type = "box", boxpoints = "outliers",
          jitter = 0.3, pointpos = -1.8, colors = PALETTE)
      } else {
        plot_ly(data = df, y = formula_from_col(target_y), type = "box",
          boxpoints = "outliers", marker = list(color = PALETTE[1]),
          fillcolor = paste0(PALETTE[1], "40"), line = list(color = PALETTE[1]))
      }

    } else if (identical(plot_type, "Violin")) {
      target_y <- if (has_y && is.numeric(df[[y_var]])) y_var else x_var
      if (!is.numeric(df[[target_y]])) return(empty_plotly("Violin plot requires a numeric variable."))
      base_layout$yaxis$title <- target_y
      base_layout$title <- list(text = paste("Violin Plot:", target_y), font = list(size = 15))
      if (has_color) {
        plot_ly(data = df, x = formula_from_col(color_var), y = formula_from_col(target_y),
          color = col_f, type = "violin", box = list(visible = TRUE),
          meanline = list(visible = TRUE), colors = PALETTE)
      } else {
        plot_ly(data = df, y = formula_from_col(target_y), type = "violin",
          box = list(visible = TRUE), meanline = list(visible = TRUE),
          fillcolor = paste0(PALETTE[1], "50"), line = list(color = PALETTE[1]))
      }

    } else if (identical(plot_type, "Scatter")) {
      if (!has_y) return(empty_plotly("Scatter plots require both X and Y variables."))
      base_layout$xaxis$title <- x_var; base_layout$yaxis$title <- y_var
      base_layout$title <- list(text = paste("Scatter:", x_var, "vs", y_var), font = list(size = 15))
      p <- plot_ly(data = df, x = formula_from_col(x_var), y = formula_from_col(y_var),
        color = col_f, type = "scatter", mode = "markers",
        marker = list(size = 7, opacity = 0.75, line = list(width = 0.5, color = "white")),
        colors = PALETTE)
      if (trend_line && is.numeric(df[[x_var]]) && is.numeric(df[[y_var]])) {
        x_vals <- df[[x_var]][!is.na(df[[x_var]]) & !is.na(df[[y_var]])]
        y_vals <- df[[y_var]][!is.na(df[[x_var]]) & !is.na(df[[y_var]])]
        if (length(x_vals) >= 2) {
          fit <- tryCatch(lm(y_vals ~ x_vals), error = function(e) NULL)
          if (!is.null(fit)) {
            x_seq <- seq(min(x_vals), max(x_vals), length.out = 80)
            y_seq <- predict(fit, newdata = data.frame(x_vals = x_seq))
            p <- add_trace(p, x = x_seq, y = y_seq, type = "scatter", mode = "lines",
              line = list(color = "#e74c3c", dash = "dash", width = 2),
              name = "Trend", showlegend = TRUE, inherit = FALSE)
          }
        }
      }
      p

    } else {
      # Bar chart
      base_layout$yaxis$title <- if (has_y) y_var else "Count"
      base_layout$title <- list(text = paste("Bar Chart:", x_var), font = list(size = 15))
      if (!has_y) {
        counts <- as.data.frame(table(df[[x_var]], useNA = "ifany"), stringsAsFactors = FALSE)
        names(counts) <- c(x_var, "count")
        plot_ly(data = counts, x = formula_from_col(x_var), y = ~count, type = "bar",
          marker = list(color = PALETTE[1], line = list(color = "#145d65", width = 0.8)))
      } else {
        plot_ly(data = df, x = formula_from_col(x_var), y = formula_from_col(y_var),
          color = col_f, type = "bar", colors = PALETTE)
      }
    }

    do.call(layout, c(list(p), base_layout))
  })

  # Statistical insights panel
  output$stat_insight_ui <- renderUI({
    df    <- filtered_data()
    x_var <- input$x_var
    if (is.null(x_var) || !(x_var %in% names(df))) return(helpText("Select a variable to see insights."))
    x <- df[[x_var]]
    if (is.numeric(x) && any(!is.na(x))) {
      vals <- x[!is.na(x)]
      div(class = "stat-insight-grid",
        div(class = "stat-insight-card", div(class = "stat-insight-label", "N"), div(class = "stat-insight-value", format(length(vals), big.mark=","))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Mean"), div(class = "stat-insight-value", round(mean(vals),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Median"), div(class = "stat-insight-value", round(stats::median(vals),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Std Dev"), div(class = "stat-insight-value", round(stats::sd(vals),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Min"), div(class = "stat-insight-value", round(min(vals),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Max"), div(class = "stat-insight-value", round(max(vals),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Q1"), div(class = "stat-insight-value", round(stats::quantile(vals,.25),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Q3"), div(class = "stat-insight-value", round(stats::quantile(vals,.75),3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Skewness"),
          div(class = "stat-insight-value",
            round({m3 <- mean((vals-mean(vals))^3); m2 <- mean((vals-mean(vals))^2); if(m2==0) 0 else m3/m2^1.5}, 3))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Missing"), div(class = "stat-insight-value", sum(is.na(x))))
      )
    } else {
      freqs <- sort(table(as.character(x[!is.na(x)])), decreasing = TRUE)
      top5  <- head(freqs, 5)
      div(class = "stat-insight-grid",
        div(class = "stat-insight-card", div(class = "stat-insight-label", "N"), div(class = "stat-insight-value", format(sum(!is.na(x)), big.mark=","))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Unique"), div(class = "stat-insight-value", length(freqs))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Missing"), div(class = "stat-insight-value", sum(is.na(x)))),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Mode"), div(class = "stat-insight-value", names(top5)[1])),
        div(class = "stat-insight-card", div(class = "stat-insight-label", "Mode Freq"), div(class = "stat-insight-value", top5[[1]]))
      )
    }
  })

  output$summary_stats_table <- renderDT({ preview_datatable(build_summary_table(filtered_data()), n = 25) })

  output$correlation_heatmap <- renderPlotly({
    df <- filtered_data()
    num_df <- df[vapply(df, is.numeric, logical(1))]
    if (ncol(num_df) < 2 || nrow(num_df) == 0)
      return(empty_plotly("Need at least 2 numeric columns for a correlation heatmap."))
    corr <- round(stats::cor(num_df, use = "pairwise.complete.obs"), 2)
    n_cols <- ncol(corr)
    ann_size <- max(7, min(12, 120 / n_cols))
    annotations <- do.call(c, lapply(seq_len(nrow(corr)), function(i) {
      lapply(seq_len(ncol(corr)), function(j) {
        list(x = colnames(corr)[j], y = rownames(corr)[i], text = sprintf("%.2f", corr[i, j]),
          xref = "x", yref = "y", showarrow = FALSE,
          font = list(size = ann_size, color = if (abs(corr[i,j]) > 0.5) "white" else "#1e293b"))
      })
    }))
    plot_ly(x = colnames(corr), y = rownames(corr), z = corr,
      type = "heatmap", colorscale = list(c(0,"#b2182b"), c(0.5,"#f7f7f7"), c(1,"#2166ac")),
      zmin = -1, zmax = 1, showscale = TRUE,
      hovertemplate = "<b>%{y} × %{x}</b><br>r = %{z}<extra></extra>") |>
      layout(template = "plotly_white",
        title = list(text = "Correlation Matrix", font = list(size = 14)),
        xaxis = list(tickangle = -35, side = "bottom"),
        annotations = annotations,
        margin = list(l = 100, b = 100))
  })

  # Export outputs 
  output$export_summary_ui <- renderUI({
    s <- dataset_quick_stats(featured_data())
    div(class = "stat-insight-grid",
      div(class = "stat-insight-card", div(class="stat-insight-label","Source"),   div(class="stat-insight-value", style="font-size:.85rem;", source_name())),
      div(class = "stat-insight-card", div(class="stat-insight-label","Rows"),    div(class="stat-insight-value", format(s$rows, big.mark=","))),
      div(class = "stat-insight-card", div(class="stat-insight-label","Columns"), div(class="stat-insight-value", s$cols)),
      div(class = "stat-insight-card", div(class="stat-insight-label","Features"),div(class="stat-insight-value", length(feature_recipes()))),
      div(class = "stat-insight-card", div(class="stat-insight-label","Missing"), div(class="stat-insight-value",
        style = if(s$missing>0) "color:#d69e2e;" else "color:#16a34a;",
        s$missing))
    )
  })

  output$export_preview <- renderDT({ preview_datatable(featured_data()) })

  output$download_processed_data <- downloadHandler(
    filename = function() paste0("processed_data_", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(featured_data(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
