# Project 2: Interactive Data Wrangling Studio

This repository contains an R Shiny application for interactive data upload, cleaning, preprocessing, feature engineering, exploratory data analysis, and export.

## Project Overview

Users can upload their own datasets or use built-in examples to explore a full data wrangling workflow — from raw data to a cleaned, transformed, and visualized result — entirely within a single Shiny interface.

## Main Features

### 1. Data Upload

Supported file types:

- `.csv`
- `.xlsx` / `.xls`
- `.json`
- `.rds`

Built-in datasets for immediate testing:

- `test1` — iris (150 rows, 5 columns, flower measurements with categorical target)
- `test2` — mtcars (32 rows, 11 columns, car performance metrics)
- `test3` — ToothGrowth (60 rows, 3 columns, vitamin C dose experiment)

### 2. Data Cleaning

- Column name standardization and text normalization
- Duplicate handling: `keep`, `flag`, or `remove`
- Missing value strategy: `keep`, `drop_rows`, or `smart_impute`
  - Numeric imputation: `median`, `mean`, or `zero`
  - Categorical imputation: `mode` or `"Missing"` label
- Real-time **before / after comparison table** (Metric | Before | After | Change)
- **Missing value bar chart** showing per-column missingness percentage
- Missing value profile table with counts, percentages, and unique value cardinality

### 3. Preprocessing

- Outlier handling: `none`, `cap`, or `remove` (IQR-based)
- Scaling: `none`, `standard`, `minmax`, or `robust`
- Categorical encoding: `none`, `onehot`, or `label`
- Real-time **before / after comparison table** with warning if missing values remain after preprocessing

### 4. Feature Engineering

Create new derived columns interactively using:

- `add`, `subtract`, `multiply`, `divide`
- `log` (log1p), `square`, `sqrt`, `abs`

Feature rules are saved and applied reactively. A distribution histogram of the selected feature is shown instantly after creation.

### 5. EDA and Visualization

Interactive Plotly visualizations with dynamic variable selection, color grouping, and dataset filtering:

- **Histogram** — adjustable bin count
- **Box plot** — with optional group-by variable
- **Violin plot** — with embedded box and mean line
- **Scatter plot** — with optional linear trend line
- **Bar chart** — count or aggregated Y variable

Additional outputs:

- **Statistical insights panel** — N, mean, median, SD, min, max, Q1, Q3, skewness, missing count (numeric); mode and frequency (categorical)
- **Summary statistics table** — full per-column statistics for the filtered dataset
- **Correlation heatmap** — annotated with r values, color-coded from −1 to +1

### 6. Export

Download the fully transformed dataset (all cleaning, preprocessing, and feature engineering applied) as a CSV file from the **Export / Download** tab.

## Required Packages

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "readxl", "jsonlite"))
```

## How to Run

In RStudio:

1. Open the project folder.
2. Open `app.R`.
3. Click **Run App**.

## Workflow

1. **Load Data** — Upload a file or select a built-in dataset.
2. **Cleaning** — Standardize text, handle duplicates, and treat missing values.
3. **Preprocessing** — Apply outlier treatment, scaling, and encoding.
4. **Feature Engineering** — Build derived variables and inspect their distributions.
5. **EDA / Visualization** — Explore the data with interactive charts and summary statistics.
6. **Export / Download** — Download the final processed dataset as CSV.

## Notes

- Upload limit: `300 MB`
- Very large datasets may take time to render in previews
- One-hot encoding high-cardinality columns (e.g. ID fields) can produce many columns and slow the app — apply to a small set of meaningful categorical columns only

## Troubleshooting

**App does not respond after upload**

- The file may be large; wait a moment for the browser to update
- Restart the app fully (do not just refresh the browser) and re-upload
- Start with all preprocessing options set to `none`

**Maximum upload size exceeded**

The app sets the limit via:

```r
options(shiny.maxRequestSize = 300 * 1024^2)
```

A full app restart (not a browser refresh) is required after any change to `app.R`.
