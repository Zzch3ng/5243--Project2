# Project 2: Interactive Data Wrangling Studio

This repository contains an R Shiny application for interactive data upload, cleaning, preprocessing, feature engineering, exploratory data analysis, and export.

## Project Overview

In the application users can:

- Upload their own data in multiple formats
- Test the app with built-in sample datasets
- Clean inconsistent values and missing data
- Apply preprocessing methods such as scaling, encoding, and outlier handling
- Create new features interactively
- Explore the transformed data with interactive plots and summary statistics
- Download the final processed dataset

There are also some tabular datasets inside a single Shiny interface which user can work with them。

## Main Features

### 1. Data Upload

Supported file types:

- `.csv`
- `.xlsx`
- `.xls`
- `.json`
- `.rds`

The app also includes built-in datasets for testing（data from the r dataset which include by itself):

- `test1（iris）`
- `test2（mtcars）`
- `test3（ToothGrowth）`

### 2. Data Cleaning

The Cleaning tab supports:

- Column name standardization
- Text normalization
- Duplicate handling: `keep`, `flag`, or `remove`
- Missing value handling: `keep`, `drop_rows`, or impute
- Numeric imputation: `median`, `mean`, or `zero`
- Categorical imputation: `mode` or `"Missing"`

### 3. Preprocessing

The Preprocessing tab supports:

- Outlier handling: `none`, `cap`, or `remove`
- Scaling: `none`, `standard`, `minmax`, or `robust`
- Categorical encoding: `none`, `onehot`, or `label`

For performance reasons, preprocessing is conservative by default. This makes the app more stable for large real-world datasets.

### 4. Feature Engineering

Users can create new features interactively using:

- `add`
- `subtract`
- `multiply`
- `divide`
- `log`
- `square`

The app stores feature rules and updates the working dataset reactively.

#### Before vs After Comparison

To help users understand the impact of transformations, the app includes:

- Side-by-side distribution plots (before vs after)
- Summary statistics comparison (mean, sd, min, median, max)

This allows users to directly observe how transformations such as log scaling or normalization affect the data distribution.

### 5. EDA and Visualization

The app includes interactive Plotly visualizations and summary outputs:

- Histogram
- Box plot
- Scatter plot
- Bar chart
- Summary statistics table
- Correlation heatmap
- Dynamic variable selection
- Optional filtering for exploration

### 6. Export

Users can download the current transformed dataset as a CSV file from the Export / Download tab.


## Required Packages

Install the required R packages before running the app:

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "readxl", "jsonlite"))
```

## How to Run

In RStudio:
1. Open the project folder.
2. Open `app.R`.
3. Click `Run App`.


## Workflow

1. Go to the `Load Data` tab.
2. Upload a file or load a built-in dataset.
3. Review the raw data preview.
4. Use the `Cleaning` tab to apply optional cleaning steps.
5. Use the `Preprocessing` tab to selectively apply encoding, scaling, or outlier handling.
6. Use `Feature Engineering` to create derived variables.
7. Use `EDA / Visualization` to explore distributions and correlations.
8. Download the final dataset from `Export / Download`.

## Some notes:

- Current upload limit is set to approximately `300 MB`
For large files, this workflow is recommended:

- Upload first and confirm the preview loads
- Leave preprocessing options at `none` initially
- Avoid applying one-hot encoding to identifier columns such as IDs
- Encode only a small set of categorical columns when needed

## Troubleshooting

### If app does not respond after upload

Common causes:

- The dataset is very large and the browser needs time to update
- Too many columns were selected for one-hot encoding
- The app was not restarted after changing `app.R`

Recommended fix:

1. Stop the app completely
2. Restart it
3. Upload the file again
4. Keep cleaning and preprocessing settings minimal at first



