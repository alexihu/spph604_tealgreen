-----

# Polypharmacy & Mortality in NHANES (2003–2018)

This repository contains the R code and reproducible analysis workflow for the Teal Green team project for SPPH604 at UBC on investigating the association between polypharmacy (levels of prescription medication use) and all-cause mortality among U.S. adults aged 45 years and older, using data from the National Health and Nutrition Examination Survey (NHANES).

## Authors

**Teal Green Group:** Alexi Hu, Alex Tam, Ezra Yu from UBC

## Repository Contents

  * **`data_clean.R`**: Script to download NHANES demographic, questionnaire, and examination data, merge with mortality files, and clean variables.
  * **`teal_green_code.Rmd`**: The main analytic R Markdown file. It performs multiple imputation, runs survey-weighted Cox Proportional Hazards models, creates Kaplan-Meier curves, and runs sensitivity analyses.
  * **`save_plots.R`**: Standalone script to generate and save high-resolution publication-quality figures (Kaplan-Meier, Interaction plots, Forest plots) to the `figures/` folder.

## Prerequisites

To run these scripts, **R** needs to be installed. The analysis relies on the following R packages:

```r
install.packages(c(
  "tidyverse", "survey", "survival", "gtsummary", "gt", "broom", 
  "mice", "mitools", "naniar", "ggplot2", "scales", "DataExplorer", 
  "splines", "car", "forcats", "stringr", "kableExtra", "patchwork", 
  "nhanesA", "janitor", "haven", "readr"
))
```

## Directory Structure

To ensure the scripts run without path errors, organize the project folder as follows:

```text
.
├── data/
│   └── mortality/      <-- Place downloaded NHANES Linked Mortality .dat files here
├── figures/            <-- High-res plots will be saved here
├── data_clean.R
├── teal_green_code.Rmd
├── save_plots.R
└── README.md
```

## Instructions for Reproduction

### 1\. Data Preparation

The **`data_clean.R`** script downloads most data directly from the CDC via the `nhanesA` package. However, you must manually download the **Public-use Linked Mortality Files** (Follow-up through 2019) and place the `.dat` files in the `data/mortality/` folder.

1.  Open `data_clean.R`.
2.  Run the entire script.
3.  **Output:** A processed file named `analytic_raw.rds` will be saved in the `data/` folder.

### 2\. Main Analysis

The analysis is contained in an R Markdown file, which ensures reproducibility of the text, tables, and statistics.

1.  Open `teal_green_code.Rmd`.
2.  Ensure `data/analytic_raw.rds` exists (from Step 1).
3.  Click **Knit** (Knit to HTML, PDF, or Word).
4.  **Output:** An HTML/PDF report containing descriptive tables, imputation diagnostics, Cox model results (HRs), and initial plots.

### 3\. Generate High-Resolution Figures

For publication-quality images (300 DPI) of the Kaplan-Meier curve, Interaction plot, and Grand Forest plot:

1.  Open `save_plots.R`.
2.  Run the script.
3.  **Output:** The following files will appear in the `figures/` folder:
      * `Figure1_KaplanMeier.png` (Includes aligned risk table)
      * `Figure2_Interactions.png` (Faceted by therapeutic class)
      * `Figure3_GrandForestPlot.png` (Main models + Sensitivity analyses)

## Analysis Details

  * **Design:** Complex survey design (strata, clusters, weights).
  * **Imputation:** Multiple Imputation by Chained Equations (MICE) was used for missing income and insurance data.
  * **Models:** Survey-weighted Cox Proportional Hazards models.
  * **Sensitivity Analyses:** Includes excluding antibiotics, restricting to higher multimorbidity counts, and censoring follow-up at 5/10 years.
