# CDISC ADaM Creation & Validation - Project 2-B

Derives 5 ADaM datasets (ADSL, ADAE, ADLBSI, ADTTE, ADEX) from SDTM source data for study **CMP135**, and validates the output against gold-standard reference XPT files.

## Project Structure

```
SDTM xpt/              # Source SDTM XPT files (SAS .xpt)
ADaM xpt/              # Reference ADaM XPT files for validation
Project 2-B.R          # Main R script (all derivations + validation)
Project 2-B.Rmd        # R Markdown version (knits to HTML)
Project 2-B.html       # Compiled report
```

## ADaM Datasets

| Dataset | Description |
|---|---|
| **ADSL** | Subject-Level Analysis - population flags (ITT, SAF, EFF, CA125), treatment, disposition, prior therapy, baseline measures |
| **ADAE** | Adverse Events - treatment-emergent flag, severity, relationship, action taken |
| **ADLBSI** | Lab Safety (SI) - chemistry, hematology, urinalysis with baseline, change, toxicity grades |
| **ADTTE** | Time-to-Event - progression-free survival (TTPFS), CA-125 responder PFS (TTPFS125), overall survival (TTOS) |
| **ADEX** | Exposure - treatment duration, cumulative capsules/dose, dose intensity |

## Usage

Open and run `Project 2-B.R` in RStudio. Each derived dataset is compared against the reference XPT files using `identical()`, `all.equal()`, and `anti_join()`.

## Requirements

R packages: **dplyr**, **tidyr**, **tibble**, **lubridate**, **haven**, **stringr**, **readxl**
