# LCA and LPA Workshop Analysis

This repository contains an R Markdown file (`LCA_LPA_Workshop_Analysis.Rmd`) that provides a workflow for analyzing latent class analysis (LCA) and latent profile analysis (LPA) for a workshop setting. The analysis covers data preparation, model fitting, profile extraction, and visualization.

## Contents

- **LCA_LPA_Workshop_Analysis.Rmd**: The main R Markdown script for the workshop analysis.
- **ugu2004_wellbeing_leisure_grade.sav**: The data file required for the analysis (not included in this repo, see below).
- **README.md**: This documentation.

## Requirements

Before running the analysis, ensure you have the following:

- R (version 4.0 or newer recommended)
- R packages:
    - `tidyverse`
    - `rmarkdown`
    - `janitor`
    - `dplyr`
    - `poLCA`
    - `tidyLPA`
    - `haven`
    - `tibble`
    - `ggplot2`
    - `purrr`
    - `nnet`
    - `tidyr`

You can install the required packages with:

```r
install.packages(c("tidyverse", "rmarkdown", "janitor", "dplyr", "poLCA", 
                   "tidyLPA", "haven", "tibble", "ggplot2", "purrr", "nnet", "tidyr"))
```

## Data


The repository includes the SPSS `.sav` file `ugu2004_wellbeing_leisure_grade.sav` required for running the LCA analysis and 'IQ_10HIGHEST.SAV' for the LPA analysis.  


## Usage

1. **Open** the `LCA_LPA_Workshop_Analysis.Rmd` file in RStudio or any R Markdown-compatible IDE.
2. **Ensure** that the data file `ugu2004_wellbeing_leisure_grade.sav` is in your working directory.
3. **Run** the R Markdown file by clicking "Knit" or running `rmarkdown::render("LCA_LPA_Workshop_Analysis.Rmd")`.

## Analysis Overview

The R Markdown script performs the following steps:

1. **Load Packages:** Loads necessary R packages.
2. **Load and Clean Data:** Reads in the data and cleans column names.
3. **Variable Definition and Missingness Check:** Selects variables for LCA and checks for missing data.
4. **Model Fitting:** Fits LCA models with 2, 3, and 4 classes using the `poLCA` package and compares fit statistics.
5. **Profile Extraction & Visualization:** Extracts latent class profiles and visualizes the estimated mean responses for each class across models.

## Output

The output will be an HTML document containing:

- Model fit statistics (Log Likelihood, BIC, AIC) for models with varying numbers of classes
- Visualizations of class profiles for each model

## Author

- Leah Glassow (copyright) leahnatashahill@gmail.com

---

For questions or support, please open an issue or contact the authors directly.
