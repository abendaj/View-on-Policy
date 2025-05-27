# View-on-Policy

This project investigates how different learning formats—specifically academic papers and documentaries—affect participants’ knowledge retention and attitudes toward immigrants, refugees, and climate change. The study is implemented as a randomized controlled trial (RCT) with four treatment groups and one control group:

- **Short Paper**
- **Documentary Video**
- **Paper followed by Video**
- **Video followed by Paper**
- **Control Group** (no intervention)

## Overview

The analysis involves multiple components:

- 📊 **PCA (Principal Component Analysis):** Used to create indices from survey responses.
- 🧠 **Open-Ended Text Analysis:** Responses to open-ended questions are analyzed using OpenAI's API and supplemented with machine learning techniques to classify themes and sentiment.
- 📈 **Quantitative Analysis:** Includes descriptive statistics, balance tables, consistency checks, and regressions to evaluate treatment effects.

A second stage of the study is currently underway to examine how responses change over time.

---

## How to Run the Code

To reproduce the full analysis, execute the scripts in the following order:

1. `experiment_cleaning.R` — Cleans and preprocesses the raw data.  
2. `experiment_mapping.R` — Maps treatments and sets up survey structure.  
3. `text_analysis.py` — Applies ML-based classification to open-ended responses.  
4. `experiment_pca.R` — Runs PCA and builds indices.  
5. `experiment_open_ended.py` — Uses OpenAI's API to analyze open-ended survey responses.  
6. `experiment_clean_llm_results.R` — Cleans and integrates LLM-generated analysis.  
7. `experiment_info.R` — Final stage of data analysis and visualization.

---

## Regressions

The `Regressions/` folder contains several regression models used for estimating treatment effects and conducting robustness checks.

---

## Dependencies

- **R Packages:** `readxl`, `dplyr`, `writexl`, `gtsummary`, `haven`, `httr`, `jsonlite`, `stringr`, `purrr`, `tidyr`
- **Python Packages (for text analysis):** `pandas`, `openai`, `scikit-learn`, `nltk`, etc.

Make sure to set your OpenAI API key properly in the environment before running `experiment_open_ended.py`.
