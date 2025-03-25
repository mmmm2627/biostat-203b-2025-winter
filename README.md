## ICU Stay Prediction and Patient Insights Dashboard with R Shiny

### Dataset 
- **MIMIC-IV v.3.1**: [Link to Dataset](https://physionet.org/content/mimiciv/3.1/)

MIMIC-IV is a large, de-identified dataset of patient admissions to the emergency department and intensive care units at the Beth Israel Deaconess Medical Center in Boston, MA. It includes data from over **65k** ICU patients and **200k** emergency department patients, capturing demographics, diagnoses, procedures, vitals, and ICU stay details.

### Overview

This project focuses on two key goals:
1. **Interactive Data Visualization**: Built an R Shiny app for dynamic visualization of variable distributions, statistical summaries, and patient information retrieval. The dashboard allows users to explore ICU admission trends, vital sign measurements, and patient-specific ADT (Admission, Discharge, and Transfer) history.

2. **ICU Stay Prediction**: Developed a machine learning pipeline in R to predict ICU stay length (>2 days) using logistic regression, Random Forest, XGBoost, and model stacking. Implemented data imputation methods to handle missing data and compared model performance using AUC and accuracy metrics.

### Project Components
- [HW1](https://github.com/mmmm2627/biostat-203b-2025-winter/tree/44210f5a41267f3286f9331c0ad3aaff73df748e/hw1):
  - Set up GitHub repository.
  - Download and manage MIMIC-IV data in a Linux environment using WSL (Ubuntu).
  - Explored file sizes and data characteristics using Bash commands for efficient data handling.
 
- [HW2](https://github.com/mmmm2627/biostat-203b-2025-winter/tree/44210f5a41267f3286f9331c0ad3aaff73df748e/hw2):
  - Evaluated the efficiency of reading large compressed files using:
    - `read.csv` (**base R**)
    - `read_csv` (**tidyverse**)
    - `fread` (**data.table**)
  - Assessed the effect of user-supplied data types on runtime and memory usage.
  - Used **arrow::open_dataset** for efficient large-scale data handling.
  - Compressed files to **Parquet** format and utilized **DuckDB** for querying and filtering large datasets.
 
- [HW3](https://github.com/mmmm2627/biostat-203b-2025-winter/tree/44210f5a41267f3286f9331c0ad3aaff73df748e/hw3):
  - Developed patient-specific visualizations using **ggplot2**:
    - **ADT History Visualization**: Visualized patient admission, discharge, and ICU department transfers over time.
    - **Vitals Monitoring**: Tracked patient vitals (e.g., heart rate, blood pressure, respiratory rate, body temperature) during ICU stays.
  - Built comprehensive tables by merging data from multiple sources (e.g., admissions, patient data, lab results, and vitals) for a unified patient view.
  - Implemented statistical summary visualizations to explore variable distributions and identify trends in the dataset.

- [HW4](https://github.com/mmmm2627/biostat-203b-2025-winter/tree/44210f5a41267f3286f9331c0ad3aaff73df748e/hw4):
  - Connected to a **Google BigQuery** database to retrieve and query data directly.
  - Combined patient data from various tables using **SQL** queries for efficient data extraction.
  - Integrated the data with the **R Shiny app** to enable interactive visualization and exploratory data analysis.
  - Visualized data using **ggplot2** for clear and informative graphical representations. 

- [HW5](https://github.com/mmmm2627/biostat-203b-2025-winter/tree/44210f5a41267f3286f9331c0ad3aaff73df748e/hw5):
  - Built and tuned models using **logistic regression**, **Random Forest**, **XGBoost**, and **model stacking**.
  - Applied multiple imputation methods to address missing data.
  - Compared model performance using metrics like **AUC** and **accuracy**.

### Purpose and Impact
This project provides actionable insights for healthcare professionals by:
- Enabling real-time exploration of patient data and ICU admission patterns using the interactive Shiny app.
- Supporting early decision-making in ICU settings by accurately predicting ICU stay lengths using machine learning.
- Facilitating personalized patient monitoring with detailed ADT history and vitals visualization.
- Streamlining data management and analysis by connecting directly to Google BigQuery for scalable querying.
- Demonstrating the impact of data management, preprocessing choices, and model optimization on predictive performance.

These insights can be valuable for resource allocation, patient monitoring, and improving healthcare outcomes.
