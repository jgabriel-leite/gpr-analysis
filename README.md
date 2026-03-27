# gpr-analysis

#Geopolitical Risk Index (GPR) Analysis
This project analyzes the Geopolitical Risk Index (GPR) across selected countries, with a focus on **Latin America and BRICS+ economies**. It combines time series visualization with correlation analysis to explore co-movements in geopolitical risk.
---

## Project Structure

```
├── data-raw/        # Raw data (auto-downloaded, not tracked)
├── plots/           # Generated figures (not tracked)
├── gpr_analysis.R   # Main analysis script
├── README.md
```

---
## Data Source

* Source: http://www.matteoiacoviello.com/gpr.htm
* Dataset: Global Geopolitical Risk Index (GPR)

The dataset is automatically downloaded and stored locally in `data-raw/`.

---

## Workflow

The script implements a fully reproducible pipeline:

1. **Create directories** (`data-raw/`, `plots/`)
2. **Download dataset**
3. **Import data**
4. **Transform to long format (tidy)**
5. **Filter relevant country groups**
6. **Generate visualizations**
7. **Export plots automatically**

---

## Country Coverage

### Latin America

* Argentina
* Brazil
* Chile
* Colombia
* Mexico
* Peru
* Venezuela

### BRICS+

* Brazil
* China
* Egypt
* Indonesia
* India
* Russia
* South Africa

---

## Outputs

The project generates:

### 1. Time Series Plots

* GPR evolution across countries
* Separate visualizations for:

  * Latin America
  * BRICS+

### 2. Correlation Heatmaps

* Cross-country correlation of GPR
* Visual identification of co-movement patterns

All outputs are saved in the `plots/` folder.

---

## Data Notes

* Missing values are removed before analysis
* Data availability starts around **1990** for most countries
* Country codes are converted to readable names

---

## Requirements

R packages:

```r
readxl
dplyr
tidyr
ggplot2
reshape2
```

---

## How to Run

Open the project in RStudio and run:

```r
Ctrl + Shift + Enter
```

---

## Analytical Focus

This project is designed for:

* Comparative geopolitical risk analysis
* Identification of synchronized shocks
* Exploration of regional vs global dynamics
* This project is meant to be a very accessible basis for those interested in exploring the database
* It could be further developed to analyse the historical data of the database, to correlate with other variables, such as debt, sanctions and protectionist measures

## Author

João Leite
Brazilian Ministry of Foreign Affairs
