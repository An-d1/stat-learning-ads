# stat-learning-ads

# Statistical Learning Project — Global Ads Performance (R)

This repository contains our Statistical Learning group project using a global ads performance dataset.  
We perform:
- **Exploratory Data Analysis (EDA)**
- **Supervised learning** (regression + model comparison, optionally regularization)
- **Unsupervised learning** (PCA + clustering)
- A reproducible workflow in **R** using **renv**

---

## Repository Structure

├── data/
│ ├── raw/ # Original dataset (do not edit)
│ └── processed/ # Cleaned/feature-engineered data created by scripts
├── R/ # R scripts (run in order)
├── outputs/
│ ├── figures/ # Generated plots
│ └── tables/ # Generated tables
├── report/ # Final report (.Rmd) and knitted outputs
├── renv.lock # Locked package versions (reproducibility)
├── .Rprofile # Auto-activates renv when opening the project
└── *.Rproj # RStudio project file 

## Requirements

- **R** (recommended: R 4.x)
- **RStudio** (recommended)
- Internet access (first setup only, to install packages)

---

## Quick Start 

### 1) Clone the repository
```
git clone <https://github.com/An-d1/stat-learning-ads.gitL>
cd <REPO_FOLDER>
```
### 2) Open the project in RStudio

```Open the *.Rproj file in the repo root.
```
### 3) Restore package environment (renv)

```In the R console:
  install.packages("renv")
  renv::restore()
  
  -This installs the exact package versions used in this project (as recorded in renv.lock).
```
### 4) Load the DataSet
```The dataset is included in:
data/raw/global_ads_performance_dataset.csv
```
