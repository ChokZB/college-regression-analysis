# Regression Modelling with the ISLR2 College Dataset in R

This repository presents a regression analysis performed on the **College** dataset from the [`ISLR2`](https://cran.r-project.org/web/packages/ISLR2/index.html) package in R.  

The analysis explores relationships between various institutional characteristics and student outcomes to demonstrate statistical modelling techniques using R.

---

## 🎯 Objectives
- Explore and visualise the College dataset.
- Apply multiple linear regression techniques.
- Evaluate model performance and interpret significant predictors.
- Demonstrate data cleaning, exploratory analysis, and regression diagnostics.

---

## 🧮 Dataset
The `College` dataset is included in the `ISLR2` R package and contains information on U.S. colleges such as:
- Number of applications and acceptances
- Tuition and room costs
- Graduation rate
- Student-to-faculty ratio
- Type of institution (Private or Public)

---

## ⚙️ Methods
1. Data exploration and summary statistics  
2. Correlation analysis and visualisation  
3. Linear and multiple regression modelling  
4. Model diagnostics and residual analysis  
5. Interpretation of coefficients and performance metrics

---

## 📁 Project Structure
| File / Folder | Description |
|----------------|-------------|
| `college_regression_analysis.R` | Main R script performing the analysis |
| `README.md` | Project documentation |
| `.gitignore` | Specifies files and folders to exclude from version control |

---

## 🧰 Requirements
- **R version ≥ 4.0**
- Required libraries:  
  ```r
  install.packages(c("ISLR2", "ggplot2", "dplyr", "corrplot"))
  ```
