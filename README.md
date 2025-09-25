# Statistical Analysis of Employee Attrition

## 📌 Project Objective
This project performs a comprehensive statistical analysis on the **IBM HR Attrition dataset** to identify the key factors that drive employee turnover.  
The goal is to move beyond anecdotal evidence and provide **data-driven insights** that can inform targeted employee retention strategies.

---

## 📂 Dataset
The analysis uses the publicly available **IBM HR Analytics Employee Attrition & Performance** dataset from [Kaggle](https://www.kaggle.com/).

---

## 🛠 Methodology
The analysis follows a structured statistical workflow:

1. **Data Cleaning**  
   - Preprocessing the data to handle missing values  
   - Removing irrelevant columns  

2. **Exploratory Data Analysis (EDA)**  
   - Visualizations and descriptive statistics  
   - Uncovering initial patterns and relationships  

3. **Inferential Statistics**  
   - Formal hypothesis tests (Two-Sample T-tests, Chi-Square Tests)  
   - Rigorous assumption checks to confirm significance  

---

## 🔑 Key Findings
The analysis concluded that the following factors are **statistically significant drivers of employee attrition**:

- **Low Monthly Income** → Employees who leave have a significantly lower monthly income on average than those who stay.  
- **Working Overtime** → Strong association with attrition likelihood.  
- **Specific Job Roles** → Roles like *Sales Representative* and *Laboratory Technician* have disproportionately high turnover rates.  

---

## 🧰 Tools Used
- **Language:** R  
- **Key Libraries:** `dplyr`, `ggplot2`, `tidyr`, `corrplot`  

---

## ▶️ How to Run
1. Ensure you have **R** and **RStudio** installed.  
2. Place the file `WA_Fn-UseC_-HR-Employee-Attrition.csv` in the same directory as the R script.  
3. Open the R script and **run it from top to bottom** to replicate the full analysis.  
