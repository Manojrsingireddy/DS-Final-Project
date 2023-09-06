# Fraud Or Not Job Postings

## Project Overview

The project aims to analyze a dataset of job postings to predict fraudulent job postings using various machine learning techniques.

## Table of Contents

- [Project Overview](#project-overview)
- [Project Structure](#project-structure)
- [Dependencies](#dependencies)
- [Data Source](#data-source)
- [Data Preprocessing](#data-preprocessing)
- [Model Building](#model-building)
- [Usage](#usage)

## Project Structure

The project structure is organized as follows:

- **Data:** The dataset used for analysis, `fake_job_postings.csv`.
- **Code:** R scripts for data preprocessing, analysis, and model building.
- **Output:** Output files generated during the analysis.
- **README.md:** This documentation file providing an overview of the project.

## Dependencies

The project uses various R libraries for data analysis and modeling. You can install these libraries using the following commands:

```R
# Install required R packages
install.packages("tidyverse")
install.packages("modelsummary")
# ... (repeat for other libraries)
```

## Data Source
The dataset used in this project is available in the CSV format within the repo:

## Data Preprocessing
Data preprocessing steps include:

- Handling missing values.
- Creating additional variables to indicate the presence of certain information (e.g., company profile, location, etc.).
- Converting categorical variables into factors for analysis.
- Data Analysis
- The project includes exploratory data analysis (EDA) steps such as creating histograms to analyze the correlation between company profiles, logos, and fraudulent job postings.

## Model Building
The project explores various machine learning techniques to predict fraudulent job postings:

- Ridge Regression
- Lasso Regression
- Tree Bagging
- Neural Network
Each technique is evaluated, and their respective confusion matrices are displayed to assess the model's performance.

## Usage
To run this project, follow these steps:

- Clone this repository to your local machine.

```bash
git clone https://github.com/Manojrsingireddy/DS-Final-Project.git
```
- Install the required R packages as mentioned in the "Dependencies" section.

- Execute the R script