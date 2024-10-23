# Predictive Analytics in R: Property Price Assessment
## Project Overview
This project was completed as part of the BUS5PA Predictive Analytics course. The goal was to build and evaluate predictive models to assess property prices using a dataset of 3970 property sales. The models aim to predict property values based on various features such as lot size, house age, and the number of bathrooms.

This project includes data exploration, cleaning, and applying regression and decision tree models to identify the most suitable predictive model for assessing property prices.

## Technologies Used
•	R programming language
•	R Libraries: ggplot2, dplyr, caret, randomForest, rpart, tidyverse

## Assignment Objectives
The project is divided into three parts:
1.	Problem Formulation: Identify key property features that affect price and discuss data sources.
2.	Data Exploration and Cleaning: Analyze and clean the dataset, apply transformations, handle missing values, and reduce dimensions where necessary.
3.	Building Predictive Models: Develop regression and decision tree models to predict property prices. Compare multiple models to identify the optimal solution.
   
## Setup Instructions
To run the analysis, follow these steps:
1.	Clone this repository:
git clone https://github.com/yourusername/predictive-analytics-assignment.git
2.	Open the project in RStudio.
3.	Install the required libraries by running:
install.packages(c("ggplot2", "dplyr", "caret", "randomForest", "rpart", "tidyverse"))
4.	Execute the scripts in the following order:
o	data_exploration.R: Explores the dataset, performs summary statistics, and creates visualizations.
o	data_cleaning.R: Handles missing values, outliers, and applies transformations.
o	regression_model.R: Builds and evaluates different regression models.
o	decision_tree_model.R: Builds and evaluates different decision tree models.

## Key Results
•	Regression Models: We built multiple regression models and performed feature selection to optimize predictions
. The final model had an R-squared value of 0.78, indicating a strong predictive capability.
•	Decision Tree Models: We built and pruned decision tree models to achieve the best results. The optimal tree provided an intuitive explanation of the factors affecting property prices.
•	Model Comparison: The decision tree model performed slightly better than the regression model based on accuracy and interpretability, making it the recommended solution for the business case.
## Project Files
•	data_exploration.R: Exploratory data analysis and data transformation.
•	data_cleaning.R: Data cleaning and handling missing values.
•	regression_model.R: Builds multiple regression models and compares them.
•	decision_tree_model.R: Builds and evaluates decision trees.
•	plots/: Contains plots generated during the analysis.
