# loading necessary libraries
# install.packages('Amelia', dependencies = TRUE)
# install.packages('caret', dependencies = TRUE)
# install.packages('plotly', dependencies = TRUE)
# install.packages('GGally', dependencies = TRUE)
# install.packages('rpart.plot', dependencies = TRUE)
# install.packages('lattice')
install.packages("purrr")
# install.packages("MASS")
# install.packages("rpart")
install.packages("tidyr") 

library(tidyr)
library(psych)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(corrplot)
library(Amelia)
library(GGally)
library(plotly)
library(caret)
library(tidyr)
library(purrr)
library(rpart.plot)
library(MASS)
library(rpart)

# loading the data file.
property_prices <- read.csv("HousingValuation.csv")
View(property_prices)

# identifying numeric or categorical variables
summary(property_prices)

# to assess discrete/continuous/ordinal/nominal dataset
head(property_prices, 3)

# Question 1 c

# Demonstrating transformation of categorical variables to numerical 
# One-Hot Encoding
housing_prices <- dummy_cols(
  property_prices  ,
  select_columns = c('LotShape', 'LandContour', 'Utilities', 'LotConfig', 'Slope', 
                     'DwellClass', 'OverallQuality', 'OverallCondition', 'ExteriorCondition',
                     'BasementCondition', 'CentralAir', 'GarageType', 'PavedDrive', 'KitchenQuality'),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE)

View(housing_prices)


# Question 2 a 
# creating continuous and categorical variables
continuous_vars <- property_prices %>% select('LotArea', 'TotalBSF', 'LowQualFinSF', 'OpenPorchSF','SalePrice','YearBuilt','PoolArea', 'Id', 'GarageCars', 'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'Fireplaces', 'YrSold', 'MoSold', 'TotalRmsAbvGrd')


categorical_vars <- property_prices %>% select('LotShape', 'LandContour', 'Utilities', 'LotConfig', 'Slope', 
                                               'DwellClass', 'OverallQuality', 'OverallCondition', 'ExteriorCondition',
                                               'BasementCondition', 'CentralAir', 'GarageType', 'PavedDrive', 'KitchenQuality')


# summary along with standard deviation
summary_with_sd <- sapply(continuous_vars, function(x) {
  c(Mean = mean(x, na.rm = TRUE), # removing na to avoid biasness
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE))
})


# Display the summary statistics
print(summary_with_sd)

# calculating the count of each categorical variables
category_counts <- sapply(categorical_vars, function(x) {sum(!is.na(x))})
print(category_counts)



# Question 3 
library(ggplot2)

# histogram of each continuous variables

continuous_vars %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# summary statistics of continuous_vars

summary(continuous_vars)

# Question 4

# checking for missing values in the entire dataset

missing_values <- sapply(property_prices, function(x) sum(is.na(x)))

missing_values[missing_values > 0]

missing_value_columns <- c("YearBuilt", "LivingArea", "GarageType")


# Question 4 c
# replace missing values with 0

property_prices_0 <- property_prices 
property_prices_0[is.na(property_prices_0)] <- 0

summary(property_prices_0)
# cross-checking for replacing missing values with 0
summary(property_prices$LivingArea)

summary(property_prices_0$LivingArea)


# delete records with missing values

property_prices_NAdeleted <- property_prices[complete.cases(property_prices),]
summary(property_prices_NAdeleted)

# cross-checking for missing values deletion
summary(property_prices$LivingArea)

summary(property_prices_NAdeleted$LivingArea)

# replace missing values with the mean of each variable

property_prices_clean <- property_prices
property_prices_clean[] <- lapply(property_prices_clean, function(x) {
  if(is.numeric(x)) { replace(x, is.na(x), mean(x, na.rm = TRUE)) } 
else if (is.character(x)) {
  mode_value <- as.character(names(sort(table(x),decreasing= TRUE))[1])
  replace(x, is.na(x), mode_value)
}
    else { x }
})

summary(property_prices_clean)

# cross-checking the execution of mean imputation
summary(property_prices_clean$LivingArea) # check for missing values 
summary(property_prices$LivingArea)

# Question 5
#check for missing values
colSums(is.na(property_prices_clean))

# missmap to visually ensure the data is free from NA's
missmap(property_prices_clean, col = c('yellow','blue'),y.at = 1,y.labels='',legend=TRUE)

# observe the distribution of variables

selected_attr <- property_prices_clean$SalePrice 

par(mfrow=c(1,2))
hist(selected_attr, col='orange', main = 'Histogram')
plot(density(selected_attr, na.rm = TRUE), main = "Density")


property_prices_clean %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

# using log to balance the right-skewness 
trans_salesprice <- log(property_prices_clean$SalePrice)
par(mfrow=c(1,2))
hist(property_prices_clean$SalePrice, col = "orange", main = "Original")
hist(trans_salesprice, col = "orange", main = "Transformed")

# remove unwanted variables from the data frame
reject_vars <- property_prices_clean %in% c("Id")
property_prices.new <- subset(property_prices_clean, select = -c(Id))
View(property_prices.new)

# changing the character columns to factors
property_prices.new$LandContour <- factor(property_prices.new$LandContour, levels=c("Lvl","Bnk","HLS", "Low"), labels = c(0,1,2,3))
property_prices.new$Utilities <- factor(property_prices.new$Utilities, levels=c("AllPub", "NoSewr","NoSeWa", "ELO"), labels = c(0,1,2,3) )
property_prices.new$LotConfig <- factor(property_prices.new$LotConfig, levels=c("Inside", "Corner","CulDSac", "FR2", "FR3"), labels = c(0,1,2,3,4) )
property_prices.new$Slope <- factor(property_prices.new$Slope, levels=c("Gtl", "Mod","Sev"), labels = c(0,1,2))
property_prices.new$DwellClass <- factor(property_prices.new$DwellClass, levels=c("1Fam", "1FmCon","Duplx", "TwnhsE", "TwnhsI"), labels = c(0,1,2,3,4) )
property_prices.new$ExteriorCondition <- factor(property_prices.new$ExteriorCondition, levels=c("Ex", "Gd","TA", "Fa", "Po"), labels = c(0,1,2,3,4) )
property_prices.new$BasementCondition <- factor(property_prices.new$BasementCondition, levels=c("Ex", "Gd","TA", "Fa", "Po", "NB"), labels = c(0,1,2,3,4,5) )
property_prices.new$CentralAir <- factor(property_prices.new$CentralAir, levels=c("N", "Y"), labels = c(0,1))
property_prices.new$KitchenQuality <- factor(property_prices.new$KitchenQuality, levels=c("Ex", "Gd","TA", "Fa", "Po"), labels = c(0,1,2,3,4))
property_prices.new$GarageType <- factor(property_prices.new$GarageType, levels=c("2Types", "Attchd","Basment", "BuiltIn", "CarPort", "Detchd","NA"), labels = c(0,1,2,3,4,5,6) )
property_prices.new$PavedDrive <- factor(property_prices.new$PavedDrive, levels=c("Y", "P","N"), labels = c(0,1,2))

property_prices.new$LandContour <- as.numeric(as.character(property_prices.new$LandContour))
property_prices.new$Utilities <- as.numeric(as.character(property_prices.new$Utilities))
property_prices.new$LotConfig <- as.numeric(as.character(property_prices.new$LotConfig))
property_prices.new$Slope <- as.numeric(as.character(property_prices.new$Slope))
property_prices.new$DwellClass <- as.numeric(as.character(property_prices.new$DwellClass))
property_prices.new$ExteriorCondition <- as.numeric(as.character(property_prices.new$ExteriorCondition))
property_prices.new$BasementCondition <- as.numeric(as.character(property_prices.new$BasementCondition))
property_prices.new$CentralAir <- as.numeric(as.character(property_prices.new$CentralAir))
property_prices.new$KitchenQuality <- as.numeric(as.character(property_prices.new$KitchenQuality))
property_prices.new$GarageType <- as.numeric(as.character(property_prices.new$GarageType))
property_prices.new$PavedDrive <- as.numeric(as.character(property_prices.new$PavedDrive))

# setting target var
target <- property_prices.new$SalePrice
View(target)

View(property_prices.new)
property_prices.new <- subset(property_prices_clean, select = -c(SalePrice))

# question 5.a.
ggcorr(property_prices.new, label = TRUE)

# converting data to matrix for correlation analysis
M <- data.matrix(property_prices.new)

CorrM <- cor(M)

any(is.na(CorrM))
CorrM <- CorrM[complete.cases(CorrM), complete.cases(CorrM)]
CorrM[is.na(CorrM)] <- 0

# question 5.b. find highly correlated variables (cutoff of 0.5)
highlyCorrM <- findCorrelation(CorrM, cutoff = 0.5)

print(names(property_prices_clean)[highlyCorrM])

# removing variables with high correlation and by assessing the knowledge of domain
property_prices_selected = subset(property_prices.new, select = -c(Id, LivingArea, FullBath, YearBuilt, OverallQuality, YrSold, MoSold))

ggcorr(property_prices_selected, label = TRUE)

# merging the target variable back to the dataset
property_prices_selected$SalePrice <- target
View(property_prices_selected)

#---------------------------------------------------------

# Part C - Building Predictive Models
# Question 1
# setting up sample data
smp_size <- floor(2/3 * nrow(property_prices_selected))
set.seed(2) # ensuring that same dataset is sampled

# sampling the dataset
property_prices_selected <- property_prices_selected[sample(nrow(property_prices_selected)), ]

# creating training dataset and testing dataset

property.train <- property_prices_selected[1:smp_size, ]
View(property.train)
property.test <- property_prices_selected[(smp_size+1):nrow(property_prices_selected),]
View(property.test)

formula = SalePrice ~.
# fit the linear regression algorithm
model <- lm(formula = formula, data = property.train)

View(property_prices_selected)

# display coefficients of linear regression model
summary(model)$coefficients

summary(model)

# predicmodel# predictions for test and train datasets
property.train$predicted.SalesPrice <- predict(model, property.train)

property.test$predicted.SalesPrice <- predict(model, property.test)

print("Actual Values")

head(property.test$SalePrice)

print("Predicted Values")

head(property.test$predicted.SalesPrice[1:5])

# plot predicted values vs actual values of the target variables
pl1 <- property.test %>%
  ggplot(aes(SalePrice, predicted.SalesPrice)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(colour='red')) + 
  xlab('Actual value of SalesPrice') +
  ylab('Predicted value of SalesPrice') + 
  theme_bw()
ggplotly(pl1)


# Calculating the r square value
r_squared <- summary(model)$r.squared
print(paste("R Squared: ", r_squared))

# Calculating the Root Mean Squared Error (RMSE)
error <- property.test$SalePrice - property.test$predicted.SalesPrice
rmse <- sqrt(mean(error^2))

print(paste("Root Mean Square Error: ", rmse))

# Second Models
# Stepwise Regression for Feature Selection
stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)

# R squared for stepwise model
r_squared1 <- summary(stepwise_model)$r.squared
print(paste("R squared (stepwise model): ", r_squared1))

# RMSE for Stepwise Model
property.test$predicted.SalePrice1 <- predict(stepwise_model, property.test)
rmse1 <- sqrt(mean((property.test$SalePrice - property.test$predicted.SalePrice1)^2))
print(paste("RMSE (stepwise model): ", rmse1))

# Third Model
# Model with domain knowledge based features of the properties
domain_features <- c("SalePrice","LotArea", "Utilities", "GarageType", "TotalBSF", "OverallCondition")
domain_model <- lm(SalePrice ~ ., data = property.train[, domain_features])
# Predicting on test data using domain features
domain_predicted_values <- predict(domain_model, newdata = property.test)
# Calculate r squared and RMSE for domain features model
summary(domain_model)
print(paste("R Squared: 0.5438"))


error2 <- property.test$SalePrice - property.test$predicted.SalesPrice
rmse2 <- sqrt(mean(error2^2))
print(paste("Root Mean Square Error: ", rmse2))


# Decision Tree 
# Question 2
# selecting property_prices_clean dataframe for the decision tree as it is free from missing values and errors
smp_size_decision <- floor(2/3 * nrow(property_prices_clean))
set.seed(2)

# sample the dataset

property_prices.selected <- property_prices_clean[sample(nrow(property_prices_clean)),]


propertyprices.train <- property_prices.selected[1:smp_size_decision, ]
propertyprices.test <- property_prices.selected[(smp_size_decision+1): nrow(property_prices.selected),]
decision_tree.validation <- property_prices.selected[(smp_size+1):nrow(property_prices.selected), ]


# specifying target and input vars

formula <- SalePrice ~.


dtree <- rpart(formula, data = propertyprices.train, method = "anova")

dtree$variable.importance


# visualize the decision tree
rpart.plot(dtree, type =, fallen.leaves = FALSE )

print(dtree)

# making predictions and assessments
predicted.SalesPriced1 <- predict(dtree, propertyprices.test)
print("Actual Values" )
head(propertyprices.test$SalePrice[1:5])

print("Predicted Values")
head(predicted.SalesPriced1[1:5])

View(propertyprices.test)

errord1 <- propertyprices.test$SalePrice - predicted.SalePriced1
rmsed1 <- sqrt(mean(error^2))
print(paste("Root Mean Square Error: ", rmsed1))


# Prune the tree with different complexity parameters (cp)
pruned_tree_1 <- prune(dtree, cp = 0.01)
pruned_tree_2 <- prune(dtree, cp = 0.005)

# Visualize the pruned trees
rpart.plot(pruned_tree_1, main = "Pruned Tree (cp = 0.01)")
rpart.plot(pruned_tree_2, main = "Pruned Tree (cp = 0.005)")

# Make predictions with the pruned trees and evaluate them
predicted.SalesPriced2_1 <- predict(pruned_tree_1, propertyprices.test)
predicted.SalesPriced2_2 <- predict(pruned_tree_2, propertyprices.test)

# RMSE for pruned tree 1
errord2_1 <- propertyprices.test$SalePrice - predicted.SalesPriced2_1
rmsed2_1 <- sqrt(mean(errord2_1^2))
print(paste("Root Mean Square Error (Pruned Tree 1): ", rmsed2_1))

# R-squared for pruned tree 1
r_squared_d2_1 <- 1 - sum(errord2_1^2) / sum((propertyprices.test$SalePrice - mean(propertyprices.test$SalePrice))^2)
print(paste("R Squared (Pruned Tree 1): ", r_squared_d2_1))

# RMSE for pruned tree 2
errord2_2 <- propertyprices.test$SalePrice - predicted.SalesPriced2_2
rmsed2_2 <- sqrt(mean(errord2_2^2))
print(paste("Root Mean Square Error (Pruned Tree 2): ", rmsed2_2))

# R-squared for pruned tree 2
r_squared_d2_2 <- 1 - sum(errord2_2^2) / sum((propertyprices.test$SalePrice - mean(propertyprices.test$SalePrice))^2)
print(paste("R Squared (Pruned Tree 2): ", r_squared_d2_2))

# Question 3
# Comparison of Decision Tree Models

# Initial Tree
print(paste("Initial Tree R-squared: ", r_squared_d1))
print(paste("Initial Tree RMSE: ", rmsed1))

# Pruned Tree 1 (cp = 0.01)
print(paste("Pruned Tree 1 R-squared: ", r_squared_d2_1))
print(paste("Pruned Tree 1 RMSE: ", rmsed2_1))

# Pruned Tree 2 (cp = 0.005)
print(paste("Pruned Tree 2 R-squared: ", r_squared_d2_2))
print(paste("Pruned Tree 2 RMSE: ", rmsed2_2))










