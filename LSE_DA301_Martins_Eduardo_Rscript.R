## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################


# 1. Load and explore the data

# Import Tidyverse.
library(tidyverse)

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv',
                         header = TRUE)

# Print the data frame.
View(turtle_sales)

# Explore the dataset.
str(turtle_sales)
typeof(turtle_sales)
class(turtle_sales)
dim(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales_cleaned <- subset(turtle_sales, 
                               select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
View(turtle_sales_cleaned)

# View the descriptive statistics.
summary(turtle_sales_cleaned)

# Explore the dataset.
str(turtle_sales_cleaned)
typeof(turtle_sales_cleaned)
class(turtle_sales_cleaned)
dim(turtle_sales_cleaned)
as_tibble(turtle_sales_cleaned)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, EU_Sales,
      data = turtle_sales_cleaned,
      geom=c('point','smooth'))
qplot(NA_Sales, Global_Sales,
      data = turtle_sales_cleaned,
      geom=c('point','smooth'))
qplot(EU_Sales, Global_Sales,
      data = turtle_sales_cleaned,
      geom=c('point','smooth'))

## 2b) Histograms
# Create histograms.
plot(hist(turtle_sales_cleaned$NA_Sales))
plot(hist(turtle_sales_cleaned$EU_Sales))
plot(hist(turtle_sales_cleaned$Global_Sales))

## 2c) Boxplots
# Create boxplots.
qplot(Platform,NA_Sales,
      data = turtle_sales_cleaned,
      colour=I('red'),
      geom = 'boxplot')
qplot(Platform,EU_Sales,
      data = turtle_sales_cleaned,
      colour=I('red'),
      geom = 'boxplot')
qplot(Platform,Global_Sales,
      data = turtle_sales_cleaned,
      colour=I('red'),
      geom = 'boxplot')

###############################################################################

# 3. Observations and insights

# There is a positive linear relationship between EU Sales and NA Sales.
# Similarly there is a positive linear relationship between Global Sales and 
# EU/NA sales. This could be due to NA/EU making up most of the Global Sales.
# Histogram for the regions are all skewed to the right with most of the 
# density between £0-10 million bin for NA and global and £0-5 million for EU
# Sales.
# The box plot for all regions showed that the Wii had an extreme outlier 
# compared to the rest of the data. This will need to be investigated.
# stackoverflow.com/questions/how-to-add-mean-and-mode-to-ggplot-histogram
# https://www.geeksforgeeks.org/how-to-create-a-grouped-barplot-in-r/
###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(turtle_sales_cleaned)

# View the data frame.
head(turtle_sales_cleaned)
dim(turtle_sales_cleaned)

# Determine if missing values exist in the data frame.
sum(is.na(turtle_sales_cleaned))

# Check output: Determine the min, max, and mean values.
# GLOBAL SALES
min(turtle_sales_cleaned$Global_Sales)
max(turtle_sales_cleaned$Global_Sales)
mean(turtle_sales_cleaned$Global_Sales)

# EU SALES
min(turtle_sales_cleaned$EU_Sales)
max(turtle_sales_cleaned$EU_Sales)
mean(turtle_sales_cleaned$EU_Sales)

# NA SALES
min(turtle_sales_cleaned$NA_Sales)
max(turtle_sales_cleaned$NA_Sales)
mean(turtle_sales_cleaned$NA_Sales)

# View the descriptive statistics.
summary(turtle_sales_cleaned)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_product <- turtle_sales_cleaned %>%
  group_by(Product) %>%
  summarise(Global_Sales = sum(Global_Sales),
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales))

# View the data frame.
head(sales_product)
dim(sales_product)

# Explore the data frame.
str(sales_product)
typeof(sales_product)
class(sales_product)
as_tibble(sales_product)

# View the descriptive statistics
summary(sales_product)

## 2b) Determine which plot is the best to compare game sales.
# Create scatter plots.

# NA vs EU Sales per product.
ggplot(data = sales_product,
       aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  labs(title = "Relationship between North America & European Union Sales",
       x = "NA Sales (million/£)",
       y = "EU Sales (million/£)") +
  theme_light()

# NA vs Global Sales per product.
ggplot(data = sales_product,
       aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  labs(title = "Relationship between North America & Global Sales",
       x = "NA Sales (million/£)",
       y = "Global Sales (million/£)") +
  theme_light()

# EU vs Global sales per product.
ggplot(data = sales_product,
       aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  labs(title = "Relationship between European Union & Global Sales",
       x = "EU Sales (million/£)",
       y = "Global Sales (million/£)") +
  theme_light()

# Create histograms.

# Global Sales per product.
ggplot(sales_product,
       aes(x = Global_Sales)) +
  geom_histogram(color = 'darkblue',
                 fill = 'lightblue',
                 alpha = 1,
                 binwidth = 10)+
  labs(title = "Global Sales", 
       x = "Sales (million/£)", 
       y = "Count") +
  geom_vline(aes(xintercept = mean(Global_Sales)),
             color = "red",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  theme_light()

# NA Sales per product.
ggplot(sales_product,
       aes(x = NA_Sales)) +
  geom_histogram(color = 'darkblue',
                 fill = 'lightblue',
                 alpha = 1,
                 binwidth = 5)+
  labs(title = "North American Sales", 
       x = "Sales (million/£)", 
       y = "Count") +
  geom_vline(aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 120, 20)) +
  theme_light()

# EU Sales per product.
ggplot(sales_product,
       aes(x = EU_Sales)) +
  geom_histogram(color = 'darkblue',
                 fill = 'lightblue',
                 alpha = 1,
                 binwidth = 5)+
  labs(title = "European Union Sales", 
       x = "Sales (million/£)", 
       y = "Count") +
  geom_vline(aes(xintercept = mean(EU_Sales)),
             color = "red",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_light()

# Create boxplots.

# Global Sales per product
ggplot(data = sales_product, 
       mapping = aes(x=Global_Sales)) +
  geom_boxplot(color = 'darkblue',
               fill = 'lightblue',
               notch = TRUE,
               outlier.color = 'red') +
  labs(title = "Distribution of Global Sales",
       x = "Sales (million/£)",
       y = "Products") +
  scale_x_continuous(breaks = seq(0, 70, 2)) +
  theme_light()

# NA sales per product
ggplot(data = sales_product, 
       mapping = aes(x=NA_Sales)) +
  geom_boxplot(color = 'darkblue',
               fill = 'lightblue',
               notch = TRUE,
               outlier.color = 'red') +
  labs(title = "Distribution of NA Sales",
       x = "Sales (million/£)",
       y = "Products") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  theme_light()

# EU sales per product
ggplot(data = sales_product, 
       mapping = aes(x=EU_Sales)) +
  geom_boxplot(color = 'darkblue',
               fill = 'lightblue',
               notch = TRUE,
               outlier.color = 'red') +
  labs(title = "Distribution of EU Sales",
       x = "Sales (million/£)",
       y = "Products") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  theme_light()

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Global Sales.
qqnorm(turtle_sales_cleaned$Global_Sales)
qqline(turtle_sales_cleaned$Global_Sales)

# NA Sales.
qqnorm(turtle_sales_cleaned$NA_Sales)
qqline(turtle_sales_cleaned$NA_Sales)

# EU Sales.
qqnorm(turtle_sales_cleaned$EU_Sales)
qqline(turtle_sales_cleaned$EU_Sales)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_product$Global_Sales)
shapiro.test(sales_product$NA_Sales)
shapiro.test(sales_product$EU_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Global Sales
skewness(sales_product$Global_Sales) 
kurtosis(sales_product$Global_Sales)

# NA Sales
skewness(sales_product$NA_Sales) 
kurtosis(sales_product$NA_Sales)

# EU Sales
skewness(sales_product$EU_Sales) 
kurtosis(sales_product$EU_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales_product)

# Determine correlation without 'Product'
# NA vs EU 
cor(sales_product$NA_Sales,
    sales_product$EU_Sales)

# NA vs Global
cor(sales_product$NA_Sales,
    sales_product$Global_Sales)

# EU vs Global
cor(sales_product$EU_Sales,
    sales_product$Global_Sales)


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Scatterplot with trend line.

# NA vs EU Sales per product.
ggplot(data = sales_product,
       aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  geom_smooth(method = 'lm',
              col='blue',
              se=FALSE) +
  labs(title = "Relationship between North America & European Union Sales",
       x = "NA Sales (million/£)",
       y = "EU Sales (million/£)") +
  theme_light()

# NA vs Global Sales per product.
ggplot(data = sales_product,
       aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  geom_smooth(method = 'lm',
              col='blue',
              se=FALSE) +
  labs(title = "Relationship between North America & Global Sales",
       x = "NA Sales (million/£)",
       y = "Global Sales (million/£)") +
  theme_light()

# EU vs Global sales per product.
ggplot(data = sales_product,
       aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=1,
             size=1.5) +
  geom_smooth(method = 'lm',
              col='blue',
              se=FALSE) +
  labs(title = "Relationship between European Union & Global Sales",
       x = "EU Sales (million/£)",
       y = "Global Sales (million/£)") +
  theme_light()

## Further exploration for sales by platform

# Sales by platform data frame to investigate more sales plots. 
sales_platform <- turtle_sales_cleaned %>%
  group_by(Platform) %>% 
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# View the new data frame.
sales_platform

# Install necessary libraries.
install.packages("MASS")
install.packages('reshape')
install.packages('reshape2')

# Import the libraries to melt data.
library(MASS)
library(reshape)
library(reshape2)

# Create a new data frame with melted data
melt_data <- melt(sales_platform,id = 'Platform')
melt_data

# Create a grouped bar plot
ggplot(melt_data,
       aes(x = reorder(Platform, -value),
           y = value,
           fill = reorder(variable, -value))) + 
  geom_bar(stat="identity",position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(title = "Sales per Region",
       x = "Platform",
       y = "Sales (million/£)",
       fill = 'Region') 
  
# Create a stacked bar plot  
ggplot(melt_data,
       aes(x = reorder(Platform, -value),
           y = value,
           fill = reorder(variable, -value))) + 
  geom_bar(stat="identity",position = 'dodge')+
  geom_col(position = position_stack(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(title = "Sales per Region",
       x = "Platform",
       y = "Sales (million/£)",
       fill = 'Region')

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# All regions had a positive correlation with each other. In particular NA and 
# Global had a very strong correlation of 0.916, similarly, Global and EU has a
# strong positive correlation of 0.848 and EU and NA had a positive correlation 
# of 0.620.
# The 5 best global selling platforms are the Wii,X360,PS3,DS and GB with the
# Wii having the most sales with £312.56 million. The 5 worst global selling
# platforms are the PSP,WiiU,2600,Gen and PSV with the PSV being the lowest 
# selling platform with £3.34 million.
# The 3 most popular platforms in NA are in order of X360, Wii and PS3.
# The 3 most popular platforms in EU are in order of Wii, PS3 and X360.
# The 3 least popular platforms in NA are PSP,Gen and PSV. 
# The 3 least popular platforms in EU are PSV,Gen and 2600.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(sales_product)
dim(sales_product)

# Determine a summary of the data frame.
summary(sales_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(sales_product)

# Create a linear regression model on the original data.
# Relationship between NA and EU Sales
plot(sales_product$EU_Sales,sales_product$NA_Sales)

# Create the linear regression model
modelEU_NA <- lm(EU_Sales ~ NA_Sales,
             data = sales_product)

# View the model.
modelEU_NA

# Regression table.
summary(modelEU_NA)

# Plot residuals(estimates of error terms (should have no pattern)).
plot(modelEU_NA$residuals)

# Determine the co-efficient.
coefficients(modelEU_NA)

# Add line of best fit.
abline(coefficients(modelEU_NA))

## 2b) Create a plot (simple linear regression)
plot(sales_product$NA_Sales, sales_product$EU_Sales)
coefficients(modelEU_NA)

# Add line of best fit.
abline(coefficients(modelEU_NA))

## 2a) Create a linear regression model
# Relationship between Global and EU Sales
plot(sales_product$EU_Sales, sales_product$Global_Sales)

# Create the linear regression model.
modelGL_EU <- lm(Global_Sales ~ EU_Sales,
             data=sales_product)

# View the model.
modelGL_EU

# Regression table.
summary(modelGL_EU)

# Plot residuals(estimates of error terms (should have no pattern)).
plot(modelGL_EU$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualization.
plot(sales_product$EU_Sales, sales_product$Global_Sales)
coefficients(modelGL_EU)

# Add line of best fit.
abline(coefficients(modelGL_EU))

## 2a) Create a linear regression model
# Relationship between Global and NA Sales
plot(sales_product$NA_Sales, sales_product$Global_Sales)

# Create the linear regression model.
modelGL_NA <- lm(Global_Sales ~ NA_Sales,
             data=sales_product)

# View the model.
modelGL_NA

# Regression table.
summary(modelGL_NA)

# Plot residuals(estimates of error terms (should have no pattern)).
plot(modelGL_NA$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_product$NA_Sales, sales_product$Global_Sales)
coefficients(modelGL_NA)

# Add line of best fit.
abline(coefficients(modelGL_NA))


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# View original data frame
head(turtle_sales)
str(turtle_sales)

# Select only numeric columns
sales_numeric <- subset(turtle_sales,
                        select = c(Product,NA_Sales,EU_Sales,Global_Sales))

# Explore the data set.
summary(sales_numeric)
dim(sales_numeric)

# Determine correlation between variables.
cor(sales_numeric)

# Multiple linear regression model.
model_mlr <- lm(Global_Sales ~ NA_Sales + EU_Sales,
                data = sales_numeric)

# Print the summary statistics.
summary(model_mlr)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Observed Values.
NA_Sales <- c(34.02,3.93,2.73,2.26,22.08)
EU_Sales <- c(23.80,1.56,0.65,0.97,0.52)

# Create a new data frame.
sales_observed <- data.frame(cbind(NA_Sales,EU_Sales))
str(sales_observed)

# Create a new object and specify the predict function.
predictTest = predict(model_mlr, 
                      newdata = sales_observed, 
                      interval='confidence')

# Print the Global_Sales object.
predictTest

# Sense check which Global values corresponding to the NA and EU values provided

# Corresponding Global_Sales value for NA_Sales provided
sales_numeric[sales_numeric$NA_Sales == 34.02,] 
sales_numeric[sales_numeric$NA_Sales == 3.93,]
sales_numeric[sales_numeric$NA_Sales == 2.73,]
sales_numeric[sales_numeric$NA_Sales == 2.26,]
sales_numeric[sales_numeric$NA_Sales == 22.08,]

# Corresponding Global_Sales value for EU_Sales provided
sales_numeric[sales_numeric$EU_Sales == 23.80,] 
sales_numeric[sales_numeric$EU_Sales == 1.56,]
sales_numeric[sales_numeric$EU_Sales == .65,]
sales_numeric[sales_numeric$EU_Sales == .97,]
sales_numeric[sales_numeric$EU_Sales == .52,]

# Observed and predicted Global Sales
Global_Sales_observed <- c(67.85,6.04,4.32,3.53,23.21)
Global_Sales_predicted <- c(71.47,6.86,4.25,4.14,26.43)

# Data Frame to compare predicted and observed values
obs_vs_pred <- data.frame(cbind(NA_Sales,EU_Sales,
                                Global_Sales_observed,Global_Sales_predicted))

# View the data frame
obs_vs_pred
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# Linear regression model:
# EU ~ NA R squared:0.3856 = 38.56% variation in one region to the other
# GL ~ EU R squared:0.7201 = 72.01% variation in one region to the other
# GL ~ NA R squared:0.8395 = 83.95% variation in one region to the other
# Multiple Linear regression model:
# GL - NA + EU R squared:0.9687 = 96.87% variation in global sales explained by
# NA and EU_Sales
# Adjusted R-square is also 96.85% which means this model can be relied on.
#   Global_Sales_Observed      Global_Sales_predicted
#             67.85                   71.47
#              6.04                    6.86
#              4.32                    4.25
#              3.53                    4.14
#             23.21                   26.43
# The predicted values are quite close to the observed values.
# There should be confidence in using this model.
###############################################################################
###############################################################################




