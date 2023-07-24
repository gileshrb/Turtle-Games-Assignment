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

# Install and import Tidyverse.
library(tidyverse) 

# Import the data set.
sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
sales

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales2)

# View the descriptive statistics.
summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Create a scatterplot to show the relationship between 
# North American sales and Global sales
qplot(NA_Sales, Global_Sales, data=sales2)

# Create a scatterplot to show the relationship between 
# European sales and Global sales
qplot(EU_Sales, Global_Sales, data=sales2)

# Create a scatterplot to show the relationship between 
# North American sales and European sales
qplot(NA_Sales, EU_Sales, data=sales2)



## 2b) Histograms
# Create histograms.


# Create a histogram to show the distribution of Global sales
qplot(Global_Sales, data=sales2, binwidth = 1)

# Create a histogram to show the distribution of North American sales
qplot(NA_Sales, data=sales2, binwidth = 1)

# Create a histogram to show the distribution of European sales
qplot(EU_Sales, data=sales2, binwidth = 1)



## 2c) Boxplots
# Create boxplots.

# Create a boxplot to show the difference in Global sales by platform
qplot(Platform, Global_Sales, data=sales2, geom="boxplot")

# Create a boxplot to show the difference in North American sales by platform
qplot(Platform, NA_Sales, data=sales2, geom="boxplot")

# Create a boxplot to show the difference in European sales by platform
qplot(Platform, EU_Sales, data=sales2, geom="boxplot")


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

#The scatterplots reveal a strong positive correlation between sales in 
## North America and Europe and Global sales. This indicates that games that 
## sell well in these regions tend to also have high global sales. 
## This relationship could be influenced by various factors, as evidenced 
## by scattered points on the plots. An interesting observation is the 
## relationship between North American and European sales, which could 
## reflect regional differences in game preferences.

#The histograms of Global, North American, and European sales exhibit 
##right-skewed distributions. This indicates that most games have relatively 
##low to moderate sales, while a small number of games achieve exceptionally 
##high sales. This pattern is consistent across the global and regional markets.

#The boxplots reveal variations in sales across different gaming platforms 
## globally, in North America, and Europe. Some platforms consistently show 
## strong sales performance, while others have a wide range of sales, 
## indicating a mix of both hits and misses. Outliers on certain platforms 
## might point to particularly successful or unsuccessful games that deviate 
## from the typical sales range. These insights can help identify platforms 
## that are more likely to yield successful games or platforms that offer 
## opportunities for improvement.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

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
head(sales2)
str(sales2)

# Check output: Determine the min, max, and mean values.
# Create a new data frame to isolate the regional sales
regional_sales <- sales2[,c("EU_Sales", "NA_Sales", "Global_Sales")]

# Use the sapply function to calculate min, median and max.
regional_stats <- sapply(regional_sales, summary)

# Display the min, median and max sales for each region.
regional_stats[c("Min.", "Median", "Max."), ]


# View the descriptive statistics.
summary(sales2)



###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_grouped <- sales2 %>% select(-Platform) %>%
  group_by(Product) %>% summarise_all(sum) %>% as.data.frame()



# View the data frame.
head(sales_grouped)


# Explore the data frame.
str(sales_grouped)
summary(sales_grouped)



## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

# Scatterplot to compare North American vs European Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(x=NA_Sales, y=EU_Sales)) + 
  # Add colour
  geom_point(color = 'red') + 
  # Add the labs function for labels
  labs(x = "NA Sales (£M)", 
  y = "EU Sales (£M)", 
  title = "European Sales vs North American Sales") + 
  # Add a 'minimal' theme
  theme_minimal()

# Scatterplot for Global_Sales vs NA_Sales
ggplot(sales_grouped, aes(x=NA_Sales, y=Global_Sales)) + 
  # Add colour
  geom_point(color = 'red') + 
  # Add the labs function for labels
  labs(x = "NA Sales (£M)", 
       y = "Global Sales (£M)", 
       title = "Global Sales vs North American Sales") + 
  # Add a 'minimal' theme
  theme_minimal()

# Scatterplot for Global_Sales vs EU_Sales
ggplot(sales_grouped, aes(x=EU_Sales, y=Global_Sales)) + 
  # Add colour
  geom_point(color = 'red') + 
  # Add the labs function for labels
  labs(x = "EU Sales (£M)", 
       y = "Global Sales (£M)", 
       title = "Global Sales vs European Sales") + 
  # Add a 'minimal' theme
  theme_minimal()


# Create histograms.

# Histogram for NA Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(x=NA_Sales)) + 
  # Add fill, colour and specify bin size
  geom_histogram(fill = 'red', color = 'black', bins = 13) + 
  # Specify labels
  labs(x = "NA Sales (£M)", 
       y = "Frequency", 
       title = "Frequency Distribution of games by North American Sales") + 
  # Add a 'minimal' theme
  theme_minimal()


# Histogram for EU Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(x=EU_Sales)) + 
  # Add fill, colour and specify bin size
  geom_histogram(fill = 'red', color = 'black', bins = 13) + 
  # Specify labels
  labs(x = "EU Sales (£M)", 
       y = "Frequency", 
       title = "Frequency Distribution of games by European Sales") + 
  # Add a 'minimal' theme
  theme_minimal()


# Histogram for Global Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(x=Global_Sales)) + 
  # Add fill, colour and specify bin size
  geom_histogram(fill = 'red', color = 'black', bins = 13) + 
  # Specify labels
  labs(x = "EU Sales (£M)", 
       y = "Frequency", 
       title = "Frequency Distribution of games by Global Sales") + 
  # Add a 'minimal' theme
  theme_minimal()


# Create a boxplot

# Reshape the data to a long format to allow for a boxplot comparing
#  regional sales
sales_grouped_long <- gather(sales_grouped, key = "Region", 
  value = "Sales", NA_Sales, EU_Sales, Global_Sales)

# Create a boxplot with a separate box for each region
# Specify the ggplot function
ggplot(sales_grouped_long, aes(x = Region, y = Sales)) + 
  # Add fill and colour
  geom_boxplot(fill = 'red', notch = TRUE, outlier.color = 'red') + 
  # Add the labs function for labels.
  labs(y = "Sales (£M)", 
       title = "Boxplot of Sales by Region") + 
  # Add a 'minimal' theme
  theme_minimal()


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.


# Create Q-Q plot for North American Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(sample = NA_Sales)) +
  stat_qq(distribution = qnorm) +
  # Add a reference line
  stat_qq_line(distribution = qnorm) +
  # Add the labs function for labels.
  labs(title = "Q-Q plot for North American Sales") +
  # Add a 'minimal' theme
  theme_minimal()


# Create Q-Q plot for European Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(sample = EU_Sales)) +
  stat_qq(distribution = qnorm) +
  # Add a reference line
  stat_qq_line(distribution = qnorm) +
  # Add the labs function for labels.
  labs(title = "Q-Q plot for European Sales") +
  # Add a 'minimal' theme
  theme_minimal()


# Q-Q plot for Global Sales
# Specify the ggplot function
ggplot(sales_grouped, aes(sample = Global_Sales)) +
  stat_qq(distribution = qnorm) +
  # Add a reference line
  stat_qq_line(distribution = qnorm) +
  # Add the labs function for labels.
  labs(title = "Q-Q plot for Global_Sales") +
  # Add a 'minimal' theme
  theme_minimal()


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)


# Perform Shapiro-Wilk test.

# Shapiro-Wilk test for Global Sales
shapiro.test(sales_grouped$Global_Sales)

#Shapiro-Wilk test for European Sales
shapiro.test(sales_grouped$EU_Sales)

#Shapiro-Wilk test for North American
shapiro.test(sales_grouped$NA_Sales)

# Our p-value is <0.05 for all three columns,
#  so the data is not normally distributed.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Global Sales
# Determine Skewness
skewness(sales_grouped$Global_Sales)
# Determine Kurtosis
kurtosis(sales_grouped$Global_Sales)

# European Sales
# Determine Skewness
skewness(sales_grouped$EU_Sales)
# Determine Kurtosis
kurtosis(sales_grouped$EU_Sales)

# North American Sales
# Determine Skewness
skewness(sales_grouped$NA_Sales)
# Determine Kurtosis
kurtosis(sales_grouped$NA_Sales)



## 3d) Determine correlation
# Determine correlation.

# Determine correlation for North American and European sales
cor(sales_grouped$NA_Sales, sales_grouped$EU_Sales)

# Determine correlation for North American and Global
cor(sales_grouped$NA_Sales, sales_grouped$Global_Sales)

# Determine correlation for Global and European sales
cor(sales_grouped$Global_Sales, sales_grouped$EU_Sales)


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Plot 1: Pairplot

# Import GGally
library(GGally)


# Create a new grouped sales dataframe without the Product column
sales_grouped2 <- sales_grouped[, c('NA_Sales', 'EU_Sales', 'Global_Sales')]



# Define a function to create scatterplots when called upon
scatter_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(size = 1) +  
    geom_smooth(method = "lm", color = "blue", ...)  
  p
}

# Define a function to create histograms when called upon
hist_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_histogram(binwidth = 1, fill = "blue", ...)  
  p
}


# Create the pair plot using the scatterplot and histogram functions
ggpairs(
  sales_grouped2,
  lower = list(continuous = wrap(scatter_fn, size = 1)),  
  diag = list(continuous = wrap(hist_fn, fill = "white")),
  upper = list(continuous = wrap(scatter_fn, size = 1))
)


# Plot 2: Violin + boxplot

# Specify the ggplot function
ggplot(sales_grouped_long, aes(x = Region, y = Sales)) + 
  # Add the geom_violin function and fill
  geom_violin(fill = 'red') +  
  # Add the geom_boxplot function and customize the fill and outliers
  geom_boxplot(fill = 'green', width = 0.1,
               outlier.color = 'green', outlier.shape = 15, outlier.size = 1) + 
  # Add the labs function for labels
  labs(y = "Sales (£M)", 
       title = "Violin Plot and Boxplot of Sales by Region") + 
  # Add a 'minimal' theme
  theme_minimal()


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Turtle Games offers 175 different products, with each product averaging 
## £10.73M in global sales. The top seller is product 107, with sales of 
## £67.85M, accounting for 3.6% of global sales. This shows that Turtle Game’s 
## income is extremely diversified with no individual product/game having an 
## overwhelming contribution to the total sales.

# The sales data reliability is questionable as it doesn't follow a normal 
## distribution. Significant right-skewness and high kurtosis values suggest 
## the presence of outliers or extreme high values, potentially impacting the 
## statistical analyses that presume normality (simple linear and multilinear 
## regression. The Q-Q plots, Shapiro-Wilk tests, and skewness and kurtosis 
## measures consistently confirm these findings.


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

# 1. Load and explor the data
# View data frame created in Week 5.
head(sales_grouped2)
str(sales_grouped2)

# Determine a summary of the data frame.
summary(sales_grouped2)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

model1 <- lm(NA_Sales~EU_Sales,
             data=sales_grouped2)

model2 <- lm(Global_Sales~NA_Sales,
             data=sales_grouped2)

model3 <- lm(Global_Sales~EU_Sales,
             data=sales_grouped2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.


# Create the plot to visualize the simple linear regression model for
# North American and European sales
plot(sales_grouped2$EU_Sales, sales_grouped2$NA_Sales,
     main = "Linear Regression Plot (North American Sales and European Sales)",
     xlab = "European Sales", ylab = "North American Sales")
# Add the line-of-best-fit
abline(model1, col = "red")


# Create the plot to visualize the simple linear regression model for
# Global sales and North American
plot(sales_grouped2$NA_Sales, sales_grouped2$Global_Sales,
     main = "Linear Regression Plot (Global Sales and North American Sales)",
     xlab = "North American Sales", ylab = "Global Sales")
# Add the line-of-best-fit
abline(model2, col = "red")


# Create the plot to visualize the simple linear regression model for
# Global sales and European
plot(sales_grouped2$EU_Sales, sales_grouped2$Global_Sales,
     main = "Linear Regression Plot (Global Sales and European Sales)",
     xlab = "European Sales", ylab = "Global Sales")
# Add the line-of-best-fit
abline(model3, col = "red")

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Create a new object and 
# specify the lm function and the variables.
modela = lm(Global_Sales~NA_Sales+EU_Sales, data=sales_grouped2)


# Multiple linear regression model.
# Change the model name.
summary(modela)



###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create a new data frame with new North American and European sales data
new_data = data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08), 
                      EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Use the predict() function to predict Global_Sales
predicted_sales = predict(modela, newdata = new_data,
                          interval='confidence')

# Display the predicted Global_Sales
print(predicted_sales)

# MLR Results:
## NA Sales,	EU Sales,	Global Sales(Predicted),	Global Sales(Observed)
#1)34.02,	     23.80,	    68.06,	                   67.85,
#2)3.93,	     1.56,      7.36,                      6.04,
#3)2.73,       0.65,	    4.91,	                     4.32,
#4)2.26,	     0.97,	    4.76,	                     3.53,
#5)22.08,	     0.52,	    26.63,	                   23.21,


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# There is a strong positive correlation between North American, European, 
## and Global game sales. Using simple linear regression we can assume that 
## an increase in sales by 1 million pounds in Europe and North America 
## corresponds to respective increases in Global sales by roughly 2.237 and 
## 1.635 million pounds. Furthermore, the multilinear regression model 
## suggests that about 96.68% of Global sales variability is explained by 
## North American and European sales. Both these markets are significant 
## predictors of Global sales. 



# Patterns / predictions:
# In the multilinear regression analysis of Turtle Games' sales data, North 
## American and European sales emerged as statistically significant predictors
## of Global sales. The regression coefficients for North American and European
## sales were 1.13040 and 1.19992 respectively. This implies that for every one 
## million pound increase in North American sales, we'd expect an increase of 
## about 1.13 million pounds in Global sales, assuming European sales are 
## constant. Similarly, for each million pound increase in European sales, 
## we'd anticipate an increase of 1.19 million pounds in Global sales, 
## assuming North American sales are constant.

#This MLR model was used to predict global sales for several scenarios, 
## The predictions align closely with the observed values, 
## demonstrating the model's accuracy. 


# The model's R-squared value of 0.9668 indicates that it explains about 96.68%
## of the variability in global sales, demonstrating its effectiveness. 
## Stakeholders may use this model and predictions to inform marketing and go to 
## market strategies, financial plans, inventory management, and sales 
## target setting.

# Further analysis should be undertaken to fix the skewnessand normality issues
## with the sales dataset before Turtle Games actions any changes 
## using these insights.










###############################################################################
###############################################################################




