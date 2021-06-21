
# title: "Calories Burned During Exercise and Activities"
# author: "igurrola"
# date: "6/20/2021"

# Load packages
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(car))
  install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(magrittr))
  install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(curl))
  install.packages("curl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  
library(tidyverse)
library(caret)
library(data.table)
library(car)
library(magrittr)
library(curl)
library(ggplot2)
library(dplyr)

# Download data set from Kaggle for study
exercise_dataset <- tempfile()
download.file("https://www.kaggle.com/aadhavvignesh/calories-burned-during-exercise-and-activities?select=exercise_dataset.csv", exercise_dataset)
# Read data
exercise_dataset <- read.csv("~/Exercise-Activities/exercise_dataset.csv")
# Original data structure was checked
head(exercise_dataset)
#Creating Calories burned columns for 130Lb, 155Lb, 180Lb and 205Lb 
exercise_dataset2 <- exercise_dataset %>%
  mutate(Cal_burned_130Lb = Calories.per.kg * X130.lb, Cal_burned_155Lb = Calories.per.kg * X155.lb, Cal_burned_180Lb = Calories.per.kg * X180.lb, Cal_burned_205Lb = Calories.per.kg * X205.lb)
# Original data structure including Calories burned columns per category
head(exercise_dataset2)
# Pie Chart of Calories burned cycling activities Category 205Lb
x <- c(1384, 306, 4906, 689, 1227, 1917)
lbl <-  c("Cycling, mountain bike, bmx","Cycling, <10 mph, leisure bicycling","Cycling, >20 mph, racing","Cycling, 10-11.9 mph, light","Cycling, 12-13.9 mph, moderate","Cycling, 14-15.9 mph, vigorous")
pie(x,labels = lbl, main="Pie Chart of Calories burned from cycling. Category 205Lb")
# Pie Chart of Calories burned running activities Category 205Lb
x <- c(1227, 1552, 1917, 2319, 2533, 2994, 3491, 3756, 4312, 4906, 6208)
lbl <-  c("Running, 5 mph (12 minute mile)","Running, 5.2 mph (11.5 minute mile)","Running, 6 mph (10 min mile)","Running, 6.7 mph (9 min mile)","Running, 7 mph (8.5 min mile)","Running, 7.5mph (8 min mile)","Running, 8 mph (7.5 min mile)","Running, 8.6 mph (7 min mile)","Running, 9 mph (6.5 min mile)","Running, 10 mph (6 min mile)","Running, 10.9 mph (5.5 min mile)")
pie(x,labels = lbl, main="Pie Chart of Calories burned from running. Category 205Lb")
# Histogram 130Lb
Cal_burned_130Lb<- exercise_dataset2$Cal_burned_130Lb
hist(Cal_burned_130Lb,
main="Calories burned Category 130Lb",
xlab="From cycling activities in database",
xlim=c(0,5000),
col="blue",
freq=FALSE
)
# Histogram 155Lb
Cal_burned_155Lb<- exercise_dataset2$Cal_burned_155Lb
hist(Cal_burned_155Lb,
main="Calories burned Category 155Lb",
xlab="From cycling activities in database",
xlim=c(0,5000),
col="blue",
freq=FALSE
)
# Histogram 180Lb
Cal_burned_180Lb<- exercise_dataset2$Cal_burned_180Lb
hist(Cal_burned_180Lb,
main="Calories burned Category 180Lb",
xlab="From cycling activities in database",
xlim=c(0,5000),
col="blue",
freq=FALSE
)
# Histogram 205Lb
Cal_burned_205Lb<- exercise_dataset2$Cal_burned_205Lb
hist(Cal_burned_205Lb,
main="Calories burned Category 205Lb",
xlab="From cycling activities in database",
xlim=c(0,5000),
col="blue",
freq=FALSE
)
# Scatter plot category 130Lb
x <- exercise_dataset2$Calories.per.kg
y <- exercise_dataset2$Cal_burned_130Lb
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Calories burned Category 130Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Calories burned Category 130Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")
# Scatter plot category 155Lb
x <- exercise_dataset2$Calories.per.kg
y <- exercise_dataset2$Cal_burned_155Lb
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Calories burned Category 155Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Calories burned Category 155Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")
# Scatter plot category 180Lb
x <- exercise_dataset2$Calories.per.kg
y <- exercise_dataset2$Cal_burned_180Lb
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Calories burned Category 180Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Calories burned Category 180Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")
# Scatter plot category 205Lb
x <- exercise_dataset2$Calories.per.kg
y <- exercise_dataset2$Cal_burned_205Lb
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Calories burned Category 205Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Calories burned Category 205Lb",
     xlab = "Calories per Kilogram", ylab = "Calories according category",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")
# Sorting to find the most intensive exercises of the database
exercise_dataset3 <- exercise_dataset2[order(- exercise_dataset2$Calories.per.kg),]
# Original data structure was checked
head(exercise_dataset3)
# Data was converted to a tidy format
exercise_dataset <- exercise_dataset %>% gather(`X130.lb`, `X155.lb`, `X180.lb`, `X205.lb`, key = "weight", value= "Calories")
# Tidy data structure was checked
head(exercise_dataset)
# Column names were checked
colnames(exercise_dataset)
# Activity column was renamed
exercise_dataset %<>% rename(Activity = Activity..Exercise.or.Sport..1.hour.)
# Two Samples grouping variable was created
exercise_dataset %<>% mutate (Exercise = stringr::str_extract(tolower(Activity), pattern = "(cycling)|(running)"))
# Updated data set was checked
head(exercise_dataset)
# Data was subset to include two variables
exercise_dataset %<>% dplyr::select(Calories, Exercise) %>% filter(Exercise == "running" | Exercise == "cycling")
# Updated data set was checked
head(exercise_dataset)
# Data structure was checked
str(exercise_dataset)
# Exercise was converted to a factor
exercise_dataset$Exercise <- as.factor(exercise_dataset$Exercise)
# Conversion checked
is.factor(exercise_dataset$Exercise)
# Data set summary statistics were gathered
exercise_dataset %>% group_by(Exercise) %>% summarise(Min = min(Calories,na.rm = TRUE),
                                              Q1 = quantile(Calories,probs = .25,na.rm = TRUE),
                                              Median = median(Calories, na.rm = TRUE),
                                              Q3 = quantile(Calories,probs = .75,na.rm = TRUE),
                                              Max = max(Calories,na.rm = TRUE),
                                              Mean = mean(Calories, na.rm = TRUE),
                                              SD = sd(Calories, na.rm = TRUE),
                                              n = n(),
                                              Missing = sum(is.na(Calories)))
# Box plots were created to visualise summary statistics
exercise_dataset %>% boxplot(Calories ~ Exercise, data = ., ylab = "Calories Burnt (per hour)")
# Outliers were removed
exercise_clean <- filter(exercise_dataset, (Exercise== "running" & Calories<1500) | (Exercise=="cycling" & Calories<1400))
# New box plot without outliers was created 
exercise_clean %>% boxplot(Calories ~ Exercise, data = ., ylab = "Calories Burnt (per hour)")
# New summary statistics without outliers were output
exercise_clean %>% group_by(Exercise) %>% summarise(Min = min(Calories,na.rm = TRUE),
                                                    Q1 = quantile(Calories,probs = .25,na.rm = TRUE),
                                                    Median = median(Calories, na.rm = TRUE),
                                                    Q3 = quantile(Calories,probs = .75,na.rm = TRUE),
                                                    Max = max(Calories,na.rm = TRUE),
                                                    Mean = mean(Calories, na.rm = TRUE),
                                                    SD = sd(Calories, na.rm = TRUE),
                                                    n = n(),
                                                    Missing = sum(is.na(Calories)))
# Equality of variances was tested
car::leveneTest(Calories ~ Exercise, data = exercise_clean)
# Two-Sample T Test was performed
t.test(Calories ~ Exercise, data = exercise_clean, var.equal = TRUE, alternative = "two.sided")
# T critical value was calculated
qt(p = 0.025, df = 112)
