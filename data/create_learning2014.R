#Adam Saada
#2.11.2018
#Excercise 2

JYTOPKYS3_data <- read_delim("~/IODS-project/data/JYTOPKYS3-data.txt", 
                               +     "\t", escape_double = FALSE, trim_ws = TRUE)

View(JYTOPKYS3_data)

lrn14 <- JYTOPKYS3_data
#Numeric data with explanatory variable as M or F

#Create an analysis dataset with the variables gender, age, 
#attitude, deep, stra, surf and points by combining questions in the learning2014 data,
#Scale all combination variables to the original scales (by taking the mean). 
#Exclude observations where the exam points variable is zero. (The data should then have 166 observations and 7 variables)
lrn14$attitude <- lrn14$Attitude / 10

library(dplyr)

keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

# see the structure of the new dataset
str(learning2014)

View(learning2014)
learning2014 <- filter(learning2014, Points > 0)

write.csv(learning2014,file = "learning2014.csv")

learning2014 <- read_delim("~/IODS-project/learning2014.csv", 
                           " ", escape_double = FALSE, trim_ws = TRUE)

###
week2 <- read_csv("~/IODS-project/week2.txt")
summary(week2)
#Mean age is 25.51 so rather young. Deep, stra and surf have normal distributions with 2.8-3.2 median.

lm <- lm(points~age+attitude+deep, data = week2)
summary(lm)
par(mfrow=c(2,2))
plot(lm)
#There could be a non-linear relationship between predictor variables
#and an outcome variable and the pattern could show up in this plot if the model doesn’t capture the non-linear relationship. No non-linearity
