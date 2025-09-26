install.packages("tidyjson")
install.packages("haven")
install.packages("tidyverse")

library(tidyjson)
library(haven)
library(tidyverse)


## Kyle Silvestre
## Exercise 1: Diagram and test hypotheses in R
## DATA 306
## 9/15/2023
### Part 1: Following along

getwd() 
setwd("C:/Workspace/DATA-306")
getwd()

school <- read.csv("data/NYCschools_r1.csv")
str(school)
head(school)
head(school, 10)

hist(school$mean_class_size)

par(mfrow = c(1,3))
hist(school[["mean_class_size"]])
hist(school[ , "mean_class_size"])
hist(school$mean_class_size)

# 6
par(mfrow = c(1,3))
hist(school[["mean_class_size"]])
boxplot(school[ , "mean_class_size"], horizontal = TRUE)
stripchart(school$mean_class_size, method = "jitter")
# Intepretation: I notice that the mean/average apearrs to be about 25. 
# We can see there are outliers that are in the 50 range, but I believe the graph is skewed to the left? 
# I believe we should intepret the "strip-chart" as a dot-plot chart. 

# 7
summary(school)
# With this it provides a lot more information about all the variables we're measuring which is definitely helpful. It provides us a lot of useful information like the values for the IQR, mean median, etc.

# 8

summary(school$total_enrollment)
# The center of the data is 506.0 which is the median. The data vary around the center very much, s the mean is 603.9. The min and max of the variable are 74.0 and 2135.0 respectively. 

# 9) 
# In NYC, on average there is about 603 students that are enrolled into schools across NYC. With these large amount of students comes a lot of support from the staff and government ensuring your child's educational access.

#install.packages("DiagrammeR")
library(DiagrammeR)
grViz("digraph {’School size’ -> ’Class size’}")

grViz("digraph {'School size' -> 'Class size' [label = '+']}")

grViz("digraph {
	rankdir = 'LR'
	'School size' -> 'Class size' [label = ' -']
	}")

# 11

plot(school$total_enrollment, school$mean_class_size)

# Regardless of the number of total enrollments in a school, an average classroom size is from 20-30 students. Rarely will you see a class with larger than that.

#12
school_lm <- lm(mean_class_size ~ total_enrollment, data = school)
summary(school_lm)

#13
plot(school$mean_class_size ~ school$total_enrollment)
abline(school_lm, col = "blue", lwd = 2)

# The model indicates a positive relationship between total enrollment and mean class size by the line, it's going upwards as the graph moves

scatter.smooth(school$mean_class_size ~ school$total_enrollment,
	span = 2/3, degree = 2, 
	lpars = list(col = "red", lwd = 2))
abline(school_lm, col = "blue", lwd = 2)


## Part 2: An analysis of student belonging and poverty
# 6
par(mfrow = c(1,3))
hist(school[["outsiders_student"]])
boxplot(school[ , "mean_class_size"], horizontal = TRUE)
stripchart(school$outsiders_student, method = "jitter")

# The average of outsiders_students is about 0.10 = 0.15. I'm not exactly sure what it means by decimal values to indicate if someone feels like they're ostracized or don't feel respected in school to be honest. 
# We should intepret the strip chart as a dot-plot graph, and look at the y and x axis to indicate which values are most often. I'm assuming the darker dots are more frequent values.

# 8
summary(school$outsiders_student)
# a) The center of the data is 0.150 the median. 
# b) The mean is 0.155, the min and max is 0.0 and 0.330 respectively. While interestingly there are 4 NA's. 

#install.packages("DiagrammeR")
library(DiagrammeR)
grViz("digraph {'Poverty' -> 'Feelings of being an outsider' [label = '+']}")

school_lm_2 <- lm(outsiders_student ~ poverty, data = school)
summary(school_lm_2)

plot(school$outsiders_student ~ school$poverty)
abline(school_lm_2, col = "blue", lwd = 2)

# The model does confirm that there is a positive correlation between outsiders_students and poverty level.

scatter.smooth(school$outsiders_student~ school$poverty,
	span = 2/3, degree = 2,
	lpars = list(col = "red", lwd = 2))

abline(school_lm_2, col = "blue", lwd = 2)


