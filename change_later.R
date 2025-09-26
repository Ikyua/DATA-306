## Kyle Silvestre
## Exercise 1: Diagram and test hypotheses in R
## DATA 306
## 9/15/2023
### Part 1: Following along

setwd("C:/Workspace/DATA-306")
school <- read.csv("data/NYCschools_r1.csv")

str(school) # gives the object's structure
print("head")
head(school, 10) # the first section of the folder from 0 - 10
print("tail")
tail(school) # the LAST section of the folder

hist(school$mean_class_size)

par(mfrow = c(1,3))
hist(school[["mean_class_size"]])
hist(school[ , "mean_class_size"])
hist(school$mean_class_size)

par(mfrow = c(1,3))
hist(school[["mean_class_size"]])
boxplot(school[ , "mean_class_size"], horizontal= TRUE)
stripchart(school$mean_class_size, method = "jitter")






