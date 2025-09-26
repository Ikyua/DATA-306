# Kyle Silvestre
# Exercise 2: Explore data surfaces in R
# 9/25

install.packages("tidyjson")
install.packages("haven")
install.packages("tidyverse")

library(tidyjson)
library(haven)
library(tidyverse)

setwd("C:/Workspace/DATA-306")
getwd()

school <- read.csv("./data/NYCschools_r1.csv")
str(school)

sch <- subset(school, select = -c(X, year)) # getting rid of year in our dataframe
colnames(sch)

ncol(school) - ncol(sch)

summary(sch[ , -c(1:2)])
# summary(sch[ , 3: 23])

summary(sch[ , 6: 7])
	
# The distribution for black starts with a minimum value of 0.0, while hispanic's minimum value is 0.0360.
# The maximum's vary as well as black and hispanic are 0.9310 and 1.0 respectively. There is a very big spread between the two, which we can see in their quartiles being:
# The 1st quartile for black and hispanic are 0.0640 and 0.2110 respectively while the 3rd quartile is 0.4650 and 0.6740 respectively. 

# Section 5
# a) For variables that are normally demographic variables but are represented as continuous variables in the dataframe. These values are proportion of students identifying/identified in each category for the school.
# b) We can convert these continous variables into categorical (ordinal or nominal) variables using values that categorize them. 
# An example would be like 0.0 - 0.0250 would be low, 0.251 - 0.500 would be mid and so forth. 

# Section 6 

cor.sch <- cor(sch[ , 3:7], use = "complete.obs")
cor.sch

cor.sch <- cor(sch[ , 3:23], use = "complete.obs")
dim(cor.sch)
cor.sch # problem with this df is that it contains duplicates & compares variables to itself 

cor.sch.lt <- cor.sch[lower.tri(cor.sch) == TRUE]
stem(cor.sch.lt)

# The distribution is unimodal (one peak) with a semi-symmetrical distribution. There aren't any outstanding outliers but there are three that stray away being -0.82, -0.73 and -0.75.
# The minimum is 0.81, which means that the values displayed at rounded up using this function. The maximum is 0.57 and the median is -0.22. 
summary(cor.sch.lt)

# Section 7 | A more human-readable method 

# install.packages("corrplot")
library(corrplot)

corrplot(cor.sch, type = "lower", order = "hclust")

# I still don't really understand what the negative values represent, but I am seeing that white and multi-racial along the hispanic relaionship are a dark red indicating negative -1 or -0.8. 
# I'm assuming it means that a lot of the population doesn't strongly fit into that category. 
# Students that are likely to be apart of the ESL class are majority of hispanic students. 

# Section 8

# install.packages("car")
library(car)
scatterplotMatrix(sch[ ,c("outsiders_student", "outsiders_teacher",
				  "outsiders_parent", "total_enrollment")])

cor.sch[c(1, 17, 19, 21), c(1, 17, 19, 21)]

# There does seem to be a positive relationship between outsiderness and total enrollment, particularly with the parents and teachers. 
# A majority of students don't feel like their outsiders but there are still about 0.29 students that feel like they are outsiders in the schools they belong in (total being -0.0712)
# The parents and teacher's values are 0.0453 and 0.010 respectively. If these values were 0, we would be able to say that there is no correlation, but there definitely is a correlation between the total enrollment and outsiderness

# Section 9

hdens <- density(sch$high)
plot(hdens , 
     xlim = c(-.2, 1.23),
     main = "red = elementary, blue = middle, black = high",
     xlab = "")

lines(density(sch$elementary), col="red")
lines(density(sch$middle), col = "blue")

library(DiagrammeR)
grViz("digraph {
		node [shape = plaintext]
		'+' [shape = doublecircle]
		'School size' -> '+' -> 'Outsiders'
		'Student age' -> '+'
		{rank = same; 'School size' 'Outsiders' '+'}
		}")

# Section 10

sch$agemix <- NA
sch$agemix[sch$elementary > 0.5] <- "Elementary"
sch$agemix[sch$middle > 0.7] <- "Middle"
sch$agemix[sch$high > 0.5] <- "High"
sch$agemix[is.na(sch$agemix) ==  TRUE] <- "Mixed"

table(sch$agemix)

sum(sch$total_enrollment[sch$agemix == "High" | 
				 sch$agemix == "Elementary"],
	na.rm = TRUE
	sum(sch$total_enrollment, 
		na.rm = TRUE)

scatterplotMatrix(~ outsiders_parent + outsiders_teacher + 
				outsiders_student + total_enrollment | agemix,
			data = sch, 
			smooth = FALSE,
			legend = list(coords = "bottomright"))

sch$povertymix <- NA
sch$povertymix[sch$asian > 0] <- "Asian"
sch$povertymix[sch$white > 0] <- "White"
sch$povertymix[sch$hispanic > 0] <- "Hispanic"
sch$povertymix[sch$black > 0] <- "Black"


scatterplotMatrix(~ asian + white + hispanic +
				black + poverty | povertymix,
			data = sch, 
			smooth = FALSE,
			legend = list(coords = "bottomright"))

# I created my own variables to work with called "povertymix" where I looked into the races of students and looked at what races are below the poverty line.
# Students who have identified or been identified as below the poverty line seem to be students who are black or hispanic. With this data we can make all types of assumptions and solutions but this is a start for schools to accomodate for these discrepensies in society and help these students get the help they need.
# Correct me if I did this wrong, but I think I did it correctly :D