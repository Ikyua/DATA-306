setwd("C:/Workspace/DATA-306")
getwd()

stn <- read.csv("./data/MTA_Subway_Stations_20240916.csv")
str(stn)

borotab <- table(stn$Borough)
borotab

sort(borotab) 
sort(borotab, decreasing=TRUE)

addmargins(sort(borotab))

p.borotab <- proportions(sort(borotab))

round(p.borotab, 2) * 100
round(p.borotab * 100, 2)

stn$ADA_full <- stn$ADA.Northbound + stn$ADA.Southbound
stn$ADA_full <- as.numeric(stn$ADA_full == 2)

