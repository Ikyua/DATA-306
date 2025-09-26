install.packages("tidyjson")
install.packages("haven")
install.packages("tidyverse")

library(tidyjson)
library(haven)
library(tidyverse)

# help(worldbank) # sometimes packages come with their own tutorials
str(worldbank)
worldbank[1]
worldbank[251] # <- 251 value
worldbank[131] # <- 131 value

wb_table_ethiopia <- spread_all(worldbank[1]) # notice the <-
wb_df_ethiopia <- as.data.frame(wb_table_ethiopia)
                                     ## <- means define everything to the right under
                                     ## the object name to the left.
wb_df_ethiopia # invoking the object name returns the contents of the object

wb_df_ethiopia[2:7] # now we are "subsetting" columns 2 through 7 of the table

wb_table <- spread_all(worldbank[1:10])
wb_df <- as.data.frame(wb_table)
wb_df[2:7]

getwd()
setwd("C:/Workspace/DATA-306")

write.csv(wb_df[2:7], "worldbank_selection.csv")

mydata <- read.csv("data/worldbank_selection.csv") # <-- relative file path
str(mydata) # look at the data
