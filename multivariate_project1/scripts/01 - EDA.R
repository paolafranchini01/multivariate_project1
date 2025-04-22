library(tidyverse)
library(dplyr)

ESS11 <- read_csv("data/ESS11.csv")
View(ESS11)

# EDA ---------------------------------------------------------------------

# ccnthum - climate change cause ------------------------------------------

class(ESS11$ccnthum) #verifying variable type -> numerical

summary(ESS11$ccnthum)

#There are some missing/don't know, so I'm going to clean the data

ESS11$ccnthum[ESS11$ccnthum %in% c(77, 88, 99)] <- NA


#I want to get a frequency table, so I'm going to make my variable a factor
#based on the answers of the questionnaire

clim_change_cause <- factor(ESS11$ccnthum,
                            levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("Entirely natural", "Mainly natural", "Equally natural and antropic", "Mainly human", "Entirely human", "No change happening"))

clim_change_cause

summary(clim_change_cause)


#checking if the recoded variable keeps the same percentages
prop.table(table(ESS11$ccnthum))

prop.table(table(clim_change_cause))

#some plots

ggplot(ESS11)