library(tidyverse)
library(dplyr)

ESS11 <- read_csv("data/ESS11.csv")
View(ESS11)

# EDA ---------------------------------------------------------------------

# ccnthum - climate change cause ------------------------------------------

class(ESS11$ccnthum) #verifying variable type -> numerical

summary(ESS11$ccnthum)

#There are some missing/don't know, so I'm going to clean the data

ESS11$ccnthum[ESS11$ccnthum %in% c(55, 77, 88, 99)] <- NA
#I've added the "climate change is not happening" responses as NAs because they were
#very few and also didn't seem to work with the following recoding.
#We should look into it though! I think it may be methodologically inappropriate.

#I want to get a frequency table, so I'm going to make my variable a factor
#based on the answers of the questionnaire

clim_change_cause <- factor(ESS11$ccnthum,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Entirely natural", "Mainly natural", "Equally natural and human", "Mainly human", "Entirely human"))

clim_change_cause

summary(clim_change_cause)


#checking if the recoded variable keeps the same percentages
prop.table(table(ESS11$ccnthum))

prop.table(table(clim_change_cause))


#visualizing the distribution
ggplot(ESS11, aes(x = clim_change_cause)) +
  geom_bar(color = "purple", fill = "purple") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
         axis.text.y = element_text(size = 12)) +
  labs(title = "Beliefs about causes of climate change",
       x = "Natural vs. human",
       y = "Count")

ggsave("plots/climate_causes_beliefs.png", width = 8, height = 6, dpi = 300)
