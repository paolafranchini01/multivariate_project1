library(tidyverse)
library(dplyr)

ESS11 <- read_csv("data/ESS11.csv")
View(ESS11)

# EDA ---------------------------------------------------------------------

# I want to clean the data by finding and excluding NAs, and also start
#start visualizing univariate distributions. In the end, I would like to explore
#some basic bivariate relationships between variables.
#To do so, I'm taking each potential variable we may use in the analysis and
#applying this step. I'm going to consider variables about climate change and its
#perception and variables about individual level of education and positioning on
#the left-right scale.

# ccnthum - climate change cause ------------------------------------------

class(ESS11$ccnthum) #verifying variable type -> numeric

summary(ESS11$ccnthum)

prop.table(table(ESS11$ccnthum))

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




# ccrdprs - personal responsibility to reduce climate change ---------------------

class(ESS11$ccrdprs) #verifying variable type -> numeric

summary(ESS11$ccrdprs)

prop.table(table(ESS11$ccrdprs)) #there are missing values coded as 66,77,88,99

#assigning NAs
ESS11$ccrdprs[ESS11$ccrdprs %in% c(66, 77, 88, 99)] <- NA

#recoding as an ordered factor

ESS11 <- ESS11 %>%
  mutate(
    clim_change_resp = case_when(
      ccrdprs >= 1 & ccrdprs <= 3 ~ "Low",
      ccrdprs >= 4 & ccrdprs <= 7 ~ "Medium",
      ccrdprs >= 8 & ccrdprs <= 10 ~ "High"
    ),
    clim_change_resp = factor(clim_change_resp, levels = c("Low", "Medium", "High"), ordered = TRUE)
  )

#adding a plot to visualize distribution
ESS11 |> 
  filter(!is.na(clim_change_resp)) |>  #I want to exclude NAs from the bar graph
  ggplot(aes(x = clim_change_resp)) +
  geom_bar(color = "purple", fill = "purple") +
  theme(axis.text.x = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  labs(title = "Feelings of personal responsibilities to reduce climate change",
       x = "Perceived responsibility",
       y = "Count")
#I don't know if I should drop the NAs from the visualization; I may eliminate
#the filter later.

ggsave("plots/climate_resp_beliefs.png", width = 8, height = 6, dpi = 300)


