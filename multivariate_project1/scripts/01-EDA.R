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

#I agree that the responses are really few, but I've looked at the dataset across all countries, 
#and the answers saying 'I don't think climate change is happening' are not negligible in a few countries—like Cyprus, 
#Greece, Slovakia, and Lithuania—while in all the others, I agree with you that they're minimal. 
#Maybe we should also consider whether we want to focus on just one country, compare a few countries, 
#or include all countries from the ESS11 in our analysis.

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

summary(ESS11$clim_change_resp)
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

edulvlb #Highest level of education; What is the highest level of education you have successfully completed?
class(ESS11$edulvlb) #verifying v. type -> numeric
summary(ESS11$edulvlb) #The response scale values are a bit hard to interpret because the types of education 
#are labeled differently depending on the European country. 
#If we look at edlvfit (highest education level, Italy), which shows the values for Italy, it's easier for us
#to understand.
prop.table(table(ESS11$edulvlb))
#This indicates the highest level of education
#There are also separate variables for each country
#It could be interesting to do an analysis by grouping countries based on their 
#average level of education (for example, countries A and B with low average education, 
#countries C and D with medium, and country E with high), and then see how attitudes 
#toward climate change vary across those groups
summary(ESS11$edlvfit)

#Removed conflict

ESS11$edulvlb_clean <- NA
#I selected the codes assigned in the questionnaire for the variable edulvlb; 
#I thought the best approach would be to remove the values with code 0 (not applicable), 
#5555 (Other), 7777 (Refusal), 8888 (Don't know), 9999 (No answer) because I don't find 
#them useful for the analysis. 
#Then, I created a new variable that assigns a score on a scale from 1 to 7, 
#categorizing the education levels in order to have a numerical score to use in a 
#scale for comparing the average education values across different countries

ESS11$edulvlb_clean[ESS11$edulvlb %in% c(113, 129)] <- 1  
ESS11$edulvlb_clean[ESS11$edulvlb %in% c(131:223)] <- 2  
ESS11$edulvlb_clean[ESS11$edulvlb %in% c(321:323, 412, 413)] <- 3 
ESS11$edulvlb_clean[ESS11$edulvlb %in% c(421:423)] <- 4  
ESS11$edulvlb_clean[ESS11$edulvlb %in% c(510:520)] <- 5 
ESS11$edulvlb_clean[ESS11$edulvlb %in% c(610, 620)] <- 6 
ESS11$edulvlb_clean[ESS11$edulvlb == 800] <- 7  

#I remove the NA values
ESS11_clean <- ESS11[!is.na(ESS11$edulvlb_clean), ]

education <- ESS11_clean %>%
  group_by(cntry) %>%
  summarise(media_edulvlb = mean(edulvlb_clean)) %>%
  arrange(desc(media_edulvlb))

#to visualize graphically tha average of levels of education over EU,
#we can see that NO has the higher level and PT the lowest
#what if we compare the attitudes toward climate change of those 2 countries?
ggplot(education, aes(x = reorder(cntry, media_edulvlb), y = media_edulvlb)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average of higher level of education (ESS11)",
       x = "Country", y = "Average level of education (1 = low, 7 = high)") +
  theme_minimal()





