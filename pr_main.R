#load packages
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(janitor)
library(naniar)

#Read in 2020 People survey data for LGB+
LGB2020 <- read_excel("C:/Users/prile/OneDrive/cs-lgbt/cs-lgbt-dashboard/Data/LGB_2020.xlsx",
           sheet = "LGB+", col_names = TRUE)

#Remove top 3 rows as they don't include relevant data
LGB2020 <- LGB2020[-c(1:3), ]

#Make first row the new headers, and change some column names to something small
LGB2020 <- row_to_names(LGB2020, 1)

LGB2020 <- rename(LGB2020, "Demo_info" = "Demographic variable [1, 5, 6]",
                  "ees" = colnames(LGB2020)[4],
                  "mw_p" = colnames(LGB2020)[5],
                  "op_p" = colnames(LGB2020)[6],
                  "lm_p" = colnames(LGB2020)[7],
                  "mt_p" = colnames(LGB2020)[8],
                  "ld_p" = colnames(LGB2020)[9],
                  "if_p" = colnames(LGB2020)[10],
                  "rw_p" = colnames(LGB2020)[11],
                  "pb_p" = colnames(LGB2020)[12],
                  "lc_p" = colnames(LGB2020)[13],
                  "E01" = colnames(LGB2020)[83],
                  "E03" = colnames(LGB2020)[104]
)

#Transform data into tidy format for easier analysis
LGBTidy <- LGB2020 %>% 
            gather(Question, percentage, -Demo_info, -Response, -Count) %>% 
  mutate(Demo_info_three = gsub(":.*","", Demo_info))

#Merge Annexes with the demo_info and questions so that the info is available
#Start by reading in annex tabs into dfs
demoQuestions <- read_excel("C:/Users/prile/OneDrive/cs-lgbt/cs-lgbt-dashboard/Data/LGB_2020.xlsx",
                            sheet = "Annex_1", col_names = FALSE)

surveyQuestions <- read_excel("C:/Users/prile/OneDrive/cs-lgbt/cs-lgbt-dashboard/Data/LGB_2020.xlsx",
                              sheet = "Annex_2", col_names = FALSE)


#Make the fourth row the new column names for demographic annex questions
demoQuestions <- row_to_names(demoQuestions, 4)
demoQuestions <- rename(demoQuestions, "Question_code" = "Question code [1, 5, 6]",
                                       "Question_label" = "Question label")

#Make fourth row the new column name for the survey questions
surveyQuestions <- row_to_names(surveyQuestions, 4)
surveyQuestions <- rename(surveyQuestions, "Question_code" = "Question code [2, 3, 4]",
                                           "Question_label" = "Question label [7]")


#Merge annexes with the question codes in master df
LGBTidyMerge <- left_join(LGBTidy, demoQuestions, by = c("Demo_info_three" = "Question_code"))

LGBTidyMerge <- left_join(LGBTidyMerge, surveyQuestions, by = c("Question" = "Question_code"))


#Tidy up columns by removing question codes and renaming duplicated cols
LGBTidyNewCols <- LGBTidyMerge %>% 
                    select(-Demo_info, -Question) %>% 
                    rename("Demo_question" = "Question_label.x",
                           "Survey_question" = "Question_label.y",
                           "Demo_response" = "Response")


#Reorder columns in a more useful way
#LGBOrdered <- LGBTidyNewCols[, c(4, 1, 2, 5, 3)]

#Summarise data to check data types
#summary(LGBOrdered)

#Make columns into correct data types
#change data types
LGBDataTypes <- LGBTidyNewCols %>%
                transform(Count = as.numeric(Count),
                          percentage = as.numeric(percentage),
                          Demo_question = as.factor(Demo_question),
                          Survey_question = as.factor(Survey_question))

#Check structure and summarise data after changes
#[c] stands for suppressed in the data, which has been changed to NA during data type change
str(LGBDataTypes)
summary(LGBDataTypes)

## Ethnicity -----------------------------------------------------------------------------------------------


#Summarise data to analyse
#Look at ethnicity info and question of bullying
EthnicityBullying <- LGBDataTypes %>% 
                      filter(Demo_question == "Ethnicities (full)", 
                             Survey_question == "Have you been bullied or harassed at work, in the past 12 months? (% yes)")

#Create graph with ethnicity and bullying data
ggplot(EthnicityBullying, aes(x = Demo_response, y = percentage)) +
  geom_bar(stat = "identity") +
  labs(x = "Ethnicity", y = "Average % of LGB+ colleagues bullied",
       title = "Median percentage of LGB+ colleagues experiencing bullying per Ethnicity") +
  theme(axis.text.x = element_text(colour = "black", size = 8, angle = 90, vjust = .8, hjust = 0.8))

## Sexual orientation ----------------------------------------------------------------------------------------------------------

#Look at bullying and harrassment info for LGB
sexualOrientationBullying <- LGBDataTypes %>% 
                      filter(Demo_question == "Which of the following (sexual orientation) options best describes how you think of yourself?", 
                              Survey_question == "Have you been bullied or harassed at work, in the past 12 months? (% yes)")


#Create graph with sexual orientation and bullying dara
ggplot(sexualOrientationBullying, aes(x = Demo_response, y = percentage)) +
  geom_bar(stat = "identity")



## By organisation -------------------------------------------------------------------------------------------------------
#Look at organisations and their score for bullying
orgBullying <- read_excel("C:/Users/prile/OneDrive/cs-lgbt/cs-lgbt-dashboard/Data/LGB_2020.xlsx",
                              sheet = "Organisations", col_names = FALSE)

#Remove unnecessary rows and make column names 
orgBullying <- row_to_names(orgBullying, 5) 

#Replace suppressed values with NA
orgBullying[orgBullying == '[c]'] <- NA


#Reduce dataframe to important columns
orgBullyingShort <- orgBullying %>% 
                      select('Organisation group', 'Organisation name', 'Response [1]', 'Count', "E03: Have you been bullied or harassed at work, in the past 12 months? (% yes) [4]" )


#Rename columns to easier ones
orgBullyingShort <- rename(orgBullyingShort, "OrgGroup" = "Organisation group",
                                             "OrgName" = "Organisation name",
                                             "Response" = "Response [1]",
                                             "Bullying" = "E03: Have you been bullied or harassed at work, in the past 12 months? (% yes) [4]" )

#group by organisation group and filter by LGB+ responses only
#Transform the columns that need to be numeric and also remove NAs
orgBullyingFilter <- orgBullyingShort %>%
                      transform(Count = as.numeric(Count),
                                Bullying = as.numeric(Bullying)) %>% 
                      na.omit() %>% 
                      filter(Response == "LGB+")

#Group by Organisation group and create column with median score
orgBullyingGroup <- orgBullyingFilter %>% 
                      group_by(OrgGroup) %>%
                      summarise_at(vars(Bullying), list(Average = median))

#Create bar graph with values
ggplot(orgBullyingGroup, aes(x = OrgGroup, y = Average)) +
  geom_bar(stat = "identity") +
  labs(x = "Organisation group", y = "Average % of LGB+ colleagues bullied",
       title = "Median percentage of LGB+ colleagues experiencing bullying per Civil Service Organisation") +
  theme(axis.text.x = element_text(colour = "black", size = 8, angle = 90, vjust = .8, hjust = 0.8))
