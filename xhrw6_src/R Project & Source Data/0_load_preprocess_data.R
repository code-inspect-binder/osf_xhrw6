###############################################################################################################################-
## CREATED: 05/13/2020
## VERSION: 3.1 (Production)
## LAST UPDATED: 06/01/2021
## 
## AUTHOR: Joel Devonshire, The University of Notre Dame
## 
## DESCRIPTION: This code analyzes data from the Critical Consciousness web survey (first-year project).
##  
##   
## RELEASE NOTES: With this code version (3.0), I'm implementing a major change to my R workflow and file organization.
##                Code will now all live in the project folder, and long code files will be more modularized
##                (and numbered), with "source" commands to call subsequent code pieces.
##                   3.1 Update: Added descriptive table exports (in script #1) 
##                
##                
## KNOWN ISSUES: 
## 
##############################################################################################################################- 

# Load required packages ------------------------------------------------------------------------------------------------------
library(here)
library(readxl)
library(haven)
library(Hmisc)
library(descr)
library(UpSetR)
library(tidyverse)
library(lubridate)
library(psych)
library(car)
library(ggpubr)
library(corrplot)
library(xtable)
library(sjmisc)
library(sjPlot)
library(lavaan)
library(semTools)
library(tidySEM)
library(semTable)
library(mice)
library(Amelia)
library(lm.beta)
library(stargazer)
library(pscl)


# Load APA Style function for formatting tables
source(here("Functions", "corstars.R"))

# Import Data------------------------------------------------------------------------------------------------------------------------
# Import the SPSS data (N = 361)
RawData <-  read_spss(here("Source Data","Critical_Consciousness_Prolific_Data_Raw.sav")) %>%
  mutate(id = PROLIFIC_PID)

# To re-create a kind of "variable view," we'll save off the variable labels (not the code frames, but just labels):
RawData.labels <- as.data.frame(lapply(RawData, function(x) attributes(x)$label)) %>%
        gather(key = "Variable", value = "Label")

colnames(RawData)

# First, let's try to figure out where that extra person came from (I had been expecting 360). If we have an extra normative person,
# we might consider dropping that 5-minute case. Let's first make sure there are no duplicate id's.
Dupes <- RawData %>%
  count(id) %>%
  filter(n > 1)

# No dupes. Let's check out the study ID to see where that extra person is.
sjmisc::frq(RawData$STUDY_ID, out = "v")

# Ok, that didn't really help, because I can't find the Study ID on the prolific website to match these numbers to. I could find out
# indirectly if I really wanted to by matching participant IDs to studies, but I actually don't think it's all that important. As you'll
# see later, the demographics that I collect in this instrument don't exactly line up with the prescreening demographics, so it's not
# that critical to achieve exact balance in N's across studies.

# Let's examine timings to confirm a relatively normal distribution. 
# Note that the data should already be relatively clean coming in (including timings), since a certain amount of screening was done 
# during data collection on Prolific. We do have a 5-minute case that I'm aware of, and I initially decided to keep. We we may re-assess. 
ggplot(RawData, aes(Duration__in_seconds_/60)) + geom_histogram(binwidth=5) + scale_x_continuous(breaks = seq(0, 1000, by = 5))
ggplot(RawData, aes(Duration__in_seconds_/60)) + geom_histogram(binwidth=1) + scale_x_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15)) 

# So, there is some positive skew, with a handful of cases taking longer than an hour. I am going to get rid of that 5-minute case, since I have
# an extra person anyway. We'll also get a sense of the summary stats of timing
describe(RawData$Duration__in_seconds_/60)

RawData <- RawData %>%
  filter(Duration__in_seconds_ >= 360)

rm(Dupes)

# Demographics -------------------------------------------------------------------------------------------------------------------------------------
# Let's look at some basic descriptives for the data set, and get a sense of the demographic breakdown. 
# Note that it's difficult to assess these numbers in isolation, because there are a lot of overlapping categories (i.e., the same person may
# share multiple specific demographic characteristics). Because of that fact, it won't be surprising to see HIGHER numbers than I explicitly
# sampled for.

    Descriptives1 <- psych::describe(RawData)

    sjmisc::frq(RawData$Gender, out = "v")    # A nice split between M/F
        # I'm actually going to create a binary gender variable and dummy variables too, just to facilitate later analysis
    RawData$GenderBinary <- case_when(RawData$Gender == 1 ~ 1, RawData$Gender == 2 ~ 2)
    RawData$GenderFemale <- case_when(RawData$Gender == 2 ~ 1, TRUE ~ 0)
    RawData$GenderNonBin <- case_when(RawData$Gender == 3 ~ 1, TRUE ~ 0)
    
    sjmisc::frq(RawData$GenderBinary, out = "v")
    sjmisc::frq(RawData$GenderFemale, out = "v")
    sjmisc::frq(RawData$GenderNonBin, out = "v")
    
    sjmisc::frq(RawData$Trans, out = "v")   
    
    CrossTable(as_factor(RawData$Gender), as_factor(RawData$Trans), format=c("SPSS")) # Total distinct n of 32 in non-normative gender categories.
    
    sjmisc::frq(RawData$Agegroup, out = "v")  # Decent distribution of age.
    ggplot(RawData, aes(Agegroup)) + geom_bar()
    
    # Calculate age based on DOB responses (NOTE: apparently, Prolific considers DOB to be PII, so not everyone answered)
    RawData  <-  RawData %>%
      mutate(DOB = ifelse(DOB == "10/15/2020", NA, DOB))

    RawData$DOB2 <- mdy(RawData$DOB)

    RawData %>%
      select(DOB2) %>%
      arrange(DOB2) %>%
      View()
    
    RawData$Age  <-  as.numeric(difftime(Sys.Date(),RawData$DOB2, units = "weeks"))/52.25
    sjmisc::descr(RawData, RawData$Age, out = "v")
    # We'll create a mean-centered version as well (to assist interpretation)
    RawData$AgeC <- scale(RawData$Age, scale = FALSE)
    
    sjmisc::frq(RawData$Race, out = "v")      # I should have at least 60 "non-white" (but more is possible). I have 75, including mixed race.
    sjmisc::frq(RawData$Ethnicity, out = "v") # But there are also 28 Latino/a/Hispanic (in prescreen, I had included this category
                                              # in "non-white" because Latino/a was included in their single combined race/ethnicity question.)
                                            
    CrossTable(as_factor(RawData$Race), as_factor(RawData$Ethnicity), format=c("SPSS"))
    
     RawData %>%
      group_by(Ethnicity) %>%
      sjmisc::frq(Race, out = "v")   
    
     RawData %>%
       filter(Race != 2 | Ethnicity == 1) %>%
       count()                               # There's a distinct n of 92 that fall into either non-white or Latino/a/Hispanic.
     
     
    sjmisc::frq(RawData$Disability, out = "v")  # An n of 58 for disability.
    
    sjmisc::frq(RawData$Sexual, out = "v") # An n of 78 for Gay/Lesbian/Bisexual
    
    sjmisc::frq(RawData$Income_Current, out = "v")
    sjmisc::descr(RawData, RawData$Income_Current, out = "v")
    
    CrossTable(as_factor(RawData$Income_Current), as_factor(RawData$HH_Size), format=c("SPSS"))
    
    RawData %>%
      group_by(HH_Size) %>%
      sjmisc::frq(Income_Current, out = "v") 
    
    sjmisc::frq(RawData$Ed_You, out = "v")
    sjmisc::frq(RawData$Ed_Mom, out = "v")
    sjmisc::frq(RawData$Ed_Dad, out = "v")
    
    CrossTable(as_factor(RawData$Agegroup), as_factor(RawData$Ed_You), format=c("SPSS"))
    CrossTable(as_factor(RawData$Ed_Mom), as_factor(RawData$Ed_Dad), format=c("SPSS"))
    
    sjmisc::frq(RawData$Political, out = "v")
    sjmisc::descr(RawData, RawData$Political, out = "v")
    
# Just some tests of SES
    LowSES <- RawData %>%
      filter(Income_Current == 1 & Ed_Mom < 3 & Ed_Dad < 3)
    
    LowSES <- RawData %>%
      filter(Income_Current == 1 & HH_Size > 2)
    
    LowSES <- RawData %>%
      filter(Income_Current == 1 & HH_Size > 2 & Ed_Mom < 3 & Ed_Dad < 3)
    
    LowSES <- RawData %>%
      filter(Agegroup > 2 & Ed_You < 3 & Ed_Mom < 3 & Ed_Dad < 3)
    
  # Combine a variation of the last two in OR conditions
      LowSES <- RawData %>%
      filter( (Income_Current == 1 & HH_Size > 1 & Ed_Mom < 3 & Ed_Dad < 3) | (Agegroup >2 & Ed_You < 3 & Ed_Mom < 3 & Ed_Dad < 3)
              | (Income_Current == 1 & HH_Size > 2))
    
  
# ~ LowStatus --------------------------------------------------------------------------------------------------------------------------
# Just to get some idea of how many participants we have who have any potentially lower-status demographic characteristics, let's count.
# First, look at the "normative" sample. (I'm not sure how to replicate the SES question here, so the count may be slightly inflated)
Normative <- RawData %>%
  select(id, Agegroup, Gender, Trans, Race, Ethnicity, Disability, Sexual, Income_Current, HH_Size, Ed_You, Ed_Mom, Ed_Dad) %>%
  filter(Trans == 2 & Gender %in% (1:2) & Race == 2 & Ethnicity == 2 & Disability == 2 & Sexual == 1)
      
      
LowStatus <- RawData %>%
  select(id, Agegroup, Gender, Trans, Race, Ethnicity, Disability, Sexual, Income_Current, HH_Size, Ed_You, Ed_Mom, Ed_Dad) %>%
  filter(Trans == 1 | Gender == 3 | Race != 2 | Ethnicity == 1 | Disability == 1 | Sexual %in% (2:4) | 
           (Income_Current == 1 & HH_Size > 1 & Ed_Mom < 3 & Ed_Dad < 3) | (Agegroup >2 & Ed_You < 3 & Ed_Mom < 3 & Ed_Dad < 3)
         | (Income_Current == 1 & HH_Size > 2))

# According to those criteria, 180 (exactly half) of the sample has at least one low-status characteristic.



# COVID and BLM Responses------------------------------------------------------------------------------------------------------------------------
# Descriptive summary of responses to the newly-added questions about COVID and the BLM protests/George Floyd killing.
sjmisc::frq(RawData$COVID_Contracted_1, out = "v")
sjmisc::frq(RawData$COVID_Contracted_2, out = "v")
sjmisc::frq(RawData$COVID_Contracted_3, out = "v")
sjmisc::frq(RawData$COVID_Contracted_4, out = "v")
sjmisc::frq(RawData$COVID_Contracted_5, out = "v")
# So, a total of 189 R's were affected by COVID contraction (or just over half)

sjmisc::frq(RawData$COVID_Died_1, out = "v")
sjmisc::frq(RawData$COVID_Died_2, out = "v")
sjmisc::frq(RawData$COVID_Died_3, out = "v")
sjmisc::frq(RawData$COVID_Died_4, out = "v")
# 36 R's knew someone who died (or 10% of the sample)

sjmisc::frq(RawData$COVID_Income_1, out = "v")
sjmisc::frq(RawData$COVID_Income_2, out = "v")
# A little less than a third of the sample reported significant income loss due to COVID.

sjmisc::frq(RawData$Floyd_1, out = "v")
sjmisc::frq(RawData$Floyd_2, out = "v")
sjmisc::frq(RawData$Floyd_3, out = "v")
sjmisc::frq(RawData$Floyd_4, out = "v")
sjmisc::frq(RawData$Floyd_5, out = "v")
sjmisc::frq(RawData$Floyd_6, out = "v")
sjmisc::frq(RawData$Floyd_7, out = "v")
sjmisc::frq(RawData$Floyd_8, out = "v")

# For the moment, I'm going to ignore the 6 "counter-protest" responses (response option #8), and instead create a sum score
# of BLM-type activism. We'll see later that this will be a useful corollary measure to "Critical Action," since
# this measure is much more specific and contextualized.
# 
# We are also using this step to establish a new "Measures" dataset that will serve as the basis for further analysis.
# We'll create some new measures that will be useful. First, we'll create a political group categorical variable
# that we may end up using. We'll also create an income/household size ratio and an average of parental education 
# that will both serve as SES indicators. 
  Measures <- RawData %>%
    mutate_at(vars(contains("Floyd")), ~replace(., is.na(.), 0)) %>%  
    mutate(BLM_Activism = structure(rowSums(select(., Floyd_1:Floyd_6), na.rm=TRUE), label = "George Floyd/BLM Activism Composite (Sum)")) %>%
    mutate(Political_Group = as_factor(structure(case_when(Political < 4 ~ "Liberal", Political > 4 ~ "Conservative"), label = "Political Group")),
           Adj_Income = structure(Income_Current/HH_Size, label = "Income dvided by HH Size"),
           Parent_Edu = structure(rowMeans(select(., Ed_Mom,Ed_Dad), na.rm=TRUE), label = "Average of both parents' education")) %>%  
    mutate(Adj_IncomeC = scale(Adj_Income, scale = FALSE),
           Parent_EduC = scale(Parent_Edu, scale = FALSE))
  
  ggplot(Measures, aes(Adj_Income)) + geom_histogram()
  sjmisc::descr(Measures, Measures$Adj_Income, out = "v") 
  sjmisc::descr(Measures, Measures$Parent_Edu, out = "v") 
  
  
  # Note that for BLM Activism, I'm omitting the last two items. The last one was counter-protest. Item #7 is a little tougher 
  # ("examined my racism"). It's a good item, but if I want this scale to be distinguished from "critical reflection," then I think only
  # retaining items that relate to taking specific social actions is appropriate.

    sjmisc::frq(Measures$BLM_Activism, out = "v")
    ggplot(Measures, aes(BLM_Activism)) + geom_histogram()
    psych::alpha(Measures %>% select(Floyd_1:Floyd_6))
  

# Create Composite Measures----------------------------------------------------------------------------------------------------------------------
source(here("1_create_composite_measures.R"), echo = TRUE)

           
# Bivariate Correlations & Scatterplots--------------------------------------------------------------------------------------------------------------
source(here("2_correlations_and_scatterplots.R"), echo = TRUE)

    
# Regressions -------------------------------------------------------------------------------------------------------------
source(here("3_regressions.R"), echo = TRUE)


# SEM Analysis Section ---------------------------------------------------------------------------------------------------------------------
source(here("4_sem_models.R"), echo = TRUE)
