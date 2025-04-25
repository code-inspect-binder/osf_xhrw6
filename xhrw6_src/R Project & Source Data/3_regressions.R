# Regressions -------------------------------------------------------------------------------------------------------------
# This section runs a series of multiple regression models. For model diagnostics, let's first
# pull in a function that we can call whenever we need it:
source(here("Functions","regression_diagnostics.R"))

# ~ Basic Needs -------------------------------------------------------------------------------------------------------
# Above, we created sub-scale scores for basic needs (history and adult), including "effectance," "discouragement," and variables
# for each individual type of need. The correlation matrix confirmed that, unsurprisingly, these are all highly correlated with one 
# another, so when you use them in regression analyses, you'll want to be careful about multicollinearity. But, in general, this 
# section does some work on seeing whether, and how, basic needs (particularly in childhood) predict outcomes (particularly those
# of political orientation and critical reflection, such as system justification. We explore this in part also in the SEM models
# further below, but regressions allow a more targeted analysis (and a more straightforward inclusion of control variables).
# I'm particularly interested in testing the relationship between specific needs in childhood and system justification in adulthood.
# 
# For controls, the one that makes the most sense is SES, so we will perform some hierarchical regressions that first include the basic
# needs variable(s) and then will add in a couple of SES variables. We might also consider some other demographics, such as age and gender,
# but in discussing it with Darcia, it seemed apparent that those latter variables have an unclear rationale for why we'd want to control
# for them. One interesting control (or interaction term) to think about would be the "LowStatus" variable I created up above. Controlling
# for this variable would mean that after one considers how status identity markers (including SES) affect the outcomes, does the meeting
# of needs in childhood have any additional predictive power? An interaction term would mean that basic needs in childhood has different
# predictive power, depending on the level of "low status" identity.
# 
# Finally, remember that for the "Political" score, higher scores means more conservative identification.

rcorr(as.matrix(Measures[,c("Income_Current","HH_Size","Adj_Income","Ed_Mom","Ed_Dad","Parent_Edu","Income_Growing","LowStatus")],
                use="complete.obs"))

sjmisc::frq(Measures$Political, out = "v")
sjmisc::frq(Measures$Political_Group, out = "v")

sjmisc::descr(Measures, Measures$Income_Current, out = "v")
sjmisc::frq(Measures$Income_Current, out = "v")
ggplot(Measures, aes(Income_Current)) + geom_histogram(binwidth=1)

sjmisc::descr(Measures, Measures$Adj_Income, out = "v")
ggplot(Measures, aes(Adj_Income)) + geom_histogram(binwidth=1)

sjmisc::descr(Measures, Measures$Parent_Edu, out = "v")
ggplot(Measures, aes(Parent_Edu)) + geom_histogram(binwidth=1)

sjmisc::descr(Measures, Measures$Income_Growing, out = "v")
sjmisc::frq(Measures$Income_Growing, out = "v")
ggplot(Measures, aes(Income_Growing)) + geom_histogram(binwidth=1)


# First, we'll look at general basic needs history effectance and discouragement for political orientation and Critical Reflection 
# (and the sub-scale of system justification). 

# Political orientation - Needs Effectance
data <- Measures[which(complete.cases(Measures[,c('Political', 'BNSSh_Eff_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(Political ~ BNSSh_Eff_Mean, data)
summary(lm.beta(m1))

m2 <- lm(Political ~ BNSSh_Eff_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)

 # Model Diagnostics
   regression_diagnostics(m2)
        
   data %>% slice(127:127) %>% dplyr::select(id)
   data %>% slice(130:130) %>% dplyr::select(id)
   data %>% slice(351:351) %>% dplyr::select(id)


# Political orientation - Needs Discouragement
data <- Measures[which(complete.cases(Measures[,c('Political', 'BNSSh_Dis_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(Political ~ BNSSh_Dis_Mean, data)
summary(lm.beta(m1))

m2 <- lm(Political ~ BNSSh_Dis_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)

        # Model Diagnostics
        regression_diagnostics(m2)      


# Critical Reflection - Needs Effectance
data <- Measures[which(complete.cases(Measures[,c('Crit_Reflection_Mean', 'BNSSh_Eff_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(Crit_Reflection_Mean ~ BNSSh_Eff_Mean, data)
summary(lm.beta(m1))

m2 <- lm(Crit_Reflection_Mean ~ BNSSh_Eff_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)
        
        # Model Diagnostics
        regression_diagnostics(m2)

        data %>% slice(145:145) %>% dplyr::select(id)
        data %>% slice(133:133) %>% dplyr::select(id)
        data %>% slice(173:173) %>% dplyr::select(id)
        data %>% slice(205:205) %>% dplyr::select(id)

open(data %>% 
        select(id, Crit_Reflection_Mean, BNSSh_Eff_Mean, Adj_Income, Parent_Edu, Income_Growing) %>%
        filter(id %in% c("5ef2ce727fab4b18187eb517","5b22f24c38fc0c0001313ea8","59ff81fd5d06850001afeeab","5e51cbd4ad88cd2047eaf29d")))       
        
# Critical Reflection - Needs Discouragement
data <- Measures[which(complete.cases(Measures[,c('Crit_Reflection_Mean', 'BNSSh_Dis_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(Crit_Reflection_Mean ~ BNSSh_Dis_Mean, data)
summary(lm.beta(m1))

m2 <- lm(Crit_Reflection_Mean ~ BNSSh_Dis_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)

        # Model Diagnostics
        regression_diagnostics(m2)        

# System Justification - Needs Effectance
data <- Measures[which(complete.cases(Measures[,c('System_Justif_Mean', 'BNSSh_Eff_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(System_Justif_Mean ~ BNSSh_Eff_Mean, data)
summary(lm.beta(m1))

m2 <- lm(System_Justif_Mean ~ BNSSh_Eff_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)

        # Model Diagnostics
        regression_diagnostics(m2)        


# System Justification - Needs Discouragement
data <- Measures[which(complete.cases(Measures[,c('System_Justif_Mean', 'BNSSh_Dis_Mean', 'Adj_Income', 'Parent_Edu', 'Income_Growing')])),]
m1 <- lm(System_Justif_Mean ~ BNSSh_Dis_Mean, data)
summary(lm.beta(m1))

m2 <- lm(System_Justif_Mean ~ BNSSh_Dis_Mean + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m2))

anova(m1,m2)

        # Model Diagnostics
        regression_diagnostics(m2)      
        

# ~ Childhood experience and critical reflection (controlling for SES)-----------------------------------------------------------------------
# This is an exploratory an exploratory analysis to see whether controlling for SES specifically makes a difference in the 
# childhood to outcome paths.

rcorr(as.matrix(Measures[,c("EDNhComp","ACEs_Sum")],
                use="complete.obs"))
        
# Critical Reflection
data <- Measures[which(complete.cases(Measures[,c('Crit_Reflection_Mean','EDNhComp','Adj_Income', 'Parent_Edu', 'Income_Growing',
                                                  'ACEs_Sum','LowStatus')])),]
m1 <- lm(Crit_Reflection_Mean ~ EDNhComp, data)
summary(lm.beta(m1))

m2 <- lm(Crit_Reflection_Mean ~ EDNhComp + ACEs_Sum, data)
summary(lm.beta(m2))

m3 <- lm(Crit_Reflection_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m3))

m4 <- lm(Crit_Reflection_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing + LowStatus, data)
summary(lm.beta(m4))

anova(m1,m2,m3,m4)

regression_diagnostics(m3)
regression_diagnostics(m4) 

# Efficacy
data <- Measures[which(complete.cases(Measures[,c('Efficacy_Mean','EDNhComp','Adj_Income', 'Parent_Edu', 'Income_Growing',
                                                  'ACEs_Sum','LowStatus')])),]
m1 <- lm(Efficacy_Mean ~ EDNhComp, data)
summary(lm.beta(m1))

m2 <- lm(Efficacy_Mean ~ EDNhComp + ACEs_Sum, data)
summary(lm.beta(m2))

m3 <- lm(Efficacy_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m3))

m4 <- lm(Efficacy_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing + LowStatus, data)
summary(lm.beta(m4))

anova(m1,m2,m3,m4)

regression_diagnostics(m3)
regression_diagnostics(m4)  


# Critical Action
data <- Measures[which(complete.cases(Measures[,c('Crit_Action_Mean','EDNhComp','Adj_Income', 'Parent_Edu', 'Income_Growing',
                                                  'ACEs_Sum','LowStatus')])),]
m1 <- lm(Crit_Action_Mean ~ EDNhComp, data)
summary(lm.beta(m1))

m2 <- lm(Crit_Action_Mean ~ EDNhComp + ACEs_Sum, data)
summary(lm.beta(m2))

m3 <- lm(Crit_Action_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m3))

m4 <- lm(Crit_Action_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing + LowStatus, data)
summary(lm.beta(m4))

anova(m1,m2,m3,m4)

regression_diagnostics(m3)
regression_diagnostics(m4)  


# BLM Activism
data <- Measures[which(complete.cases(Measures[,c('BLM_Activism','EDNhComp','Adj_Income', 'Parent_Edu', 'Income_Growing',
                                                  'ACEs_Sum','LowStatus')])),]
m1 <- lm(BLM_Activism ~ EDNhComp, data)
summary(lm.beta(m1))

m2 <- lm(BLM_Activism ~ EDNhComp + ACEs_Sum, data)
summary(lm.beta(m2))

m3 <- lm(BLM_Activism ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m3))

m4 <- lm(BLM_Activism ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing + LowStatus, data)
summary(lm.beta(m4))

anova(m1,m2,m3,m4)

regression_diagnostics(m3)
regression_diagnostics(m4)  




# Militarism
data <- Measures[which(complete.cases(Measures[,c('Militarism_Mean','EDNhComp','Adj_Income', 'Parent_Edu', 'Income_Growing',
                                                  'ACEs_Sum','LowStatus')])),]
m1 <- lm(Militarism_Mean ~ EDNhComp, data)
summary(lm.beta(m1))

m2 <- lm(Militarism_Mean ~ EDNhComp + ACEs_Sum, data)
summary(lm.beta(m2))

m3 <- lm(Militarism_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing, data)
summary(lm.beta(m3))

m4 <- lm(Militarism_Mean ~ EDNhComp + ACEs_Sum + Adj_Income + Parent_Edu + Income_Growing + LowStatus, data)
summary(lm.beta(m4))

anova(m1,m2,m3,m4)

regression_diagnostics(m3) 
