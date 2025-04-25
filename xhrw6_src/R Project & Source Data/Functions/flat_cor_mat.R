################################################################################################################################-
## CREATED: 12/14/2020
## VERSION: 1.0
## LAST UPDATED:
## 
## AUTHOR: Joel Devonshire, The University of Notre Dame
## 
## DESCRIPTION: Function I found online that creates a "long" version of a correlation matrix (e.g., for exporting to Excel
##              and sorting, filtering, formatting, etc.). The function takes as input a correlation matrix from the rcorr
##              package (see commented example beneath the function code below).
##  
##   
## RELEASE NOTES: 
## 
##                
## KNOWN ISSUES:
## 
###############################################################################################################################- 


flat_cor_mat <- function(cor_n, cor_r, cor_p){
        #This function provides a simple formatting of a correlation matrix
        #into a table with 4 columns containing :
        # Column 1 : row names (variable 1 for the correlation test)
        # Column 2 : column names (variable 2 for the correlation test)
        # Column 3 : the number of observations
        # Column 4 : the correlation coefficients
        # Column 5 : the p-values of the correlations
        library(tidyr)
        library(tibble)
        cor_n <- rownames_to_column(as.data.frame(cor_n), var = "row")
        cor_n <- gather(cor_n, column, n, -1)
        cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
        cor_r <- gather(cor_r, column, cor, -1)
        cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
        cor_p <- gather(cor_p, column, p, -1)
        cor_p_matrix <- left_join(cor_n, cor_r, by = c("row", "column")) %>%
                left_join(cor_p, by = c("row", "column"))
        cor_p_matrix
}

# ~ Export correlation table ---- 
# rMeasures <- rcorr(as.matrix(Measures[,c("EDNhComp","ACEs_Sum","Political","LowStatus","MEQ_Total","Discrim_Major_Sum","Discrim_Everyday_Mean",
#                                          "BNSSh_Eff_Mean","BNSSh_Dis_Mean","BNSSh_Belonging","BNSSh_Control",
#                                          "BNSSh_Competence","BNSSh_Autonomy",
#                                          "BNSSh_Trust","BNSSh_Purpose","BNSSh_Enhancement","BNSSh_Satisf",
#                                          "BNSS_Eff_Mean","BNSS_Dis_Mean", "BNSS_Belonging","BNSS_Control","BNSS_Competence","BNSS_Autonomy",
#                                          "BNSS_Trust","BNSS_Purpose","BNSS_Enhancement","BNSS_Satisf",
#                                          "Crit_Reflection_Mean","System_Justif_Mean","Efficacy_Mean",
#                                          "Crit_Action_Mean","Militarism_Mean","Zero_Sum_Mean")],
#                              use="complete.obs"))
# 
# 
# my_cor_matrix <- flat_cor_mat(rMeasures$n, rMeasures$r, rMeasures$P)
# 
# open(my_cor_matrix)