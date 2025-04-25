################################################################################################################################-
## CREATED: 02/26/2021
## VERSION:
## LAST UPDATED:
## 
## AUTHOR: Joel Devonshire, The University of Notre Dame
## 
## DESCRIPTION: Taken from the web, APA style formatting for gt table objects. (https://gist.github.com/pdparker/1b61b6d36d09cb295bf286a931990159)
##  
##   
## RELEASE NOTES: 
## 
##                
## KNOWN ISSUES:
## 
###############################################################################################################################- 

# Provides APA 7th Styling to a table ####
# Only works using the global-font-options branch of the gt package

#Requires
library(gt)
library(tidyverse)

# APA style ####
apa_style <- function(data) {
        data %>%
                opt_table_lines(extent = "none") %>%
                tab_options(
                        heading.border.bottom.width = 2,
                        heading.border.bottom.color = "black",
                        heading.border.bottom.style = "solid",
                        table.border.top.color = "white",
                        table_body.hlines.color = "white",
                        table_body.border.top.color = "black",
                        table_body.border.top.style = "solid",
                        table_body.border.top.width = 1,
                        heading.title.font.size = 12,
                        table.font.size = 12,
                        heading.subtitle.font.size = 12,
                        table_body.border.bottom.color = "black",
                        table_body.border.bottom.width = 1,
                        table_body.border.bottom.style = "solid",
                        column_labels.border.bottom.color = "black",
                        column_labels.border.bottom.style = "solid",
                        column_labels.border.bottom.width = 1
                ) %>%
                opt_table_font(font = "times")
}