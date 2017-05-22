## This script serves to combine the new datasets received from EPA for change comparisons across all 3 survey years.
## This is done separately as the data were received later, and so keeping the code separate was neater.
## The final output for this file is saved twice, once as a separate combined file using the convention "NCCA_trend_YYYY_MM_DD.csv", and again as "trend_temp.csv"
## "trend_temp.csv" is then read into the main script, script_3.R, and combined it with the other Condition and Change data received from EPA.

### Updated 4/4/2017

############################################
#   NOTES ON MODIFIED SCRIPT FOR EPA USE   #
############################################

### Written on 4/4/2017
### PLEASE READ BEFORE PROCEEDING ###

# Directory:
### Please set your working directory below:
setwd("C:/REPLACE WITH YOUR DIRECTORY HERE/")

### This R script titled "script_for_EPA" has been adapted for EPA's use.
### If there are any questions or comments, please email Anthea Piong at anthea@crowinsight.com

# What this script does:
### This script is similar to the main script "script_for_EPA.R" in that it combines, cleans, and prepares Trend data for the NCCA dashboard.
### Much of the code in this script is similar in function to the other main script, except that this only works with the Trend datasets.
### While this script should be run first before running the main script, do read through the notes in the main script as well before compiling any code.

# Subsetting and transforming:
### As the datasets used in this script differ slightly from the Condition Estimates and Status datasets, a separate set of tables are needed to subset and transform the Trend data.
### The "keep_subpop_trend.csv" table is used to subset out the subpopulations to be included in the NCCA dashboard.
### The "indicator_names_trend.csv" table is used to convert "raw" data names to be more readable and in accordance with EPA terminology.
### These tables can be edited separately to add new subpopulations, or change names. Only the columns of raw names used should remain the same, as that is used as the match reference.


###########################
#   LOADING THE PACKAGES  #
###########################

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("plyr")
# install.packages("stringr")

library(dplyr)
library(tidyr)
library(plyr)
library(stringr)


########################
#   LOADING THE DATA   #
########################

EPA_SQI_BI_change <- read.csv("EPAREGSQI_BI_Change_data_03.04.2017.csv")
EPA_WQI_change <- read.csv("EPAREGWQI_CHANGE_VALUES_03.04.2017.csv")
NCCA_SQI_BI_change <- read.csv("NCCAREGSQI_BI.CHANGE_03.04.2017.csv")
NCCA_WQI_change <- read.csv("NCCAREGWQI_CHANGE DATA_03.04.2017.csv")

# Converts columns with numbers into numeric type, so that bind_rows will work.
for(i in c(5:ncol(EPA_SQI_BI_change))) {
  EPA_SQI_BI_change[,i] <- as.numeric(as.character(EPA_SQI_BI_change[,i]))
}

for(i in c(5:ncol(EPA_WQI_change))) {
  EPA_WQI_change[,i] <- as.numeric(as.character(EPA_WQI_change[,i]))
}

for(i in c(5:ncol(NCCA_SQI_BI_change))) {
  NCCA_SQI_BI_change[,i] <- as.numeric(as.character(NCCA_SQI_BI_change[,i]))
}

for(i in c(5:ncol(NCCA_WQI_change))) {
  NCCA_WQI_change[,i] <- as.numeric(as.character(NCCA_WQI_change[,i]))
}

# Renaming columns. Coercing all names to match NCCA_WQI_change to facilitate joining
# Column names in EPA_WQI are different than in the other datasets. Change column names in order to join
newnames <- names(NCCA_WQI_change)

names(EPA_WQI_change) <- newnames
names(EPA_SQI_BI_change) <- newnames
names(NCCA_SQI_BI_change) <- newnames

# Binding rows on column names
trend_1 <- bind_rows(EPA_SQI_BI_change, EPA_WQI_change)
trend_2 <- bind_rows(NCCA_SQI_BI_change, NCCA_WQI_change)
trend_3 <- bind_rows(trend_1, trend_2)

# Renaming headers and making it more human readable. See Trend_labels.csv for lookup table.
labels <- read.csv("Trend_labels.csv")
oldnames <- c(paste(labels$Trend.raw))
newnames <- c(paste(labels$Trend.new))

names(trend_3) <- str_replace(string=names(trend_3), pattern=oldnames, replacement=newnames)

###################################
##  RENAMING VALUES AND HEADERS  ##
###################################

# Renaming category values
proper_case <- c("FAIR" = "Fair", "GOOD" = "Good", "POOR" = "Poor", "MISS" = "Missing")
trend_3$Condition <- revalue(trend_3$Condition, proper_case)
trend_4 <- filter(trend_3, Subpopulation!="US EPA")
# write.csv(trend_5, "trend test.csv", row.names=FALSE)

#####################
## CLEANING NAMES  ##
#####################

indicator_trend <- read.csv("indicator_names_trend.csv")
subpopulation_trend <- read.csv("keep_subpop_trend.csv")

trend_5 <- rename(trend_4, c("Indicator" = "Indicator.Old", "Type" = "Type.Old", "Subpopulation" = "Subpopulation.Old"))
trend_6 <- left_join(trend_5, indicator_trend, by=c("Indicator.Old" = "Indicator.Old"))
trend_7 <- left_join(trend_6, subpopulation_trend, by=c("Type.Old" = "Type.Old", "Subpopulation.Old" = "Subpopulation.Old"))

#########################
##  DROPPING OLD COLS  ##
#########################

trend_8 <- trend_7[-c(1:3)]
join880 <- c("Subpopulation" = "Subpopulation", "Subpopulation.Plain.Language" = "Subpopulation.Plain.Language", "Aggregation" = "Aggregation", "Indicator" = "Indicator", "Condition" = "Condition", "Type" = "Type", "Source" = "Source", "Indicator.Plain.Language" = "Indicator.Plain.Language")

###################
## DATA PADDING  ##
###################
subpop <- read.csv("keep_subpop.csv")  # 20 subpopulations
indicator <- read.csv("indicator_names.csv")  # 11 indicators
Condition <- c("Good", "Fair", "Poor", "Missing") # 4 condition categories
condition <- as.data.frame(Condition)

# Creating 'Blank' dataframe with 880 rows
df880 <- merge(merge(subpop, indicator), condition)
#df880 <- arrange(df880, df880$Type, df880$Subpopulation, df880$Indicator) #sorts data by Type, then Subpopulation, then Indicator

trend_9 <- left_join(df880, trend_8, by=join880)
trend_9$Type <- revalue(trend_9$Type, c("NEP" = "Chesapeake_Bay"))

setwd("./Combined")
write.csv(trend_9, "NCCA_trend_2017_03_04.csv", row.names = FALSE)
setwd("..")

# Extra file to be used for combining with Change and Condition data in script_3.R
write.csv(trend_9, "trend_temp.csv", row.names = FALSE)