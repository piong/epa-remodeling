### This R Script serves the function of cleaning and combining datasets from the US EPA's National Coastal Condition Assessment Survey, for the
### purposes of eventual data analysis in a D3 dashboard.

### Updated 4/4/2017

############################################
#   NOTES ON MODIFIED SCRIPT FOR EPA USE   #
############################################

### Written on 4/4/2017
### PLEASE READ BEFORE PROCEEDING ###

### This R script titled "script_for_EPA" has been adapted for EPA's use.
### If there are any questions or comments, please email Anthea Piong at anthea@crowinsight.com

# What this script does:
### In plain language, this script joins and binds all Change Estimate, Status, and Trend data together to form one unified dataset.
### First, this script joins the three Change Estimate files (one per period comparison) and the Status file for each type (e.g. Sediment, Water, ... etc.) together horizontally.
### Next, it binds the four combined data files together vertically to form one large dataset.
### Along the way, columns and rows are added, renamed, and taken away according to what was necessary at the time. Each line is commented to briefly note what action is being taken.
### The combined files are then output and written to a specified folder. There is also the option to split out the combined dataset and write only Condition, Change, or Trend estimates.

# Directory:
### Please set your working directory below:
setwd("C:/REPLACE WITH YOUR DIRECTORY HERE/")

# Reading and writing files:
### Reading - This script assumes raw data are in the parent directory
### Writing - This script will write files to a child directory named "Combined". Be sure to change the save location if you have other preferences.

# Renaming time markers:
### The first survey was taken over the years 1999-2001. EPA uses the notation "91" to denote the first period. Here, it will be referred to as "1" as it is the first survey.
### The second survey was taken over the years 2005-2006. EPA uses the notation "56" to denote the second period. Here, it will be referred to as "2" as it is the second survey.
### The third survey was taken in the year 2010. EPA uses the notation "10" to denote the third period. Here, it will be referred to as "3" as it is the third survey.
##### So, a notation in this script that looks like "1v3" is a comparison of time periods 1 and 3, which is to say, the 1999-2001 survey and the 2010 survey.
##### This notation will appear in the recoding of dataframes as well as columns.

# File naming convention:
### An example name for a raw Change Estimates file provided by the EPA is this: "NCCA_56_10_Benthic_Change_Estimates_20140618.csv"
### And the example name for a raw Status Estimates file provided by the EPA is this: "NCCA10_Benthic_Status_Estimates_20140617.csv"
##### This script loads the data and renames the dataframe upon loading based on the file name. In plain language, this is how the import loop works:
##### - Looks for all files in directory that contain one of the strings specified in [type]. In this case, the raw file must have either "Sediment", "Benthic", "Water", or "Fish" in its name.
##### - Of those files, it looks for those with the word "Status". It will read the dataframe and name it [type]_Status, like Benthic_Status for example.
##### - If a file doesn't have the word "Status", it will then look for a time marker such as "56_10". When the file is read in, the dataframe will be renamed to a simplified time marker.

# Trend data:
### "Trend" data (now called "Change") was received at a later time, and separate from the first batch of raw EPA files.
### Data had to be regenerated in order to normalize all 3 years of data to the same sample size. EPA was in charge of generating the data and sending it to us.
### Although "Trend" is now called "Change" in the final version of the dashboard, for data cleaning and manipulation purposes, the name "Trend" was kept to prevent confusion and switching around.
### Since this data was received separately and at a later time, a separate script was written for the cleaning and prepping of that data. Please run that script first before running this one.

# Subsetting and transforming:
### This script utilizes a couple of other data tables in order to subset and transform the data, in preparation for the NCCA dashboard.
### The "keep_subpop.csv" table is used to subset out the subpopulations to be included in the NCCA dashboard.
### The "indicator_names.csv" table is used to convert "raw" data names to be more readable and in accordance with EPA terminology.
### Each of these tables has a "_trend.csv" version as well to accompany the separate script used to prepare the Trend data.
### These tables can be edited separately to add new subpopulations, or change names. Only the columns of raw names used should remain the same, as that is used as the match reference.
### If you choose not to use these tables, be careful to amend the script as transformed data frames will be referenced on later lines of code.

# Calculations
### Some new columns are generated with calculations based on the raw data (i.e. Significance column, Confidence Interval difference, etc.)

# Rearranging columns:
### In the later part of the script, columns are rearranged to be similar in form to the previous dataset used for the NLA dashboard.
### These columns are identified by their index number. So if an earlier part of the script is amended, this may affect the index numbers of those columns.


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

## GLOBAL STATIC VALUES ##
join_vals <- c("Type" = "Type", "Subpopulation" = "Subpopulation", "Indicator" = "Indicator", "Category" = "Category")
col_suffix <- c("Type", "Subpopulation", "Indicator", "Category")

type <- c("Sediment", "Benthic", "Water", "Fish")

## Imports the data and makes sure to load and name the right files appropriately
for (i in 1:length(type)) {
  quality <- type[i]
  files <- list.files(pattern=c(quality,"*.csv"))
  for (i in 1:length(files)) {
    if(i==grep("Status",files)) {
      assign(paste(quality,"Status",sep="_"), read.csv(files[i]))
    } else if(i==grep("91_56",files)) {
      assign(paste(quality,"1v2",sep="_"), read.csv(files[i]))
    } else if(i==grep("56_10",files)) {
      assign(paste(quality,"2v3",sep="_"), read.csv(files[i]))
    } else if(i==grep("91_10",files)) {
      assign(paste(quality,"1v3",sep="_"), read.csv(files[i]))
    }
  }
}

## Loading new change data for trend bars (2/16/2017)
files <- list.files(pattern=c("CHANGE","*.csv"))
for (i in 1:length(files)) {
  assign(paste("trend_change", i, sep="_"), read.csv(files[i]))
}

# Converts columns with numbers into numeric type, so that bind_rows will work.
for(i in c(5:ncol(trend_change_1))) {
  trend_change_1[,i] <- as.numeric(trend_change_1[,i])
}

for(i in c(5:ncol(trend_change_2))) {
  trend_change_2[,i] <- as.numeric(trend_change_2[,i])
}


#####################
#   SOME CLEANING   #
#####################

# Adding suffixes to column/variable names ###

# Creating a list with dfs, for use with rename_vars function later
inputs <- list(Benthic_1v2 = Benthic_1v2, Benthic_1v3 = Benthic_1v3, Benthic_2v3 = Benthic_2v3, Benthic_Status = Benthic_Status)
inputs2 <- list(Sediment_1v2 = Sediment_1v2, Sediment_1v3 = Sediment_1v3, Sediment_2v3 = Sediment_2v3, Sediment_Status = Sediment_Status)
inputs3 <- list(Water_1v2 = Water_1v2, Water_1v3 = Water_1v3, Water_2v3 = Water_2v3, Water_Status = Water_Status)
inputs4 <- list(Fish_Status = Fish_Status)

# Function to rename variables and appending strings to the end of each variable name based on the name of the df
# Function accounts for ignoring variable names specified in col_suffix, and appends suffixes to all other variables
rename_vars <- function(list) {
  for(i in 1:length(list)) {
    nam <- colnames(list[[i]])
    if(grep("Status", names(list)) == i) {
      list[[i]] <- setNames(list[[i]], ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".stat")))
    } else  if(grep("1v2", names(list)) == i) {
      list[[i]] <- setNames(list[[i]], ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".1v2")))
    } else  if(grep("1v3", names(list)) == i) {
      list[[i]] <- setNames(list[[i]], ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".1v3")))
    } else  if(grep("2v3", names(list)) == i) {
      list[[i]] <- setNames(list[[i]], ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".2v3")))
    }
  }
  for(i in 1:length(list)) {
    assign(names(list)[i], list[[i]], envir = .GlobalEnv)
  }
}

# Renaming all dfs in inputs using rename_vars function
# Possible future improvement to allow all DFs to be stored in single input list. Currently, errors occur when the list has mutiple
# occurrances of a string e.g. "1v3"
rename_vars(inputs)
rename_vars(inputs2)
rename_vars(inputs3)
rename_vars(inputs4)

# Replacing "Surface_Phosphorus" variable in Water_Status to "Surface_DIP" due to inconsistent naming. "Surface_DIP" will match with the Water Change estimates.
Water_Status$Indicator <- revalue(Water_Status$Indicator, c("Surface_Phosphorus" = "Surface_DIP"))


#######################
#  JOINING THE DATA   #
#######################

# Re-storing inputs after adding suffixes
inputs <- list(Benthic_1v2 = Benthic_1v2, Benthic_1v3 = Benthic_1v3, Benthic_2v3 = Benthic_2v3, Benthic_Status = Benthic_Status)
inputs2 <- list(Sediment_1v2 = Sediment_1v2, Sediment_1v3 = Sediment_1v3, Sediment_2v3 = Sediment_2v3, Sediment_Status = Sediment_Status)
inputs3 <- list(Water_1v2 = Water_1v2, Water_1v3 = Water_1v3, Water_2v3 = Water_2v3, Water_Status = Water_Status)
inputs4 <- list(Fish_Status = Fish_Status)

Benthic_joined <- join_all(inputs, by = join_vals, type = "full")
Sediment_joined <- join_all(inputs2, by = join_vals, type = "full")
Water_joined <- join_all(inputs3, by = join_vals, type = "full")
Fish_joined <- join_all(inputs4, by = join_vals, type = "full")

# # Joining data in order (from left to right) of Status, 91_56, 56_10, and 91_10
# # This a bulkier version of the code above, but is simpler to understand.
# Sediment_joined <- Sediment_Status %>%
#   full_join(Sediment_1v2, by = join_vals) %>%
#   full_join(Sediment_1v3, by = join_vals) %>%
#   full_join(Sediment_2v3, by = join_vals)

# Dropping columns with X ID rows in original raw data  
Benthic_joined <- Benthic_joined[, -grep("X.*", colnames(Benthic_joined))]
Sediment_joined <- Sediment_joined[, -grep("X.*", colnames(Sediment_joined))]
Water_joined <- Water_joined[, -grep("X.*", colnames(Water_joined))]
Fish_joined <- Fish_joined[, -grep("X.*", colnames(Fish_joined))]

# Adding Source column and moving it to first column
Benthic_joined$Source <- "Benthic"
Benthic_joined <- Benthic_joined[,c(ncol(Benthic_joined), 1:ncol(Benthic_joined)-1)]

Sediment_joined$Source <- "Sediment"
Sediment_joined <- Sediment_joined[,c(ncol(Sediment_joined), 1:ncol(Sediment_joined)-1)]

Water_joined$Source <- "Water"
Water_joined <- Water_joined[,c(ncol(Water_joined), 1:ncol(Water_joined)-1)]

Fish_joined$Source <- "Fish"
Fish_joined <- Fish_joined[,c(ncol(Fish_joined), 1:ncol(Fish_joined)-1)]

# Dropping rows that say "Total" under the Category column
Benthic_final <- filter(Benthic_joined, Category!="Total")
Sediment_final <- filter(Sediment_joined, Category!="Total")
Water_final <- filter(Water_joined, Category!="Total")
Fish_final <- filter(Fish_joined, Category!="Total")


##################################
#   SAVING JOINED DATA BY TYPE   #
##################################

# Saves file to Combined directory, then changes directory back
# setwd("./Combined")
# write.csv(Benthic_final, "Benthic_joined_12_27_2016.csv", row.names=F)
# write.csv(Sediment_final, "sediment_joined_12_27_2016.csv", row.names=F)
# write.csv(Water_final, "water_joined_12_27_2016.csv", row.names=F)
# write.csv(Fish_final, "fish_joined_12_27_2016.csv", row.names=F)
# setwd("..")


################################################################################

########################
#   BINDING ALL DATA   #
########################

# Bind all data by row names
All_joined <- bind_rows(Benthic_final, Sediment_final, Water_final, Fish_final)

# Removes everything in Global Environment except what is in quotations. Just keeps things neat I suppose.
# rm(list=setdiff(ls(), "All_joined"))

# Reading in the table that Hugh and Sarah provided, of Subpopulations by Type to be kept.
keep <- read.csv("keep_subpop.csv")

# Trimming down dataset by only subpopulations to be included in the dashboard
trimmed <- left_join(keep, All_joined, by = c("Type", "Subpopulation"))


##############################
#   CALCULATE SIGNIFICANCE   #
##############################

trimmed$Significant.1v2 <- ifelse(trimmed$LCB95Pct.P.1v2*trimmed$UCB95Pct.P.1v2>0, "Yes", "No") # Column 95
trimmed$Significant.1v3 <- ifelse(trimmed$LCB95Pct.P.1v3*trimmed$UCB95Pct.P.1v3>0, "Yes", "No") # Column 96
trimmed$Significant.2v3 <- ifelse(trimmed$LCB95Pct.P.2v3*trimmed$UCB95Pct.P.2v3>0, "Yes", "No") # Column 97


#####################################
#   CALCULATE CONFIDENCE INTERVAL   #
#####################################

trimmed <- mutate(trimmed, Diff.CI.Length.P.1v2 = (UCB95Pct.P.1v2 - LCB95Pct.P.1v2)) # Column 98
trimmed <- mutate(trimmed, Diff.CI.Length.P.1v3 = (UCB95Pct.P.1v3 - LCB95Pct.P.1v3)) # Column 99
trimmed <- mutate(trimmed, Diff.CI.Length.P.2v3 = (UCB95Pct.P.2v3 - LCB95Pct.P.2v3)) # Column 100


##########################################
#   MOVING SIGNIFICANCE AND CI COLUMNS   #
##########################################

# This slots Significance and Confidence Intervals at the 'end' of the columns of data for that time period.
trimmed_sorted <- trimmed[c(1:33,95,98,34:59,96,99,60:85,97,100,86:94)]


################
#   ROUNDING   #
################

# Commented out on 2/18/2017 after a decision to round the data in the D3 dashboard code directly.
# # Creating index lists of columns. We need this later to specify which columns should be rounded differently from others.
# index <- c(1:length(trimmed_sorted))  # Full list
# whole <- c(grep(".U", names(trimmed_sorted)))  # Index list of columns containing ".U"
# sig <- c(grep("Significant", names(trimmed_sorted))) # Index list of columns containing statistical significance
# estimates <- index[-c(1:7, whole, sig)]  # Index of all other columns other than those containing ".U" and strings
# 
# # Carrying out the rounding
full <- trimmed_sorted #%>%
#   mutate_each(funs(round2(.,0)), whole) %>%
#   mutate_each(funs(round2(.,2)), estimates)


######################
#   CLEANING NAMES   #
######################
full$Source <- revalue(full$Source, c("Benthic" = "Biological"))
full$Source <- revalue(full$Source, c("Fish" = "Eco-Fish"))


##################################
#   SAVING JOINED DATA BY TYPE   #
##################################

# setwd("./Combined")
# write.csv(full, "NCCA_remodeled_02_06_2017.csv", row.names=F)


###################################
#   TRANSLATING INDICATOR NAMES   #
###################################

# reading in file with translation table. file subject to change with EPA input (there seem to be two phosphorus indicators)
indicator_join <- read.csv("indicator_names.csv")
ind_names <- c("Indicator" = "Indicator", "Source" = "Source")
indicator_join_full <- left_join(full, indicator_join, by = ind_names)
full2 <- indicator_join_full[c(1:6,101,7:100)]  # moving Indicator.Plain.Language column to right of Indicator


##############################
#   PIVOTING NATIONAL DATA   #
##############################

# adding national data to every row for creation of grey bands as national comparisons
national <- filter(full2, Subpopulation == 'National')

# Extracting Indicator, Category, Estimate.P.stat, LCB95Pct.P.stat, UCB95Pct.P.stat, LCB95Pct.U.stat, UCB95Pct.U.stat
national_2 <- national[c(6,8,94,96,97,100,101)]

# Replacing names
names(national_2) <- c("Indicator", "Condition", "National.Estimate.P", "National.LB.Ref.Range", "National.UB.Ref.Range", "National.LB.Ref.Range.U", "National.UB.Ref.Range.U")

names_join_vals <- c("Indicator" = "Indicator", "Category" = "Condition")

full3 <- left_join(full2, national_2, by = names_join_vals) %>%
  rename(., c("Category"="Condition")) # renaming Category to Condition


#############################
#   SAVING REMODELED DATA   #
#############################
# Data padding to ensure a total of 880 rows (20 subpopulations, 11 indicators, 4 condition categories)

subpop <- read.csv("keep_subpop.csv")  # 20 subpopulations
indicator <- read.csv("indicator_names.csv")  # 11 indicators
Condition <- c("Good", "Fair", "Poor", "Missing") # 4 condition categories
condition <- as.data.frame(Condition)

# Creating 'Blank' dataframe with 880 rows
df880 <- merge(merge(subpop, indicator), condition)
df880 <- arrange(df880, df880$Type, df880$Subpopulation, df880$Indicator) #sorts data by Type, then Subpopulation, then Indicator

# Matches column names in df880 with those in the combined data
join880 <- c("Subpopulation" = "Subpopulation", "Subpopulation.Plain.Language" = "Subpopulation.Plain.Language", "Aggregation" = "Aggregation", "Indicator" = "Indicator", "Condition" = "Condition", "Type" = "Type", "Source" = "Source", "Indicator.Plain.Language" = "Indicator.Plain.Language")

full4 <- left_join(df880, full3, by=join880)


####################
#  CLEANING NAMES  #
####################
full4$Type <- revalue(full4$Type, c("NEP" = "Chesapeake_Bay"))


#################################
#   CI CALCULATION (CONDITION)  #
#################################
# Create Confidence Interval (Point in Time) for Condition Estimates
full4$Confidence.Interval.Point.in.Time <- (full4$UCB95Pct.P.stat-full4$LCB95Pct.P.stat)
full4 <- full4[c(1:101, 107, 102:106)]


############################
#   COMBINING TREND DATA   #
############################
# Read in Trend data from separate script, script_trend.R

trend <- read.csv("trend_temp.csv")
remodeled <- left_join(full4, trend, by=join880)


#############################
#   SAVING REMODELED DATA   #
#############################

setwd("./Combined")
write.csv(remodeled, "NCCA_remodeled_2017_03_04.csv", row.names=F)
setwd("..")


######################
#   SPLITTING DATA   #
######################

# names(full4)
change <- full4[c(1:92)]
condition <- full4[c(1:8, 93:107)]


#########################
#   SAVING SPLIT DATA   #
#########################

setwd("./Combined")
write.csv(change, "NCCA_change_estimates_2017_03_04.csv", row.names=F)
write.csv(condition, "NCCA_condition_estimates_2017_03_04.csv", row.names=F)
setwd("..")