##########################################################################################################
# BUSINESS UNDERSTANDING:

# The Company XYZ has maintained their employees data that consists of generat data related to the employee age,
# department, education, income, experience, location and so on.... and also has their manager survery and employee survey details.

# PROBLEM STATEMENT:

# Every year 15% of employees leave the company, which need to be replaced. This sort of attrition may be because of their own choice or 
# they might got fired by the company. The company believes this might be because of narrow timelines due to the delay in the projects!
# or may be in recruiting new talent to keep the deparment sizeable.

# GOAL:

# The goal is to provide a model that shows the "PROBABILITY OF ATTRITION" using logistic regression. From this model, it would be clear
# that certain factor are responsible for an employee to leave the company. So, that the company can take necessary steps in to retain 
# an employee based on their need in requirement.
###########################################################################################################

# BUSINESS UNDERSTANDING
# DATA UNDERSTANDING
# DATA PREPARATION & EDA
# a. Data Cleaning
# b. Convert Categorical Variables to numeric variables
# c. Create Derived metrics
# d. Standardise the data
# MODEL BUILDING 
# MODEL EVALUATION

###########################################################################################################

# Required Packages
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
install.packages("caTools")
install.packages("magrittr")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("woe")
install.packages("ggplot2")


library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(GGally)
library(caTools)
library(Hmisc)
library(plyr)
library(dplyr)
library(stringr)
library(cowplot)
library(lubridate)
library(tidyr)
library(lubridate)
library(woe)
library(ggplot2)


# ------------------------------------------ Loading Data --------------------------------------------------

general <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", header = TRUE,stringsAsFactors = F)
out_time <- read.csv("out_time.csv",header = TRUE, stringsAsFactors = F)

# See the structure of the above data frames.
str(general) # 4410 obs. of  24 variables
# describe(general) 
str(employee_survey) # 4410 obs. of  4 variables
str(manager_survey)  # 4410 obs. of  3 variables
str(in_time) # 4410 obs of 262 variables
str(out_time) # 4410 obs of 262 variables

# Check duplicate rows
nrow(general) ==  length(unique(general$EmployeeID))
# EmployeeID is the primary key in general dataframe
nrow(employee_survey) ==  length(unique(employee_survey$EmployeeID))
# EmployeeID is the primary key in employee_survey dataframe
nrow(manager_survey) ==  length(unique(manager_survey$EmployeeID))
# EmployeeID is the primary key in manager_survey dataframe
nrow(in_time) ==  length(unique(in_time$X))
# x is the primary key in in_time dataframe
nrow(out_time) ==  length(unique(out_time$X))
# x is the primary key in out_time dataframe

# Checking if EmployeeID is identical across datasets
setdiff(general$EmployeeID,employee_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general$EmployeeID,manager_survey$EmployeeID) # Identical EmployeeID across these datasets

# Merging data
master_data <- merge(general, employee_survey, by="EmployeeID", all=F)
master_data <-merge(master_data, manager_survey,  by="EmployeeID", all = F)


# --------------------------------------------- Derived Metrics ---------------------------------------

# Analyse the intime and outtime data frames and derive some meaningful insgihts.

# If we observe the columns with solely of NA values in "intime" and "outtime" dataframes, there is certainly a pattern, 

# Get only the Columns with NA's and analyse their pattern
get_NA_columns <- function(vectordf){
  timedf <- data.frame()
  colnamevector <- c()
  for(i in 1:ncol(vectordf)){
    if(sum(is.na(vectordf[,i])) == length(vectordf[,i])){
      colname <- colnames(vectordf[i])
      colnamevector <- c(colnamevector,colname)
    }
  }
  timedf <- vectordf[,which(names(vectordf) %in% colnamevector)]
  return(timedf)
}

# intime Data Frame only with NA columns to analyse the patern that we have it in hand.
in_time_NA_Columns <- get_NA_columns(in_time)
# outime Data Frame only with NA columns to analyse the patern that we have it in hand.
out_time_NA_Columns <- get_NA_columns(out_time)

# we took the Date columns with NA's and compare across the "Indian Holiday Calendar" ...

# 01/01/2015 is a holiday for NEW YEAR
# 01/14/2015 is a holiday for PONGAL
# 01/26/2015 is a holiday for REPUBLIC DAY
# .....
# .....
# 12/25/2015 is a holiday for Christmas

# So, Remove all the NA's value columns from in-time and analyse them clearly till the end
in_time <- in_time[, -which(colnames(in_time) %in% colnames(in_time_NA_Columns))]

# So, Remove all the NA's value columns from out-time and analyse them clearly till the end
out_time <- out_time[, -which(colnames(out_time) %in% colnames(out_time_NA_Columns))]

# Removing the "EmployeeID" before we convert the intime and outime data frames to standard R Date Format.
in_time<- in_time[,-1]
out_time <- out_time[,-1]

##################################################################################################################
# Calculating Average WWorking Hours for each employee 
##################################################################################################################

# Parsing DateTime  to convert into R format
in_time1 <- as.data.frame(lapply(in_time, function(x) parse_date_time(x, "%Y-%m-%d %H:%M:%S")))
out_time1 <- as.data.frame(lapply(out_time, function(x) parse_date_time(x, "%Y-%m-%d %H:%M:%S")))

# Working hours of each day
workingHours <- as.data.frame(sapply(out_time1 - in_time1, function(x) round(x,2)))

# Average working hours
str(workingHours)
avgWorkingHrs <- as.data.frame(apply(workingHours, 1, mean, na.rm=T))
colnames(avgWorkingHrs)[1] <- "AvgWorkingHrs"
avgWorkingHrs$AvgWorkingHrs <- round(avgWorkingHrs$AvgWorkingHrs,2)

# Append EmployeeID to avgWorkingHrs DataFrame
avgWorkingHrs$EmployeeID <- 1:4410

# Checking if EmployeeID is identical across datasets
setdiff(avgWorkingHrs$EmployeeID,master_data$EmployeeID) # Identical EmployeeID across these datasets

# Final Merged Dataset
master_data <- merge(master_data, avgWorkingHrs, by="EmployeeID", all=F)

# Understanding the structure of the collated file
str(master_data) #4410 obs. of  30 variables
describe(master_data)

View(master_data) # master file

rm(list = setdiff(ls(),"master_data")) # flushing un-neccessary objects from the environment 

##################################################################################################################
#creating new field to understand whether employee is doing extended working hours or not
#As the standard working hours is 8 from data, those who have an average working hours more 8 can be considered 
#for doing extended working hours
##################################################################################################################
master_data$extended_working_hrs <- ifelse(master_data$AvgWorkingHrs > 8,"Yes","No")

##################################################################################################################
#JobTenure: People who have tendency of changing jobs frequently are tend to leave the company within short
#periods. This can be found out from Jobtenure field.
##################################################################################################################
master_data$JobTenure <- ifelse(master_data$NumCompaniesWorked!=0,
                                round(master_data$TotalWorkingYears/master_data$NumCompaniesWorked,2),0)

# -------------------------------------- Exploratory Data Analysis ---------------------------------------


# Since there are few columns with values as constants, for example EmployeeCount as 1, Over18 as Y, StandardHours as 8. We can drop these columns.
master_data <- master_data[,-which(colnames(master_data) %in% c("EmployeeCount","Over18","StandardHours"))]

str(master_data)
# For example, take Education, it was a numeric factor variable, lets replace them with their respective levels.
# So, replace the levels of these factor variables with their respective Label's as shown below: Education, EnvironmentSatisfaction, JobInvolvement, JobSatisfaction,
# PerformanceRating, WorkLifeBalance

master_data$Education <- as.factor(master_data$Education)
levels(master_data$Education)
master_data$Education <- revalue(master_data$Education, c("1"="Below College","2"="College","3"="Bachelor","4"="Master","5"="Doctor"))
master_data$EnvironmentSatisfaction <- as.factor(master_data$EnvironmentSatisfaction)
levels(master_data$EnvironmentSatisfaction)
master_data$EnvironmentSatisfaction <- revalue(master_data$EnvironmentSatisfaction, c("1"="Low","2"="Medium","3"="High","4"="Very High"))
master_data$JobInvolvement <- as.factor(master_data$JobInvolvement)
levels(master_data$JobInvolvement)
master_data$JobInvolvement <- revalue(master_data$JobInvolvement, c("1"="Low","2"="Medium","3"="High","4"="Very High"))
master_data$JobSatisfaction <- as.factor(master_data$JobSatisfaction)
levels(master_data$JobSatisfaction)
master_data$JobSatisfaction <- revalue(master_data$JobSatisfaction, c("1"="Low","2"="Medium","3"="High","4"="Very High"))
master_data$PerformanceRating <- as.factor(master_data$PerformanceRating)
levels(master_data$PerformanceRating)
master_data$PerformanceRating <- revalue(master_data$PerformanceRating, c("3"="Ecellent","4"="Outstanding"))
master_data$WorkLifeBalance <- as.factor(master_data$WorkLifeBalance)
levels(master_data$WorkLifeBalance)
master_data$WorkLifeBalance <- revalue(master_data$WorkLifeBalance, c("1"="Bad","2"="Good","3"="Better","4"="Best"))


data_over_all <- master_data %>% group_by(Attrition) %>% summarise(count1 = n())
data_over_all$count1 <- 100 * data_over_all$count1/nrow(master_data)
data_over_all$count2 <- str_c(round(data_over_all$count1,2),"%")
plot_attrition <- ggplot(data_over_all,aes(x=Attrition,y=count1,fill=Attrition)) + geom_bar(stat="identity") +
  geom_text(aes(label=count2),vjust = 2) + ylab("% of Employees") + ggtitle("Percentage of Employee Attrition")
plot_attrition

#Conclusion : About 16.12% of employees left the company in the year 2015
#Plotting in figures
data_over_all2     <- master_data %>% group_by(Attrition) %>% summarise(counta = n())
plot_attrition_fig <- ggplot(data_over_all2,aes(Attrition,y=counta,fill=Attrition))+geom_bar(stat="identity") +
  geom_text(aes(label=counta),vjust = 2) + ylab("No of Employees") + ggtitle("Employee Attrition Count")
plot_attrition_fig

##################################################################################################################
# Barcharts for categorical features with stacked base_data information
#################################################################################################################
bar_theme1<- theme(axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 1))

plot_grid(ggplot(master_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,align = "v")

plot_grid(ggplot(master_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
        ggplot(master_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#################################################################################################################
#Conclusions from the chart:
#Those who travelled rarely tend to leave company more.
#Attrition is less for HR dept. as low proportion of HR employees are there in an organization. Also Research & 
#Development Dept. have high attrition 
#In correlation with Department, HR have less attrition if considering education field
#Attrition rate is more among male employees as we can assume compared to female employees they tend to have more
#dependants which results in seeking a good package
#Assuming Joblevel 1 denotes employees who have just entered the role/field and 5 denoting highly experienced or 
#experts of the field. From the plot we understand that as the level increases people are less tend to leave 
#company
#Attrition is more among singles and less among divorcees
##################################################################################################################

plot_grid(ggplot(master_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x= PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

##################################################################################################################
#Conclusions from the chart:
#StockOptionLevel - More employees quit in lower levels
#EnvironmentSatisfaction - More employees quit where the statisfaction level is 1 compared to other levels. But not
#showing any significant trend
#JobSatisfaction - Higher attrition among Job Satisfaction level 1.
#WorkLifeBalance - if considering the proportion of data, attrition is more in level 1 which is as expected.
#JobInvolvement  - Attrition is more with involvement level 3
#PerformanceRating - compared to rating 4, rating 3 has high attrition amount.
##################################################################################################################
# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(master_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(master_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 
#No outliers in the PercentSalaryHikea and DistanceFromHome

# few outliers in  MonthlyIncome, YearsAtCompany, YearsSinceLastPromotion, AvgWorkingHrs, NumCompaniesWorked,
# StockOptionLeve, TotalWorkingYears, TrainingTimesLastYear and YearsWithCurrManager

# For YearsAtCompany there is jump between 92% to 93%. So, cap all the values above 17 to 17
quantile(master_data$YearsAtCompany,seq(0,1,.01),na.rm = T)

master_data$YearsAtCompany[which(master_data$YearsAtCompany>17)] <- 17
plot_grid(ggplot(master_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Similarly for YearsSinceLastPromotion there is jump between 92% to 93%. So, cap all the values above 9 to 9
quantile(master_data$YearsSinceLastPromotion,seq(0,1,.01),na.rm = T) 

master_data$YearsSinceLastPromotion[which(master_data$YearsSinceLastPromotion>7)] <- 7
plot_grid(ggplot(master_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# For MonthlyIncome there is jump between 90% to 91%. So, cap all the values above 137756.0 to 137756.0 
quantile(master_data$MonthlyIncome,seq(0,1,.01),na.rm = T) 

master_data$MonthlyIncome[which(master_data$MonthlyIncome>137756.0)] <- 137756.0
plot_grid(ggplot(master_data, aes(MonthlyIncome))+ geom_histogram(binwidth =30),
          ggplot(master_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 


# For YearsWithCurrManager there is jump between 99% to 100%. So, cap all the values above 14 to 14 
quantile(master_data$YearsWithCurrManager,seq(0,1,.01),na.rm = T) 

master_data$YearsWithCurrManager[which(master_data$YearsWithCurrManager>14)] <- 14
plot_grid(ggplot(master_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


# For AvgWorkingHrs there is jump between 99% to 100%. So, cap all the values above 10.90 to 10.90 
quantile(master_data$AvgWorkingHrs,seq(0,1,.01),na.rm = T) 
master_data$AvgWorkingHrs[which(master_data$AvgWorkingHrs>10.90)] <- 10.90
plot_grid(ggplot(master_data, aes(AvgWorkingHrs))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=AvgWorkingHrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# For NumCompaniesWorked it was showing that the percentile values from 96-to 100% were outliers. So, cap all the values above 8 to 8
quantile(master_data$NumCompaniesWorked,seq(0,1,.01),na.rm = T) 
master_data$NumCompaniesWorked[which(master_data$NumCompaniesWorked>8)] <- 8

plot_grid(ggplot(master_data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# For StockOptionLevel it was showing that the percentile values from 95-to 100% were outliers. So, cap all the values above 2 to 2
quantile(master_data$StockOptionLevel,seq(0,1,.01),na.rm = T) 
master_data$StockOptionLevel[which(master_data$StockOptionLevel>2)] <- 2

plot_grid(ggplot(master_data, aes(StockOptionLevel))+ geom_histogram(binwidth = 2),
          ggplot(master_data, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# For TotalWorkingYears there is a sudden raise from 94% to 95%. So, cap all the values above 26 to 26
quantile(master_data$TotalWorkingYears,seq(0,1,.01),na.rm = T) 
master_data$TotalWorkingYears[which(master_data$TotalWorkingYears>26)] <- 26

plot_grid(ggplot(master_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# For TrainingTimesLastYear there were outliers on both the ends, i.e on left and right of the scale.
# We will replace thefrom 96-to 100% were outliers. So, cap all the values above 8 to 8
quantile(master_data$TrainingTimesLastYear,seq(0,1,.01),na.rm = T) 
master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear==0)] <- 1
master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear>5)] <- 5

plot_grid(ggplot(master_data, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 20),
          ggplot(master_data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Replace them with WOE values,

# woe(master_data,"TrainingTimesLastYear",TRUE,"Attrition",4,Bad = "Yes",Good = "No")
# master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear<=2)] <- -0.056
# master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear>2 & master_data$TrainingTimesLastYear<=3)] <- -0.107
# master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear>3 & master_data$TrainingTimesLastYear<=5)] <- 0.175


data_over_all3     <- master_data %>% group_by(Attrition, extended_working_hrs ) %>% summarise(countb = n())
plot_extended_wrkhrs <- ggplot(data_over_all3,aes(extended_working_hrs,y=countb,fill=extended_working_hrs))+
  geom_bar(stat="identity") + facet_grid(~Attrition) + geom_text(aes(label=countb),vjust = 2)
plot_extended_wrkhrs

# Conclusion: There is a relatively higher amount of people working in extended working hours, in the group of 
# those who left the company


plot_tenure <- ggplot(master_data,aes(JobTenure))+geom_density()+facet_grid(~Attrition)
plot_tenure

#Conclusion: This clearly shows the trend as the number of years per job is less for those who quit.

# Boxplots of numeric variables relative to Attrition 
plot_grid(ggplot(master_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none")+bar_theme1,
          ggplot(master_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Correlation between numeric variables
ggpairs(master_data[, c("MonthlyIncome", "PercentSalaryHike", "YearsSinceLastPromotion")])


# ------------------------------------- Data Preparation ---------------------------------------------------

# Checking Duplicate rows
duplicates <- master_data[duplicated(master_data),]
duplicates
# 0 observations

# Converting Target variable from categorical to numerical
master_data$Attrition <- ifelse(master_data$Attrition=="Yes",1,0)

# Checking Missing Values
sapply(master_data,function(x) sum(is.na(x)))
# 19 NAs in NumCompaniesWorked, 9 in TotalWorkingYears, 25 in EnvironmentSatisfaction, 20 in JobSatisfaction, 38 in WorkLifeBalance
# Total 111 NA values

# Let us view them and see if the missing values have any pattern or is it just by data entry error.
View(subset(master_data, is.na(NumCompaniesWorked)))
View(subset(master_data, is.na(TotalWorkingYears)))
View(subset(master_data, is.na(EnvironmentSatisfaction)))
View(subset(master_data, is.na(JobSatisfaction)))
View(subset(master_data, is.na(WorkLifeBalance)))

# we can ignore these records since as total 9+25+20+19+38+26=137; 137/4410, i.e 3.1% of records. Remove these records from the analysis
master_data <- master_data[!is.na(master_data$TotalWorkingYears),]
master_data <- master_data[!is.na(master_data$EnvironmentSatisfaction),]
master_data <- master_data[!is.na(master_data$JobSatisfaction),]
master_data <- master_data[!is.na(master_data$NumCompaniesWorked),]
master_data <- master_data[!is.na(master_data$WorkLifeBalance),]
master_data <- master_data[!is.na(master_data$JobTenure),]

sum(is.na(master_data))
# 0 NA

# Feature standardisation
# Normalising continuous features 
master_data$Age <- scale(master_data$Age)
master_data$DistanceFromHome <- scale(master_data$DistanceFromHome)
master_data$JobLevel <- scale(master_data$JobLevel)
master_data$MonthlyIncome <- scale(master_data$MonthlyIncome)
master_data$NumCompaniesWorked <- scale(master_data$NumCompaniesWorked)
master_data$PercentSalaryHike <- scale(master_data$PercentSalaryHike)
master_data$StockOptionLevel <- scale(master_data$StockOptionLevel)
master_data$TotalWorkingYears <- scale(master_data$TotalWorkingYears)
master_data$TrainingTimesLastYear <- scale(master_data$TrainingTimesLastYear)
master_data$YearsAtCompany <- scale(master_data$YearsAtCompany)
master_data$YearsSinceLastPromotion <- scale(master_data$YearsSinceLastPromotion)
master_data$YearsWithCurrManager <- scale(master_data$YearsWithCurrManager)
master_data$AvgWorkingHrs <- scale(master_data$AvgWorkingHrs)

# Creating a data frame of NUMERIC VARIABLES
str(master_data)
numeric_variables <-  c("EmployeeID", "Age", "DistanceFromHome", "JobLevel","MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike",
                        "StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "AvgWorkingHrs", "Attrition", "JobTenure")
master_data_numeric <-master_data[,numeric_variables]
str(master_data_numeric)

# Creating a data frame of CATEGORICAL VARIABLES
master_data_chr <- master_data[, -which(colnames(master_data) %in% numeric_variables)]

# converting categorical variables to factor
master_fact<- data.frame(sapply(master_data_chr, function(x) factor(x)))
str(master_fact)


# Creating dummy Variables for the above factor variables
dummies<- data.frame(sapply(master_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_fact))[,-1]))

final_data <- cbind(master_data_numeric,dummies)
final_data <- final_data[,-1]

##################################################################################################

############### *********** Splitting the Data into TRAIN & TEST ************ ###################

set.seed(100)

indices <- sample.split(final_data$Attrition,SplitRatio = 0.70)

train <- final_data[indices,]

test <- final_data[!(indices),]

############################# ********* MODEL BUILDING ************* ##############################

# Logistic Regression Analysis

# Initial Model, build a generalized linear regression model using glm() function and see what it gives us,
model_1 <- glm(Attrition~.,data = train, family = "binomial") # AIC: 2077.7 ... with 53 coefficients, 
# Null deviance: 2661.4  Residual deviance: 1971.7
summary(model_1)

# We have a total of 53 variables considered into the model 
#Now let's run the code. 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 
step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can identify the removed variables as they have a plus(+) sign at the final step of stepAIC.

model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + JobTenure + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Education.xDoctor + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

# Let us look at the summary of the model
summary(model_2)

# Apply the VIF function to check the multicollinearity of the independent variables and 
# remove the variables with VIF>2/3, in the order of their insignificance.

vif(model_2)

# Most of the variables have their VIF<2, except few for education and Business fields, which are highly significant when comes to p-values
# So, lets see if there are variables with vif>2 and having p-values less that 0.05
#  MaritalStatus.xMarried has vIF (2.127097) and p-value of 0.089990, so lets remove and see if this has an effect on model.

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + JobTenure + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Education.xDoctor + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

# check the summary for their significance.
summary(model_3)
# Check the Correlation
vif(model_3)

# For a jobTenure variable the VIF is around "2.010365" and its p-value is 0.046167

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Education.xDoctor + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_4)
vif(model_4)

# As we can see that their is no change in few of the variables, EducationField.xLife.Sciences ,EducationField.xMarketing 
# EducationField.xMedical, EducationField.xOther, EducationField.xTechnical.Degree.

# Lets check their correlation among them.

ggpairs(train[,c("EducationField.xLife.Sciences" ,"EducationField.xMarketing", "EducationField.xMedical", "EducationField.xOther",
                 "EducationField.xTechnical.Degree")])
# As we can see from the abvoe graph there is a high correlation of 60% among EducationField.xLife.Sciences and EducationField.xMedical

# Check the correlation between the business variables also, BusinessTravel.xTravel_Frequently, BusinessTravel.xTravel_Rarely
ggpairs(train[,c("BusinessTravel.xTravel_Frequently", "BusinessTravel.xTravel_Rarely")])

# As we can see from the above graph there is 75% correlation among the business variables.

# And for the worklifeBalance, WorkLifeBalance.xBetter, WorkLifeBalance.xGood 
ggpairs(train[,c("WorkLifeBalance.xBetter", "WorkLifeBalance.xGood")])
# From the above graph they have around 70% of correlation.

# Lets remove BusinessTravel.xTravel_Rarely and see if this impacts the model...

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_4)
vif(model_4)
# Now we can see that BusinessTravel.xTravel_Frequently has came down below 2 as per vIF is concerned.

# Since EducationField.xLife.Sciences and EducationField.xMedical are highly collinear and 
# Remove EducationField.xLife.Sciences
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_5)
vif(model_5)

# Remove WorkLifeBalance.xBetter which has around 3.55 vif and highly significant, but had 70% colliniarity with WorkLifeBalance.xGood
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBest + WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_6)
vif(model_6)

# As everything has the vif below 2, except the totalworkingyears with 2.3, which is highly significant.

# Now based on insignificance we will remove the variables.

# Remove WorkLifeBalance.xBest

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_7)
vif(model_7)

# Remove EducationField.xMarketing

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + 
                 EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_8)
vif(model_8)

# Remove EducationField.xMedical

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + JobInvolvement.xVery.High + 
                 extended_working_hrs, family = "binomial", data = train)

summary(model_9)
vif(model_9)

# Remove WorkLifeBalance.xGood

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_10)
vif(model_10)


# Remove EducationField.xOther

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +  
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_11)
vif(model_11)

# Remove EducationField.xTechnical.Degree

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +  
                  JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_12)
vif(model_12)

# Remove JobInvolvement.xVery.High

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +  
                  JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)
summary(model_13)
vif(model_13)

# Remove JobRole.xHuman.Resources

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +  
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_14)
vif(model_14)

# Remove Education.xDoctor

model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_15)
vif(model_15)

# Remove TotalWorkingYears

model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_16)
vif(model_16)

# Remove EnvironmentSatisfaction.xVery.High

model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_17)
vif(model_17)

# Remove JobRole.xManager

model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_18)
vif(model_18)

# Remove JobRole.xResearch.Director

model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManufacturing.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +  
                  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)


summary(model_19)
vif(model_19)

# Remove JobRole.xSales.Executive

model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_20)
vif(model_20)

# Remove TrainingTimesLastYear

model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +  JobInvolvement.xVery.High + 
                  extended_working_hrs, family = "binomial", data = train)

summary(model_21)
vif(model_21)

# Remove JobInvolvement.xVery.High

model_22 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.xTravel_Frequently +   
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +   
                 extended_working_hrs, family = "binomial", data = train)
summary(model_22)
vif(model_22)

summary(model_22)
vif(model_22)

final_model <- model_22

### Model Evaluation
# However, every model has to be checked in order to understand how well it is doing. This process, as you may recall, is called model evaluation.
# In a model evaluation process, you take your model that has been prepared with the help of training data, and you use it to make predictions for the testing data.
# After that, since you have the actual values for the testing data too, you can compare the actual values with the predicted values, 
# and that will help you understand how well your model is doing.

### Test Data ####

########### ******** predicted "probabilities of Attrition" 1 for test data ************* #############

# If we won't give type="response" the predict function will give us the log(odds), but we need the probabilities.
# Discard the customerID - newdata = test[,-1]

test_pred = predict(final_model, type = "response", newdata = test)

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Now we have these probabilities, we can make a decision on "ATTRITION" by making a "threshold" of 0.5.
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
# Using "table" function we can get a 2-by-2 table matrix which tells the discripencies of actual and predicted model
# 1053, persons has not attritioned, 52 have attritioned On the otherhand we look at the off-set of 28 says that they actually won't attrition,but the model says they attritioned
# 157, the actual says they attritioned and the model says they won't attrition
table(test_actual_attrition,test_pred_attrition)

# We got the accuracy as 85% (1051+53)/(1051+53+156+30)=1105/1290, but this won't tell you everything we will see what are all other variables for evaluation.
#######################################################################

# We know whaat sensitivity is?; which indicates how many of the actual Attritions the model was able to predict accurately, 
# is equal to only 53/209 (25%) for the model used here, even though the overall accuracy of the model is around 85%.

# So, what do we do now? Do we discard this model? lets' make some modifications? 

# Now try with different threshold of 0.40 and try to predict.
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

# Over here positive="Yes" tells us that the positive scenario in our model is yes.
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  1021  133
# Yes   60   76
# 
# Accuracy : 0.8504          
# 95% CI : (0.8297, 0.8694)
# No Information Rate : 0.838           
# P-Value [Acc > NIR] : 0.1201          
# 
# Kappa : 0.3587          
# Mcnemar's Test P-Value : 2.187e-07       
#                                           
#             Sensitivity : 0.36364         
#             Specificity : 0.94450         
#          Pos Pred Value : 0.55882         
#          Neg Pred Value : 0.88475         
#              Prevalence : 0.16202         
#          Detection Rate : 0.05891         
#    Detection Prevalence : 0.10543         
#       Balanced Accuracy : 0.65407         
#                                           
#        'Positive' Class : Yes      

#######################################################################

#########################################################################################

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 
# Given a matrix or data.frame x, t returns the transpose of x.

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(test_pred)
# Creating cutoff values from the summary of test_pred, which gives us 0.001482 to 0.849734 for plotting and initiallizing a matrix of 100 X 3.

# Sequence is going to generate 100 records between the lenght of .01 to 0.08
s = seq(.01,.84,length=100)
# matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,dimnames = NULL)
# So here you are asking to create a 100 rows with 3 columns with zeros as its data
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


OUT

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

abs(OUT[,1]-OUT[,2])
which(abs(OUT[,1]-OUT[,2])<0.01)
OUT[,1]-OUT[,2]
testout <-abs((OUT[,1]-OUT[,2])<0.01)
testout
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.1692929 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1692929, "Yes", "No"))
test_cutoff_attrition
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  No Yes
# No  803  52
# Yes 278 157
# 
# Accuracy : 0.7442          
# 95% CI : (0.7194, 0.7678)
# No Information Rate : 0.838           
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.344           
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.7512          
# Specificity : 0.7428          
# Pos Pred Value : 0.3609          
# Neg Pred Value : 0.9392          
# Prevalence : 0.1620          
# Detection Rate : 0.1217          
# Detection Prevalence : 0.3372          
# Balanced Accuracy : 0.7470          
# 
# 'Positive' Class : Yes 


acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# Accuracy 
# 0.744186 
# > sens
# Sensitivity 
# 0.7511962 
# > spec
# Specificity 
# 0.7428307 

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

install.packages("ROCR")
library(ROCR)
# on testing  data we will take both cutoff and actual attrition data and standardize their format.
pred_object_test <- prediction(test_cutoff_attrition, test_actual_attrition)
pred_object_test
# All kinds of evaluations are done using performance function
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
performance_measures_test
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# KS-statistic is around 50% for our model.
# [1] 0.4940269
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
install.packages("dplyr")
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile


# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1   129        75      75  35.9    3.59
# 2      2   129        50     125  59.8    2.99
# 3      3   129        28     153  73.2    2.44
# 4      4   129        17     170  81.3    2.03
# 5      5   129         6     176  84.2    1.68
# 6      6   129         6     182  87.1    1.45
# 7      7   129         5     187  89.5    1.28
# 8      8   129         7     194  92.8    1.16
# 9      9   129        10     204  97.6    1.08
# 10     10   129         5     209 100      1   


