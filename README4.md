---
title: "Untitled"
author: "Mirajul Fahim"
date: "12/19/2020"
output:
  html_document:
    df_print: paged
---
https://rpubs.com/mirajulfahim8/705963

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = FALSE)
getwd()
setwd("/Users/mirajulfahim/Downloads/ecob2000_lecture1/Econometrics Project")

library(foreign)
x<-read.csv("/Users/mirajulfahim/Downloads/ecob2000_lecture1/Econometrics Project/hmda_2017_ny_all-records_labels.csv")

```

```{r include=FALSE}
dat2 <- x[-c(1,4,5,7,9,11,13,16,18,21:23,26,28,30,32:40,42:50,52,54,57,59:64,66,68:71,78)]

dat3<- subset(dat2, select =c(property_type_name, loan_purpose_name, owner_occupancy_name ,loan_amount_000s, preapproval_name, action_taken_name, msamd_name, county_name, applicant_ethnicity_name, co_applicant_ethnicity_name, applicant_race_name_1,applicant_race_name_1, applicant_sex_name, co_applicant_sex_name, applicant_income_000s, purchaser_type_name, denial_reason_name_1 , hoepa_status_name, lien_status_name ,population, minority_population ,hud_median_family_income , number_of_owner_occupied_units, number_of_1_to_4_family_units))

dfna <- dat3[ , c("property_type_name", "loan_purpose_name", "owner_occupancy_name" ,"loan_amount_000s", "preapproval_name", "action_taken_name", "msamd_name", "county_name", "applicant_ethnicity_name", "co_applicant_ethnicity_name", "applicant_race_name_1","applicant_race_name_1", "applicant_sex_name", "co_applicant_sex_name", "applicant_income_000s", "purchaser_type_name", "denial_reason_name_1" , "hoepa_status_name", "lien_status_name" ,"population", "minority_population" ,"hud_median_family_income" , "number_of_owner_occupied_units", "number_of_1_to_4_family_units")] 
dfna <- dat3[complete.cases(dat3), ] # Omit NAs by columns
 
```

```{r include=FALSE}
attach(dfna)

```

```{r include=FALSE}
# Now we have a look at $applicant_income_000s column of the dfna dataset with boxplot
boxplot(dfna$applicant_income_000s)
# You can get the actual values of the outliers with this
boxplot(dfna$applicant_income_000s)$out
# Now you can assign the outliers values into a vector
outliers<-boxplot(dfna$applicant_income_000s,plot=FALSE)$out
# Check the results
#print(outliers)
# First you need find in which rows the outliers are
# Now you can remove the rows containing the outliers, one possible option is:
dfna <- dfna[-which(dfna$applicant_income_000s %in% outliers),]
#If you check now with boxplot, you will notice that those pesky outliers are gone


```

```{r include=FALSE}
dfna$Hispanic_or_Latino <- ifelse(dfna$applicant_ethnicity_name == 'Hispanic or Latino', 1, 0)
dfna$Not_Hispanic_or_Latino<- ifelse(dfna$applicant_ethnicity_name == 'Not Hispanic or Latino', 1, 0)

dfna$American_Indian_or_Alaska_Native <-ifelse(dfna$applicant_race_name_1 =='American Indian or Alaska Native',1,0)
dfna$Asian <-ifelse(dfna$applicant_race_name_1 =='Asian',1,0)
dfna$Black <-ifelse(dfna$applicant_race_name_1 =='Black or African American',1,0)
dfna$Native_Hawaiian_or_P.Islander <-ifelse(dfna$applicant_race_name_1 =='Native Hawaiian or Other Pacific Islander',1,0)
dfna$White<-ifelse(dfna$applicant_race_name_1 =='White',1,0)

dfna$Male<-ifelse(dfna$applicant_sex_name =='Male',1,0)
dfna$Female<-ifelse(dfna$applicant_sex_name =='Female',1,0)


dfna$Loan_Approved<-ifelse(dfna$action_taken_name =='Loan originated',1,0)
dfna$Loan_Not_Approved<-ifelse(dfna$action_taken_name =='Application denied by financial institution',1,0)
dfna$Preapproval_request<-ifelse(dfna$action_taken_name =='Preapproval request denied by financial institution',1,0)

dfna$Collateral<-ifelse(dfna$denial_reason_name_1 =='Collateral',1,0)
dfna$Debt_to_income_ratio<-ifelse(dfna$denial_reason_name_1 =='Debt-to-income ratio',1,0)
dfna$Credit_History<-ifelse(dfna$denial_reason_name_1=='Credit history ',1,0)
dfna$Credit_app_incomplete<-ifelse(dfna$denial_reason_name_1 == 'Credit application incomplete',1,0)                             

dfna$Hispanic_or_Latino2 <- ifelse(dfna$co_applicant_ethnicity_name == 'Hispanic or Latino', 1, 0)
dfna$Not_Hispanic_or_Latino2<- ifelse(dfna$co_applicant_ethnicity_name == 'Not Hispanic or Latino', 1, 0)
dfna$Male2<-ifelse(dfna$co_applicant_sex_name =='Male',1,0)
dfna$Female2<-ifelse(dfna$co_applicant_sex_name =='Female',1,0)

dfna$Merged_Race_Ethnicity <- paste(dfna$applicant_race_name_1,dfna$applicant_ethnicity_name)
categories <- unique(dfna$Merged_Race_Ethnicity) 
numberOfCategories <- length(categories)
table(categories)

dfna$White_Hispanic <- ifelse(dfna$Merged_Race_Ethnicity=='White Hispanic or Latino',1,0 )
dfna$White_Not_Hispanic <- ifelse(dfna$Merged_Race_Ethnicity=='White Not Hispanic or Latino',1,0)
dfna$Black_Not_Hispanic <-ifelse(dfna$Merged_Race_Ethnicity=='Black or African American Not Hispanic or Latino',1,0)

dfna$Black_Hispanic<-ifelse(dfna$Merged_Race_Ethnicity=='Black or African American Hispanic or Latino',1,0)
```


```{r echo=TRUE}

library(stargazer)

use_varb <- (dfna$minority_population >= 51) 
use_varb2<-(dfna$minority_population <= 51)
majorityneighborhoods <- subset(dfna,use_varb)
minorityneighborhoods<-subset(dfna,use_varb2)

x<-lm(Loan_Approved~minority_population,data=minorityneighborhoods)
x2<-lm(Loan_Approved~minority_population,data=majorityneighborhoods)
stargazer(x)
stargazer(x2)
plot(x)
plot(x2)
```

