---
title: "HDMA PROJECT"
author: "Mirajul Fahim"
date: "12/19/2020"
output:
  pdf_document: default
  html_document: default
---
https://rpubs.com/mirajulfahim8/705924

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
getwd()
setwd("/Users/mirajulfahim/Downloads/ecob2000_lecture1/Econometrics Project")

library(foreign)
x<-read.csv("/Users/mirajulfahim/Downloads/hmda_2017_ny_all-records_labels.csv")



dat2 <- x[-c(1,4,5,7,9,11,13,16,18,21:23,26,28,30,32:40,42:50,52,54,57,59:64,66,68:71,78)]

dat3<- subset(dat2, select =c(property_type_name, loan_purpose_name, owner_occupancy_name ,loan_amount_000s, preapproval_name, action_taken_name, msamd_name, county_name, applicant_ethnicity_name, co_applicant_ethnicity_name, applicant_race_name_1,applicant_race_name_1, applicant_sex_name, co_applicant_sex_name, applicant_income_000s, purchaser_type_name, denial_reason_name_1 , hoepa_status_name, lien_status_name ,population, minority_population ,hud_median_family_income , number_of_owner_occupied_units, number_of_1_to_4_family_units))

dfna <- dat3[ , c("property_type_name", "loan_purpose_name", "owner_occupancy_name" ,"loan_amount_000s", "preapproval_name", "action_taken_name", "msamd_name", "county_name", "applicant_ethnicity_name", "co_applicant_ethnicity_name", "applicant_race_name_1","applicant_race_name_1", "applicant_sex_name", "co_applicant_sex_name", "applicant_income_000s", "purchaser_type_name", "denial_reason_name_1" , "hoepa_status_name", "lien_status_name" ,"population", "minority_population" ,"hud_median_family_income" , "number_of_owner_occupied_units", "number_of_1_to_4_family_units")] 
dfna <- dat3[complete.cases(dat3), ] # Omit NAs by columns
  

save(dfna,file="HDMA2017.Rdata")

```
```{r}
attach(dfna)

library(VIM)
VIM::aggr(dfna)
VIM::aggr(dat3)

```

```{r echo=TRUE}
# Now we have a look at $applicant_income_000s column of the dfna dataset with boxplot
boxplot(dfna$applicant_income_000s)
# You can get the actual values of the outliers with this
boxplot(dfna$applicant_income_000s)$out
# Now you can assign the outliers values into a vector
outliers<-boxplot(dfna$applicant_income_000s,plot=FALSE)$out
# Check the results
print(outliers)
# First you need find in which rows the outliers are
dfna[which(dfna$applicant_income_000s %in% outliers),]```


```{r echo=TRUE}

# Now you can remove the rows containing the outliers, one possible option is:
dfna <- dfna[-which(dfna$applicant_income_000s %in% outliers),]
#If you check now with boxplot, you will notice that those pesky outliers are gone
boxplot(dfna$applicant_income_000s)

summary(dfna$applicant_income_000s)

```

```{r}
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
```{r}
library(knitr)

approvedprop<-dfna$Loan_Approved
approvedtable<-table(approvedprop)
loanapprovedtable<-table(dfna$Loan_Approved,dfna$Loan_Not_Approved)
colnames(loanapprovedtable)<- c("Approved","Not Approved")
loantable<- addmargins(loanapprovedtable)
t1<-addmargins(approvedtable)
t2<-prop.table(loanapprovedtable)
kables(list(kable(t1,align="l",col.name=c("Approved","Total")),kable(t2,col.names = c("Approved","Proportion"))))
```
```{r}

library(stargazer)

y<-lm(Loan_Approved~White_Hispanic+Black_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y,type="text")


y2<-lm(Loan_Approved~White_Hispanic+White_Not_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y2,type="text")

y3<-lm(Loan_Approved~Black_Hispanic+Black_Not_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y3,type="text")

y4<-lm(Loan_Approved~Black_Not_Hispanic+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y4,type="text")

y5<-lm(Loan_Approved~Male+Female+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y5,type="text")

y6<- lm(Loan_Approved~Male2+Female2+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y6,type="text")

y7<-lm(Loan_Approved~Male+Female+ I(Male* applicant_income_000s)+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y7,type="text")

```

```{r}
library(car)
linearHypothesis(y,matchCoefs(y,"county_name"))
linearHypothesis(y,c("Black_Hispanic=0","county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))
linearHypothesis(y,c("Black_Hispanic")) 


linearHypothesis(y2,matchCoefs(y2,"county_name"))
linearHypothesis(y2,c("county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))


linearHypothesis(y3,matchCoefs(y3,"county_name"))
linearHypothesis(y3,c("Black_Hispanic=0","county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))
linearHypothesis(y3,c("Black_Hispanic=0")) 

linearHypothesis(y4,matchCoefs(y4,"county_name"))
linearHypothesis(y4,c("county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))

linearHypothesis(y5,matchCoefs(y5,"county_name"))
linearHypothesis(y5,c("county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))


linearHypothesis(y6,matchCoefs(y6,"county_name"))
linearHypothesis(y6,c("county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))

linearHypothesis(y7,matchCoefs(y7,"county_name"))
linearHypothesis(y7,c("county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFranklin County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))

```
```{r}
use_varb <- (dfna$minority_population >= 51) 
use_varb2<-(dfna$minority_population <= 51)
majorityneighborhoods <- subset(dfna,use_varb)
minorityneighborhoods<-subset(dfna,use_varb2)

x<-lm(Loan_Approved~minority_population,data=minorityneighborhoods)
x2<-lm(Loan_Approved~minority_population,data=majorityneighborhoods)


library(stargazer)
stargazer(x,type="text")
stargazer(x2,type="text")
```

```{r}
require(standardize)
set.seed(654321)
NN<- length(dfna$Loan_Approved)
NN
restrict_1 <-as.logical(runif(NN)<0.20)
summary(restrict_1)
dat_train<-subset(dfna,restrict_1)
dat_test<- subset(dfna,!restrict_1)
sobj<- standardize(Loan_Approved~Black+ Asian + American_Indian_or_Alaska_Native + White + Male + Female + applicant_income_000s +loan_purpose_name +county_name + minority_population,dat_train,family=binomial)
s_dat_test<-predict(sobj,dat_test)
summary(s_dat_test)
```
```{r}
linear <- lm(sobj$formula, data = sobj$data)
summary(linear)

pred_vals_OLS <- suppressWarnings(predict(linear, s_dat_test))
pred_model_OLS1 <- (pred_vals_OLS > 0.45)
pred1OLStable <- table(pred = pred_model_OLS1, true = dat_test$Loan_Approved)
pred1OLStable
goodolspred <- sum((prop.table(pred1OLStable)[1,1])+(prop.table(pred1OLStable)[2,2]))
goodolspred
```
```{r}
logit <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(logit)
pred_vals <- suppressWarnings(predict(logit, s_dat_test, type = "response"))
pred_model_logit1 <- (pred_vals > 0.5)
pred1Logtable <- table(pred = pred_model_logit1, true = dat_test$Loan_Approved)
pred1Logtable
goodlogpred <- sum((prop.table(pred1Logtable)[1,1])+(prop.table(pred1Logtable)[2,2]))
goodlogpred
```
```{r}
probit <- glm(sobj$formula, family = binomial (link='probit'), data = sobj$data)
summary(probit)
pred_vals2 <- suppressWarnings(predict(probit, s_dat_test, type = "response"))
pred_model_probit1 <- (pred_vals2 > 0.5)
pred1Logtable2 <- table(pred = pred_model_probit1, true = dat_test$Loan_Approved)
pred1Logtable2
goodlogpred2 <- sum((prop.table(pred1Logtable2)[1,1])+(prop.table(pred1Logtable2)[2,2]))
goodlogpred2
```

