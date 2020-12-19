---
title: "HDMA FINAL PROJECT"
author: "Mirajul Fahim"
date: "12/18/2020"
output:
  word_document: default
  html_document: default
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
getwd()
setwd("/Users/mirajulfahim/Downloads/ecob2000_lecture1/Econometrics Project")

library(foreign)
x<-read.csv("/Users/mirajulfahim/Downloads/hmda_2017_ny_all-records_labels.csv")

```

```{r}
attach(dfna)

library(VIM)
VIM::aggr(dfna)
VIM::aggr(dat3)

```



```{r}

y<-lm(Loan_Approved~White_Hispanic+Black_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
library(stargazer)
stargazer(y,type="text")
plot(y)

y2<-lm(Loan_Approved~White_Hispanic+White_Not_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y2,type="text")
plot(y2)

y3<-lm(Loan_Approved~Black_Hispanic+Black_Not_Hispanic +applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y3,type="text")
plot(y3)



y4<-lm(Loan_Approved~Black_Not_Hispanic+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y4,type="text")
plot(y4)


y5<-lm(Loan_Approved~Male+Female+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y5,type="text")
plot(y5)

y6<- lm(Loan_Approved~Male2+Female2+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y6,type="text")
plot(y6)

y7<-lm(Loan_Approved~Male+Female+ I(Male* applicant_income_000s)+applicant_income_000s + loan_purpose_name + county_name + minority_population,data=dfna)
stargazer(y7,type="text")
plot(y7)

```

