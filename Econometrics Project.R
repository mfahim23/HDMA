df <- data.frame(hmda_2017_ny_all.records_labels)

attach(df)

dat2 <- df[-c(1,4,5,7,9,11,13,16,18,21:23,26,28,30,32:40,42:50,52,54,57,59:64,66,68:71,78)]
dat2
View(dat2)

detach(df)

dat3<- subset(dat2, select =c(property_type_name, loan_purpose_name, owner_occupancy_name ,loan_amount_000s, preapproval_name, action_taken_name, msamd_name, county_name, applicant_ethnicity_name, co_applicant_ethnicity_name, applicant_race_name_1,applicant_race_name_1, applicant_sex_name, co_applicant_sex_name, applicant_income_000s, purchaser_type_name, denial_reason_name_1 , hoepa_status_name, lien_status_name ,population, minority_population ,hud_median_family_income , number_of_owner_occupied_units, number_of_1_to_4_family_units))

detach(dat3)


dfna <- dat3[ , c("property_type_name", "loan_purpose_name", "owner_occupancy_name" ,"loan_amount_000s", "preapproval_name", "action_taken_name", "msamd_name", "county_name", "applicant_ethnicity_name", "co_applicant_ethnicity_name", "applicant_race_name_1","applicant_race_name_1", "applicant_sex_name", "co_applicant_sex_name", "applicant_income_000s", "purchaser_type_name", "denial_reason_name_1" , "hoepa_status_name", "lien_status_name" ,"population", "minority_population" ,"hud_median_family_income" , "number_of_owner_occupied_units", "number_of_1_to_4_family_units")] 
dfna <- dat3[complete.cases(dat3), ] # Omit NAs by columns
dfna
View(dfna)

install.packages("VIM")
library(VIM)
VIM::aggr(dfna)

attach(dfna)




# Now we have a look at $applicant_income_000s column of the dfna dataset with boxplot
boxplot(dfna$applicant_income_000s)
# You can get the actual values of the outliers with this
boxplot(dfna$applicant_income_000s)$out
# Now you can assign the outliers values into a vector
outliers<-boxplot(dfna$applicant_income_000s,plot=FALSE)$out
# Check the results
print(outliers)
# First you need find in which rows the outliers are
dfna[which(dfna$applicant_income_000s %in% outliers),]
# Now you can remove the rows containing the outliers, one possible option is:
dfna <- dfna[-which(dfna$applicant_income_000s %in% outliers),]
#If you check now with boxplot, you will notice that those pesky outliers are gone
boxplot(dfna$applicant_income_000s)

summary(dfna$applicant_income_000s)




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



dfna$Appliction_approved<-ifelse(dfna$action_taken_name == 'Application approved but not accepted',1,0)                             
summary(dfna$loan_amount_000s)

install.packages("knitr")
library(knitr)

approvedprop<-dfna$Loan_Approved
approvedtable<-table(approvedprop)
loanapprovedtable<-table(dfna$Loan_Approved)
colnames(loanapprovedtable)<- c("Approved","Not Approved")
loantable<- addmargins(loanapprovedtable)
t1<-addmargins(approvedtable)
t2<-prop.table(loanapprovedtable)
kables(list(kable(t1,align="l",col.name=c("Approved","Total")),kable(t2,col.names = c("Approved","Proportion"))))



require(standardize)
set.seed(654321)
NN<- length(dfna$Loan_Approved)
NN
restrict_1 <-as.logical(runif(NN)<0.20)
summary(restrict_1)
dat_train<-subset(dfna,restrict_1)
dat_test<- subset(dfna,!restrict_1)
sobj<- standardize(Loan_Approved~Black+ Asian + American_Indian_or_Alaska_Native + White + Male + Female + Hispanic_or_Latino + applicant_income_000s +loan_purpose_name + county_name + minority_population,dat_train,family=binomial)
s_dat_test<-predict(sobj,dat_test)
summary(s_dat_test)


linear <- lm(sobj$formula, data = sobj$data)
summary(linear)

pred_vals_OLS <- suppressWarnings(predict(linear, s_dat_test))
pred_model_OLS1 <- (pred_vals_OLS > 0.45)
pred1OLStable <- table(pred = pred_model_OLS1, true = dat_test$Loan_Approved)
pred1OLStable
goodolspred <- sum((prop.table(pred1OLStable)[1,1])+(prop.table(pred1OLStable)[2,2]))


linearHypothesis(linear,matchCoefs(linear,"county_name"))
linearHypothesis(linear,c("Black=0","county_nameBroome County=0","county_nameClinton County=0"  ,"county_nameFulton County=0","county_nameHamilton County=0","county_nameOrleans County=0","county_nameSaratoga County =0","county_nameSchenectady County=0" , "county_nameSt. Lawrence County=0" ,"county_nameUlster County=0" ,"county_nameWarren County=0"))
linearHypothesis(linear,c("Black"))


install.packages("dummies")
library(dummies)

save(dfna,file = "HDMA2017_2.Rdata")


