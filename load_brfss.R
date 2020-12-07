# Econ B2000, Statistics and Introduction to Econometrics
# Kevin R Foster, the City College of New York, CUNY
# July 2015
# This file preps BRFSS 2013 data, downloaded from http://www.cdc.gov/brfss/annual_data/annual_2013.html on June 17 2015; codebook is there too
# I know, everything is all caps so it's like some grannie did all the coding
# at bottom are variable descriptions

# this is how I loaded the BRFSS data but you don't have to - use next program

getwd()
setwd("C:\\Users\\Kevin\\Documents\\R") # or whatevs
# I ran this on home computer not laptop since file was so big

library(foreign)
dat1 <- read.xport("LLCP2013.XPT")
summary(dat1)

dat2 <- subset(dat1,select = c(X_STATE, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, 
                               MEDCOST, CHECKUP1, SLEPTIM1, BPHIGH4, TOLDHI2, 
                               ASTHMA3, VETERAN3, MARITAL, CHILDREN, EDUCA, EMPLOY1, 
                               INCOME2, RENTHOM1, SEX, SEATBELT, FLUSHOT6, HIVTST6, 
                               PDIABTST, MEDICARE, CARERCVD, MSCODE, X_MRACE1, 
                               X_HISPANC, HTM4, WTKG3, X_BMI5CAT, X_CHLDCNT, 
                               X_RFBING5, X_RFDRHV4, X_FRTLT1, X_VEGLT1, X_TOTINDA, 
                               X_PACAT1, X_HCVU651, X_SMOKER3))

summary(dat2)
save(dat2,file = "brfss2013.Rdata")
# later 
# rm(list = ls(all = TRUE))
# load("brfss2013.Rdata")

attach(dat2)

library(plyr)

X_STATE <- as.factor(X_STATE)
levels_orig <- levels(X_STATE) 
levels_n <- read.csv("X_STATE_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(X_STATE) <- levels_new$New_Level
# damn if I were more patient I'd write this into a function...

is.na(GENHLTH) <- which((GENHLTH == 7) | (GENHLTH == 9)) 
GENHLTH <- as.factor(GENHLTH)
levels_orig <- levels(GENHLTH) 
levels_n <- read.csv("GENHLTH_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(GENHLTH) <- levels_new$New_Level

is.na(PHYSHLTH) <- which((PHYSHLTH == 77) | (PHYSHLTH == 88) | (PHYSHLTH == 99)) 

is.na(MENTHLTH) <- which((MENTHLTH == 77) | (MENTHLTH == 88) | (MENTHLTH == 99)) 

is.na(HLTHPLN1) <- which((HLTHPLN1 == 7) | (HLTHPLN1 == 9)) 
HLTHPLN1 <- as.factor(HLTHPLN1)
levels_orig <- levels(HLTHPLN1) 
levels_n <- read.csv("HLTHPLN1_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(HLTHPLN1) <- levels_new$New_Level

is.na(PERSDOC2) <- which((PERSDOC2 == 7) | (PERSDOC2 == 9)) 
PERSDOC2 <- as.factor(PERSDOC2)
levels_orig <- levels(PERSDOC2) 
levels_n <- read.csv("PERSDOC2_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(PERSDOC2) <- levels_new$New_Level

is.na(X_HCVU651) <- which((X_HCVU651 == 9))
X_HCVU651 <- as.factor(X_HCVU651)
levels(X_HCVU651) <- c("Yes","No")

is.na(MEDCOST) <- which((MEDCOST == 7) | (MEDCOST == 9) )
MEDCOST <- as.factor(MEDCOST)
levels(MEDCOST) <- c("Yes","No")

is.na(CHECKUP1) <- which((CHECKUP1 == 7) | (CHECKUP1 == 9)) 
CHECKUP1 <- as.factor(CHECKUP1)
levels_orig <- levels(CHECKUP1) 
levels_n <- read.csv("CHECKUP1_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(CHECKUP1) <- levels_new$New_Level

is.na(SLEPTIM1) <- which((SLEPTIM1 == 77) | (SLEPTIM1 == 88) | (SLEPTIM1 == 99)) 

is.na(BPHIGH4) <- which((BPHIGH4 == 7) | (BPHIGH4 == 9)) 
BPHIGH4 <- as.factor(BPHIGH4)
levels_orig <- levels(BPHIGH4) 
levels_n <- read.csv("BPHIGH4_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(BPHIGH4) <- levels_new$New_Level

is.na(TOLDHI2) <- which((TOLDHI2 == 7) | (TOLDHI2 == 9) )
TOLDHI2 <- as.factor(TOLDHI2)
levels(TOLDHI2) <- c("Yes","No")

is.na(ASTHMA3) <- which((ASTHMA3 == 7) | (ASTHMA3 == 9) )
ASTHMA3 <- as.factor(ASTHMA3)
levels(ASTHMA3) <- c("Yes","No")

is.na(VETERAN3) <- which((VETERAN3 == 7) | (VETERAN3 == 9) )
VETERAN3 <- as.factor(VETERAN3)
levels(VETERAN3) <- c("Yes","No")

is.na(MARITAL) <- which((MARITAL == 9)) 
MARITAL <- as.factor(MARITAL)
levels_orig <- levels(MARITAL) 
levels_n <- read.csv("MARITAL_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(MARITAL) <- levels_new$New_Level

is.na(CHILDREN) <- which((CHILDREN == 88) | (CHILDREN == 99) )

is.na(EDUCA) <- which((EDUCA == 9)) 
EDUCA <- as.factor(EDUCA)
levels_orig <- levels(EDUCA) 
levels_n <- read.csv("EDUCA_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(EDUCA) <- levels_new$New_Level

is.na(EMPLOY1) <- which((EMPLOY1 == 9)) 
EMPLOY1 <- as.factor(EMPLOY1)
levels_orig <- levels(EMPLOY1) 
levels_n <- read.csv("EMPLOY1_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(EMPLOY1) <- levels_new$New_Level

is.na(INCOME2) <- which((INCOME2 == 77) | (INCOME2 == 99) )
INCOME2 <- as.factor(INCOME2)
levels_orig <- levels(INCOME2) 
levels_n <- read.csv("INCOME2_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(INCOME2) <- levels_new$New_Level

is.na(RENTHOM1) <- which((RENTHOM1 == 7) | (RENTHOM1 == 9) )
RENTHOM1 <- as.factor(RENTHOM1)
levels_orig <- levels(RENTHOM1) 
levels_n <- read.csv("RENTHOM1_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(RENTHOM1) <- levels_new$New_Level

SEX <- as.factor(SEX)
levels(SEX) <- c("Male","Female")

is.na(SEATBELT) <- which((SEATBELT == 7) | (SEATBELT == 9) )
SEATBELT <- as.factor(SEATBELT)
levels_orig <- levels(SEATBELT) 
levels_n <- read.csv("SEATBELT_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(SEATBELT) <- levels_new$New_Level

is.na(FLUSHOT6) <- which((FLUSHOT6 == 7) | (FLUSHOT6 == 9) )
FLUSHOT6 <- as.factor(FLUSHOT6)
levels(FLUSHOT6) <- c("Yes","No")

is.na(HIVTST6) <- which((HIVTST6 == 7) | (HIVTST6 == 9) )
HIVTST6 <- as.factor(HIVTST6)
levels(HIVTST6) <- c("Yes","No")

is.na(PDIABTST) <- which((PDIABTST == 7) | (PDIABTST == 9) )
PDIABTST <- as.factor(PDIABTST)
levels(PDIABTST) <- c("Yes","No")

is.na(MEDICARE) <- which((MEDICARE == 7) | (MEDICARE == 9) )
MEDICARE <- as.factor(MEDICARE)
levels(MEDICARE) <- c("Yes","No")

is.na(CARERCVD) <- which((CARERCVD == 7) | (CARERCVD == 8) | (CARERCVD == 9))
CARERCVD <- as.factor(CARERCVD)
levels_orig <- levels(CARERCVD) 
levels_n <- read.csv("CARERCVD_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(CARERCVD) <- levels_new$New_Level

MSCODE <- as.factor(MSCODE)
levels_orig <- levels(MSCODE) 
levels_n <- read.csv("MSCODE_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(MSCODE) <- levels_new$New_Level

is.na(X_MRACE1) <- which((X_MRACE1 == 77) | (X_MRACE1 == 99) )
X_MRACE1 <- as.factor(X_MRACE1)
levels_orig <- levels(X_MRACE1) 
levels_n <- read.csv("X_MRACE1_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(X_MRACE1) <- levels_new$New_Level

is.na(X_HISPANC) <- which((X_HISPANC == 7) | (X_HISPANC == 9) )
X_HISPANC <- as.factor(X_HISPANC)
levels(X_HISPANC) <- c("Yes","No")

WTKG3 <- WTKG3/100

X_BMI5CAT <- as.factor(X_BMI5CAT)
levels_orig <- levels(X_BMI5CAT) 
levels_n <- read.csv("X_BMI5CAT_levels.csv")
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(X_BMI5CAT) <- levels_new$New_Level

is.na(X_CHLDCNT) <- which((X_CHLDCNT == 9))
X_CHLDCNT <- (X_CHLDCNT - 1)

is.na(X_RFBING5) <- which((X_RFBING5 == 9))
X_RFBING5 <- as.factor(X_RFBING5)
levels(X_RFBING5) <- c("No","Yes")

is.na(X_RFDRHV4) <- which((X_RFDRHV4 == 9))
X_RFDRHV4 <- as.factor(X_RFDRHV4)
levels(X_RFDRHV4) <- c("No","Yes")

is.na(X_FRTLT1) <- which((X_FRTLT1 == 9))
X_FRTLT1 <- as.factor(X_FRTLT1)
levels(X_FRTLT1) <- c("Yes","No")

is.na(X_VEGLT1) <- which((X_VEGLT1 == 9))
X_VEGLT1 <- as.factor(X_VEGLT1)
levels(X_VEGLT1) <- c("Yes","No")

is.na(X_TOTINDA) <- which((X_TOTINDA == 9))
X_TOTINDA <- as.factor(X_TOTINDA)
levels(X_TOTINDA) <- c("Yes","No")

is.na(X_PACAT1) <- which((X_PACAT1 == 9))
X_PACAT1 <- as.factor(X_PACAT1)
levels(X_PACAT1) <- c("Highly Active","Active","Insufficiently Active","Inactive")

is.na(X_SMOKER3) <- which((X_SMOKER3 == 9))
X_SMOKER3 <- as.factor(X_SMOKER3)
levels(X_SMOKER3) <- c("Now smokes daily","Now smokes some","Former smoker","Never smoked")

dat3 <- data.frame(X_STATE, GENHLTH, PHYSHLTH, MENTHLTH, HLTHPLN1, PERSDOC2, 
                   MEDCOST, CHECKUP1, SLEPTIM1, BPHIGH4, TOLDHI2, 
                   ASTHMA3, VETERAN3, MARITAL, CHILDREN, EDUCA, EMPLOY1, 
                   INCOME2, RENTHOM1, SEX, SEATBELT, FLUSHOT6, HIVTST6, 
                   PDIABTST, MEDICARE, CARERCVD, MSCODE, X_MRACE1, 
                   X_HISPANC, HTM4, WTKG3, X_BMI5CAT, X_CHLDCNT, 
                   X_RFBING5, X_RFDRHV4, X_FRTLT1, X_VEGLT1, X_TOTINDA, 
                   X_PACAT1, X_HCVU651, X_SMOKER3)
summary(dat3)
save(dat3, file = "BRFSS2013_a.RData")

detach()

# ---------------------------
# Variable descriptions
# ---------------------------
# X_STATE state of residence
# GENHLTH "Would you say that in general your health is:"
# PHYSHLTH, "Now thinking about your physical health, which includes physical illness and injury, for how many days during the
# past 30 days was your physical health not good?" 
# MENTHLTH, "Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?"  
# HLTHPLN1 "Do you have any kind of health care coverage, including health insurance, prepaid plans such as HMOs, or government plans such as Medicare, or Indian Health Service?"
# PERSDOC2, "Do you have one person you think of as your personal doctor or health care provider?"
# X_HCVU651, "Have health care coverage (18-64 years old)"
# MEDCOST, "Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?" 
# CHECKUP1, "About how long has it been since you last visited a doctor for a routine checkup?"
# SLEPTIM1, "On average, how many hours of sleep do you get in a 24-hour period?" 
# BPHIGH4, "Have you EVER been told by a doctor, nurse or other health professional that you have high blood pressure?"
# TOLDHI2, "Have you EVER been told by a doctor, nurse or other health professional that your blood cholesterol is high?" 
# ASTHMA3, "(Ever told) you had asthma?" 
# VETERAN3, "Have you ever served on active duty in the United States Armed Forces, either in the regular military or in a National Guard or military reserve unit?" 
# MARITAL, "Marital Status"
# CHILDREN, "How many children less than 18 years of age live in your household?" 
# EDUCA, "What is the highest grade or year of school you completed?"  
# EMPLOY1, "Employment status"
# INCOME2, "your annual household income from all sources"
# RENTHOM1, "Do you own or rent your home?" 
# SEX, "Sex M/F"
# SEATBELT,"How often do you use seat belts when you drive or ride in a car?"
# FLUSHOT6, "During the past 12 months, have you had either a flu shot or a flu vaccine" 
# HIVTST6, "Have you ever been tested for HIV?" 
# PDIABTST,"Have you had a test for high blood sugar or diabetes within the past three years?" 
# MEDICARE, "Covered by Medicare"
# CARERCVD, "In general, how satisfied are you with the health care you received?"
# MSCODE, "Metropolitan Status Code"
# X_MRACE1, "Race"
# X_HISPANC, "Hispanic"
# HTM4 "Reported height in centimeters"
# WTKG3 "Reported weight in kilograms (2 implied decimal places)"
# X_BMI5CAT, "Four categories of Body Mass Index (BMI)"
# X_CHLDCNT, "Number of children in household" 
# X_RFBING5 "binge drinker"
# X_RFDRHV4 "heavy drinker" 
# X_FRTLT1, "consume fruit more than 1 per day"
# X_VEGLT1, "consume veg more than 1 per day"
# X_TOTINDA, "adults report exercise during past 30 days"
# X_PACAT1, "physical activity categories"
# X_SMOKER3 "current/past smoking"





