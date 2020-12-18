# HDMA

df <- data.frame(hmda_2017_ny_all.records_labels)

attach(df)

dat2 <- df[-c(1,4,5,7,9,11,13,16,18,21:23,26,28,30,32:40,42:50,52,54,57,59:64,66,68:71,78)]
dat2
View(dat2)

detach(df)
