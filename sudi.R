library(lubridate)
sudi <- read.csv("Dataset for LR.csv")
names(sudi)
##[1] "YearOfDeath"  "DOB"          "BW.Calc"      "birth.weight" "DHBRes"       "Dep06"        "Sex"
##[8] "EthPrio"      "MDOB"         "SUDI"
str(sudi)
#'data.frame':	786300 obs. of  10 variables:
#     $ YearOfDeath : int  2002 2002 2002 2002 2002 2002 2002 2002 2002 2002 ...
#$ DOB         : Factor w/ 6598 levels "0000-00-00 00:00:00",..: 3271 6473 1044 276 3891 5916 6312 4709 5767 930 ...
#$ BW.Calc     : Factor w/ 11 levels "<1000",">=4500",..: 8 9 9 6 8 8 7 6 8 8 ...
#$ birth.weight: Factor w/ 4127 levels "\"Other LBW\"",..: 2310 2940 2886 1329 2469 2800 2194 1698 2390 2310 ...
#$ DHBRes      : Factor w/ 24 levels "Auckland","Bay of Plenty",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ Dep06       : int  10 10 9 3 9 5 4 3 8 9 ...
#$ Sex         : Factor w/ 2 levels "F","M": 1 1 1 1 1 2 1 1 2 2 ...
#$ EthPrio     : Factor w/ 7 levels "Asian","European",..: 3 3 3 2 6 3 2 2 6 2 ...
#$ MDOB        : Factor w/ 14613 levels "#N/A","0000-00-00",..: 10153 5276 11233 7283 822 10566 11555 6202 10724 8526 ...
#$ SUDI        : int  0 0 0 0 0 0 0 0 0 0 ...
sudi$DOB <- as.Date(sudi$DOB)
sudi$MDOB <- as.Date(sudi$MDOB)
sudi$year <- as.factor(sudi$year)
sudi$Dep06 <- as.factor(sudi$Dep06)
head(sudi)
#  YearOfDeath           DOB   BW.Calc birth.weight   DHBRes Dep06 Sex         EthPrio     MDOB SUDI
#1        2002 21/04/00 0:00 3500-3999         3500 Auckland    10   F           Maori 29/03/68    0
#2        2002  9/05/00 0:00 4000-4499         4105 Auckland    10   F           Maori  2/01/67    0
#3        2002 14/05/00 0:00 4000-4499         4050 Auckland     9   F           Maori 30/07/72    0
#4        2002 10/06/00 0:00 2500-2999         2520 Auckland     3   F        European 23/03/62    0
#5        2002 24/06/00 0:00 3500-3999         3650 Auckland     9   F Pacific Peoples 10/09/76    0
#6        2002  6/07/00 0:00 3500-3999         3970 Auckland     5   M           Maori  3/01/83    0
age <- function(dob, age.day, units = "years", floor = TRUE) {
     calc.age = interval(dob, age.day) / duration(num = 1, units = units)
     if (floor) return(as.integer(floor(calc.age)))
     return(calc.age)
}
sudi$mage <- age(dob = sudi$MDOB, age.day = sudi$DOB)
head(sudi)
#  YearOfDeath        DOB   BW.Calc birth.weight   DHBRes Dep06 Sex         EthPrio       MDOB SUDI mage
#1        2002 2000-04-21 3500-3999         3500 Auckland    10   F           Maori 1968-03-29    0   32
#2        2002 2000-05-09 4000-4499         4105 Auckland    10   F           Maori 1967-01-02    0   33
#3        2002 2000-05-14 4000-4499         4050 Auckland     9   F           Maori 1972-07-30    0   27
#4        2002 2000-06-10 2500-2999         2520 Auckland     3   F        European 1962-03-23    0   38
#5        2002 2000-06-24 3500-3999         3650 Auckland     9   F Pacific Peoples 1976-09-10    0   23
#6        2002 2000-07-06 3500-3999         3970 Auckland     5   M           Maori 1983-01-03    0   17
str(sudi)
table(sudi$SUDI)
#     0      1
#785567    733
t.test(sudi$mage[sudi$SUDI != 1], sudi$mage[sudi$SUDI == 1])
#Welch Two Sample t-test
#
#data:  sudi$mage[sudi$SUDI != 1] and sudi$mage[sudi$SUDI == 1]
#t = 15.01, df = 574.84, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#     3.357628 4.368594
#sample estimates:
#     mean of x mean of y
#      29.22485  25.36174
