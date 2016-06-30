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
sudi$YearOfDeath <- as.factor(sudi$YearOfDeath)
sudi$Dep06 <- as.factor(sudi$Dep06)
sudi$MDOB <- NULL; sudi$DOB <- NULL; sudi$birth.weight <- NULL
head(sudi)
#  YearOfDeath   BW.Calc   DHBRes Dep06 Sex         EthPrio SUDI
#1        2002 3500-3999 Auckland    10   F           Maori    0
#2        2002 4000-4499 Auckland    10   F           Maori    0
#3        2002 4000-4499 Auckland     9   F           Maori    0
#4        2002 2500-2999 Auckland     3   F        European    0
#5        2002 3500-3999 Auckland     9   F Pacific Peoples    0
#6        2002 3500-3999 Auckland     5   M           Maori    0

addmargins(table(sudi$SUDI, exclude = NULL))
#     0      1   <NA>    Sum
#785567    733      0 786300
addmargins(table(sudi$YearOfDeath, sudi$SUDI, exclude = NULL))
#          0      1   <NA>    Sum
#2002  53969     54      0  54023
#2003  56074     62      0  56136
#2004  58386     62      0  58448
#2005  58504     51      0  58555
#2006  60002     71      0  60073
#2007  64833     65      0  64898
#2008  65060     65      0  65125
#2009  63049     63      0  63112
#2010  64424     62      0  64486
#2011  61904     54      0  61958
#2012  61822     40      0  61862
#2013  59479     44      0  59523
#2014  58061     40      0  58101
#<NA>      0      0      0      0
#Sum  785567    733      0 786300
addmargins(table(sudi$Dep06, sudi$SUDI, exclude = NULL))
#          0      1   <NA>    Sum
#0      1006      0      0   1006
#1     53577     17      0  53594
#2     65403     21      0  65424
#3     62616     27      0  62643
#4     64383     32      0  64415
#5     75610     41      0  75651
#6     71369     43      0  71412
#7     83747     77      0  83824
#8     96782     93      0  96875
#9     98689    137      0  98826
#10   112385    244      0 112629
#<NA>      0      1      0      1
#Sum  785567    733      0 786300
addmargins(table(sudi$Sex, sudi$SUDI, exclude = NULL))
#          0      1   <NA>    Sum
#F    382446    312      0 382758
#M    403121    421      0 403542
#<NA>      0      0      0      0
#Sum  785567    733      0 786300
addmargins(table(sudi$BW.Calc, sudi$SUDI, exclude = NULL))
#               0      1   <NA>    Sum
#<1000       3735      6      0   3741
#>=4500     21001      2      0  21003
#1000-1499   4657     14      0   4671
#1500-1999   9396     32      0   9428
#2000-2499  29114     79      0  29193
#2500-2999 105973    175      0 106148
#3000-3499 260151    245      0 260396
#3500-3999 252646    133      0 252779
#4000-4499  98167     40      0  98207
#o              0      7      0      7
#Unknown      727      0      0    727
#<NA>           0      0      0      0
#Sum       785567    733      0 786300
addmargins(table(sudi$EthPrio, sudi$SUDI, exclude = NULL))
#                     0      1   <NA>    Sum
#Asian            86286     13      0  86299
#European        370881    130      0 371011
#Maori           227142    495      0 227637
#MELAA            11776      0      0  11776
#Other             3506      3      0   3509
#Pacific Peoples  85394     91      0  85485
#Unknown            582      1      0    583
#<NA>                 0      0      0      0
#Sum             785567    733      0 786300
##
##
addmargins(table(sudi$DHBRes, sudi$SUDI, exclude = NULL))
#                        0      1   <NA>    Sum
#Auckland            82791     45      0  82836
#Bay of Plenty       36938     37      0  36975
#Canterbury          80068     47      0  80115
#Capital & Coast         0     27      0     27
#Capital and Coast   49765      0      0  49765
#Counties Manukau   108148    141      0 108289
#Hawke's Bay         29114     29      0  29143
#Hutt                26622     40      0  26662
#Lakes               20531     30      0  20561
#MidCentral          28750     30      0  28780
#Nelson Marlborough  20713     10      0  20723
#Northland           28809     56      0  28865
#Otago               20274     13      0  20287
#South Canterbury     7964      3      0   7967
#Southern            10367      0      0  10367
#Southland           15300     16      0  15316
#Tairawhiti           9862     20      0   9882
#Taranaki            19549     25      0  19574
#Unknown               956      0      0    956
#Waikato             68579     80      0  68659
#Wairarapa            6622      6      0   6628
#Waitemata           97443     57      0  97500
#West Coast           5151      2      0   5153
#Whanganui           11251     19      0  11270
#<NA>                    0      0      0      0
#Sum                785567    733      0 786300
########################################
#
# DEP
#
levels(sudi$Dep06)
#[1] "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
levels(sudi$Dep06)  <- list("01-08" = c("1","2","3","4","5","6","7","8"),
                            "09-10" = c("9","10"),
                            "Unknown" = "0")
#sudi  <-  sudi[sudi$Dep06  != "Unknown",]
#sudi  <- droplevels(sudi)
levels(sudi$Dep06)
#[1] "01-08" "09-10"
addmargins(table(sudi$Dep06, sudi$SUDI, exclude = NULL))
#           0      1
#01-08 573487    351
#09-10 211074    381
##
### DEP DONE
##
###################################
#
# ETH
#
levels(sudi$EthPrio)
#[1] "Asian"           "European"        "Maori"           "MELAA"           "Other"           "Pacific Peoples" "Unknown"
levels(sudi$EthPrio)  <- list("Maori" = "Maori",
                        "European or other" = c("European", "Asian", "MELAA", "Other"),
                        "Pacific" = "Pacific Peoples",
                        "Unknown" = "Unknown")
#sudi  <-  sudi[sudi$EthPrio  != "Unknown",]
#sudi  <- droplevels(sudi)
levels(sudi$EthPrio)
#[1] "Maori"             "European or other" "Pacific"
addmargins(table(sudi$EthPrio, sudi$SUDI, exclude = NULL))
#                       0      1
#Maori             227142    495
#European or other 472449    146
#Pacific            85394     91
##
### ETH DONE
##
##
##################################
##
## DHB
##
levels(sudi$DHBRes)
#[1]  "Auckland"           "Bay of Plenty"      "Canterbury"         "Capital & Coast"    "Capital and Coast"
#[6]  "Counties Manukau"   "Hawke's Bay"        "Hutt"               "Lakes"              "MidCentral"
#[11] "Nelson Marlborough" "Northland"          "Otago"              "South Canterbury"   "Southern"
#[16] "Southland"          "Tairawhiti"         "Taranaki"           "Waikato"            "Wairarapa"
#[21] "Waitemata"          "West Coast"         "Whanganui"
levels(sudi$DHBRes) <- list("Nothern" = c("Auckland","Waitemata","Northland","Counties Manukau"),
                            "Midland" = c("Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki"),
                            "Central" = c("Hawke's Bay","MidCentral","Whanganui","Capital & Coast", "Capital and Coast",
                                          "Hutt","Wairarapa"),
                            "Southern"= c("Nelson Marlborough","West Coast","Canterbury","South Canterbury","Southern",
                                          "Otago","Southland"),
                            "Unknown" = "Unknown")
#sudi  <-  sudi[sudi$DHBRes  != "Unknown",]
#sudi  <- droplevels(sudi)
levels(sudi$DHBRes)
#[1] "Nothern"  "Midland"  "Central"  "Southern"
addmargins(table(sudi$DHBRes, sudi$SUDI, exclude = NULL))
#              0      1
#Nothern  317168    299
#Midland  155451    192
#Central  152117    151
#Southern 159825     90
##
### DHB DONE
##
##
##################################
##
# deal with missing data
row.has.na <- apply(sudi, 1, function(x){any(is.na(x))}); sum(row.has.na);foo <- sudi[!row.has.na,]; rm(row.has.na)
##
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
#
s1 <- glm(SUDI ~ YearOfDeath + BW.Calc + DHBRes + Dep06 + Sex + EthPrio + magecat, data = sudi, family = "binomial")
