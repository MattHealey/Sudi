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
addmargins(table(sudi$SUDI, exclude = NULL))
#     0      1   <NA>    Sum 
#785567    733      0 786300 
# check un-categorised maternal age
addmargins(table(sudi$mage, sudi$SUDI, exclude = NULL))
#          0      1   <NA>    Sum
#11        1      0      0      1
#12        5      0      0      5
#13       51      1      0     52
#14      346      1      0    347
#15     1469      4      0   1473
#16     4727     10      0   4737
#17    10123     16      0  10139
#18    15782     28      0  15810
#19    21320     50      0  21370
#20    24198     21      0  24219
#21    25881     48      0  25929
#22    28255     48      0  28303
#23    30101     42      0  30143
#24    31627     34      0  31661
#25    33389     34      0  33423
#26    35970     27      0  35997
#27    38673     28      0  38701
#28    42016     26      0  42042
#29    44883     13      0  44896
#30    46152     16      0  46168
#31    48188     19      0  48207
#32    47798     11      0  47809
#33    45252     17      0  45269
#34    42327     20      0  42347
#35    38152     16      0  38168
#36    32664     16      0  32680
#37    27024      8      0  27032
#38    21543      5      0  21548
#39    16741      8      0  16749
#40    11980      4      0  11984
#41     7994      0      0   7994
#42     5029      0      0   5029
#43     2885      2      0   2887
#44     1544      1      0   1545
#45      781      1      0    782
#46      360      0      0    360
#47      157      0      0    157
#48       86      0      0     86
#49       30      0      0     30
#50       18      0      0     18
#51       17      0      0     17
#52        5      0      0      5
#53        4      0      0      4
#54        1      0      0      1
#55        3      0      0      3
#56        2      0      0      2
#57        1      0      0      1
#64        1      0      0      1
#65        2      0      0      2
#70        1      0      0      1
#<NA>      8    158      0    166
#Sum  785567    733      0 786300
subset(sudi, is.na(sudi$mage))
head(sudi)
#  YearOfDeath        DOB   BW.Calc birth.weight   DHBRes Dep06 Sex         EthPrio       MDOB SUDI mage
#1        2002 2000-04-21 3500-3999         3500 Auckland    10   F           Maori 1968-03-29    0   32
#2        2002 2000-05-09 4000-4499         4105 Auckland    10   F           Maori 1967-01-02    0   33
#3        2002 2000-05-14 4000-4499         4050 Auckland     9   F           Maori 1972-07-30    0   27
#4        2002 2000-06-10 2500-2999         2520 Auckland     3   F        European 1962-03-23    0   38
#5        2002 2000-06-24 3500-3999         3650 Auckland     9   F Pacific Peoples 1976-09-10    0   23
#6        2002 2000-07-06 3500-3999         3970 Auckland     5   M           Maori 1983-01-03    0   17
## create factor for mage
age.cat <- function(x, lower = 0, upper, by = 5,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
addmargins(table(age.cat(sudi$mage, lower = 10,upper = 80), sudi$SUDI, exclude = NULL))
#           0      1   <NA>    Sum
#10-14    403      2      0    405
#15-19  53421    108      0  53529
#20-24 140062    193      0 140255
#25-29 194931    128      0 195059
#30-34 229717     83      0 229800
#35-39 136124     53      0 136177
#40-44  29432      7      0  29439
#45-49   1414      1      0   1415
#50-54     45      0      0     45
#55-59      6      0      0      6
#60-64      1      0      0      1
#65-69      2      0      0      2
#70-74      1      0      0      1
#75-79      0      0      0      0
#80+        0      0      0      0
#<NA>       8    158      0    166
#Sum   785567    733      0 786300
sudi$magecat <- age.cat(sudi$mage, lower = 10,upper = 80)
levels(sudi$magecat)
#[1] "10-14" "15-19" "20-24" "25-29" "30-34" "35-39" "40-44" "45-49" "50-54" "55-59" "60-64" "65-69" "70-74" "75-79" "80+"  
levels(sudi$magecat)  <- list(">20"   = c("10-14","15-19"),
                            "20-24" = "20-24",
                            "25-29" = "25-29",
                            "30-34" = "30-34",
                            "35-39" = "35-39",
                            "<40" = c("40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
sudi <- droplevels(sudi)
levels(sudi$magecat)
addmargins(table(sudi$magecat, sudi$SUDI, exclude = NULL))
#           0      1   <NA>    Sum
#>20    53824    110      0  53934
#20-24 140062    193      0 140255
#25-29 194931    128      0 195059
#30-34 229717     83      0 229800
#35-39 136124     53      0 136177
#<40    30901      8      0  30909
#<NA>       8    158      0    166
#Sum   785567    733      0 786300
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
table(sudi$Dep06, sudi$SUDI)
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
table(sudi$EthPrio, sudi$SUDI)
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
table(sudi$DHBRes, sudi$SUDI)
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
