options(scipen = 999)
library(lubridate)
sudi <- read.csv("Dataset for LR.csv")
str(sudi)
#'data.frame':	786300 obs. of  10 variables:
#$ YearOfDeath : int  2002 2002 2002 2002 2002 2002 2002 2002 2002 2002 ...
#$ DOB         : Factor w/ 5921 levels "0000-00-00 00:00:00",..: 603 617 622 646 658 666 668 738 745 753 ...
#$ BW.Calc     : Factor w/ 11 levels "<1000",">=4500",..: 8 9 9 6 8 8 7 6 8 8 ...
#$ birth.weight: Factor w/ 4127 levels "\"Other LBW\"",..: 2310 2940 2886 1329 2469 2800 2194 1698 2390 2310 ...
#$ DHBRes      : Factor w/ 24 levels "Auckland","Bay of Plenty",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ Dep06       : int  10 10 9 3 9 5 4 3 8 9 ...
#$ Sex         : Factor w/ 2 levels "F","M": 1 1 1 1 1 2 1 1 2 2 ...
#$ EthPrio     : Factor w/ 7 levels "Asian","European",..: 3 3 3 2 6 3 2 2 6 2 ...
#$ MDOB        : Factor w/ 14613 levels "#N/A","0000-00-00",..: 3716 3264 5300 1518 6803 9109 6701 6538 8499 1734 ...
#$ SUDI        : int  0 0 0 0 0 0 0 0 0 0 ...
#
## rename variables, fix date formats, make YearOfDeath and Dep06 factors
names(sudi)
##[1] "YearOfDeath"  "DOB"          "BW.Calc"      "birth.weight" "DHBRes"       "Dep06"        "Sex"
##[8] "EthPrio"      "MDOB"         "SUDI"
names(sudi) <- c("yod","dob","bw","bwr","dhb","dep","sex","eth","mdob","sudi"); names(sudi)
#[1] "yod" "dob"  "bw"   "bwr"  "dhb"  "dep"  "sex"  "eth"  "mdob" "sudi"
sudi$dob <- as.Date(sudi$dob); sudi$mdob <- as.Date(sudi$mdob)
sudi$yod <- as.factor(sudi$yod)
sudi$dep <- as.factor(sudi$dep)
head(sudi)
#   yod        dob        bw  bwr      dhb dep sex             eth       mdob sudi
#1 2002 2000-04-21 3500-3999 3500 Auckland  10   F           Maori 1968-03-29    0
#2 2002 2000-05-09 4000-4499 4105 Auckland  10   F           Maori 1967-01-02    0
#3 2002 2000-05-14 4000-4499 4050 Auckland   9   F           Maori 1972-07-30    0
#4 2002 2000-06-10 2500-2999 2520 Auckland   3   F        European 1962-03-23    0
#5 2002 2000-06-24 3500-3999 3650 Auckland   9   F Pacific Peoples 1976-09-10    0
#6 2002 2000-07-06 3500-3999 3970 Auckland   5   M           Maori 1983-01-03    0

# NAs?
##
# rename foo to sudi to run with NA removed set
row.has.na <- apply(sudi, 1, function(x){any(is.na(x))}); sum(row.has.na);foo <- sudi[!row.has.na,]; rm(row.has.na)
#[1] 167
##
# None in sudi$sudi
addmargins(table(sudi$sudi, exclude = NULL))
#     0      1   <NA>    Sum
#785567    733      0 786300
#
##
# None in sudi$yod
addmargins(table(sudi$yod, sudi$sudi, exclude = NULL))
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
#
##
# 1 in sudi$dep
addmargins(table(sudi$dep, sudi$sudi, exclude = NULL))
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
#
##
# None in sudi$sex
addmargins(table(sudi$sex, sudi$sudi, exclude = NULL))
#          0      1   <NA>    Sum
#F    382446    312      0 382758
#M    403121    421      0 403542
#<NA>      0      0      0      0
#Sum  785567    733      0 786300
#
##
# 7 sneaky ones in sudi$bw and levels need cleaning and ordering
addmargins(table(sudi$bw, sudi$sudi, exclude = NULL))
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
#
##
# None in sudi$eth
addmargins(table(sudi$eth, sudi$sudi, exclude = NULL))
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
#
##
# None in sudi$dhb
addmargins(table(sudi$dhb, sudi$sudi, exclude = NULL))
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

###
# Fix levels
##
# BW
levels(sudi$bw)
#[1] "<1000"     ">=4500"    "1000-1499" "1500-1999" "2000-2499" "2500-2999" "3000-3499" "3500-3999" "4000-4499"
#[10] "o"         "Unknown"
levels(sudi$bw) <- list("<1000" = "<1000",   "1000-1499" = "1000-1499",
                        "1500-1999" = "1500-1999", "2000-2499" = "2000-2499", "2500-2999" = "2500-2999",
                        "3000-3499" = "3000-3499", "3500-3999" = "3500-3999", "4000-4499" = "4000-4499",
                        ">=4500" = ">=4500", "Unknown" = c("o", "Unknown"))
sudi  <- droplevels(sudi)
levels(sudi$bw)
addmargins(table(sudi$bw, sudi$sudi, exclude = NULL))
#               0      1   <NA>    Sum
#<1000       3721      6      0   3727
#>=4500     20969      2      0  20971
#1000-1499   4651     11      0   4662
#1500-1999   9366     23      0   9389
#2000-2499  29037     61      0  29098
#2500-2999 105743    125      0 105868
#3000-3499 259611    201      0 259812
#3500-3999 252212    114      0 252326
#4000-4499  98030     30      0  98060
#Unknown      670      1      0    671
#<NA>           0      0      0      0
#Sum       784010    574      0 784584
##
# BW END
###

##
# DEP
#
levels(sudi$dep)
#[1] "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
levels(sudi$dep)  <- list("01-08" = c("1","2","3","4","5","6","7","8"),
                            "09-10" = c("9","10"),
                            "Unknown" = "0")
sudi  <- droplevels(sudi)
levels(sudi$dep)
#[1] "01-08" "09-10"
addmargins(table(sudi$dep, sudi$sudi, exclude = NULL))
#             0      1   <NA>    Sum
#01-08   573487    351      0 573838
#09-10   211074    381      0 211455
#Unknown   1006      0      0   1006
#<NA>         0      1      0      1
#Sum     785567    733      0 786300
##
# DEP DONE
###

###
# ETH
##
levels(sudi$eth)
#[1] "Asian"           "European"        "Maori"           "MELAA"           "Other"           "Pacific Peoples" "Unknown"
levels(sudi$eth)  <- list("Maori" = "Maori",
                              "European or other" = c("European", "Asian", "MELAA", "Other"),
                              "Pacific" = "Pacific Peoples",
                              "Unknown" = "Unknown")
sudi  <- droplevels(sudi)
levels(sudi$eth)
#[1] "Maori"             "European or other" "Pacific"
addmargins(table(sudi$eth, sudi$sudi, exclude = NULL))
#                       0      1   <NA>    Sum
#Maori             227142    495      0 227637
#European or other 472449    146      0 472595
#Pacific            85394     91      0  85485
#Unknown              582      1      0    583
#<NA>                   0      0      0      0
#Sum               785567    733      0 786300
##
# ETH DONE
###

###
# DHB
##
levels(sudi$dhb)
#[1]  "Auckland"           "Bay of Plenty"      "Canterbury"         "Capital & Coast"    "Capital and Coast"
#[6]  "Counties Manukau"   "Hawke's Bay"        "Hutt"               "Lakes"              "MidCentral"
#[11] "Nelson Marlborough" "Northland"          "Otago"              "South Canterbury"   "Southern"
#[16] "Southland"          "Tairawhiti"         "Taranaki"           "Waikato"            "Wairarapa"
#[21] "Waitemata"          "West Coast"         "Whanganui"
levels(sudi$dhb) <- list("Nothern" = c("Auckland","Waitemata","Northland","Counties Manukau"),
                            "Midland" = c("Waikato","Lakes","Bay of Plenty","Tairawhiti","Taranaki"),
                            "Central" = c("Hawke's Bay","MidCentral","Whanganui","Capital & Coast", "Capital and Coast",
                                          "Hutt","Wairarapa"),
                            "Southern"= c("Nelson Marlborough","West Coast","Canterbury","South Canterbury","Southern",
                                          "Otago","Southland"),
                            "Unknown" = "Unknown")
sudi  <- droplevels(sudi)
levels(sudi$dhb)
#[1] "Nothern"  "Midland"  "Central"  "Southern"
addmargins(table(sudi$dhb, sudi$sudi, exclude = NULL))
#              0      1   <NA>    Sum
#Nothern  317191    299      0 317490
#Midland  155459    192      0 155651
#Central  152124    151      0 152275
#Southern 159837     91      0 159928
#Unknown     956      0      0    956
#<NA>          0      0      0      0
#Sum      785567    733      0 786300
##
# DHB DONE
####

##
# make a copy at this point for use in uni var analyses. Uncomment line below to create.
sudi.v1.copy  <- sudi
##

##
# Trim SUDI df and remove vars not needed in multivar LR
sudi <- sudi[,c(1,3,5,6,7,8,10)]
##

##
# Deal with missing data, ie remove them!
row.has.na <- apply(sudi, 1, function(x){any(is.na(x))}); sum(row.has.na);sudi <- sudi[!row.has.na,]; rm(row.has.na,foo)
##
# Get rid of unknowns
##
sudi  <-  sudi[sudi$dep  != "Unknown",]
sudi  <-  sudi[sudi$dhb  != "Unknown",]
sudi  <-  sudi[sudi$eth  != "Unknown",]
sudi  <-  sudi[sudi$bw  != "Unknown",]
sudi <- droplevels(sudi)
