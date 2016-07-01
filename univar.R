options(scipen = 999)
contrasts(usudi$yod)  <- contr.treatment(levels(usudi$yod), base=which(levels(usudi$yod) == "2014"))
contrasts(usudi$dhb)  <- contr.treatment(levels(usudi$dhb), base=which(levels(usudi$dhb) == "Southern"))
contrasts(usudi$eth)  <- contr.treatment(levels(usudi$eth), base=which(levels(usudi$eth) == "European or other"))
contrasts(usudi$sex)  <- contr.treatment(levels(usudi$sex), base=which(levels(usudi$sex) == "F"))
contrasts(usudi$bw)   <- contr.treatment(levels(usudi$bw), base=which(levels(usudi$bw) == "2500-2999"))
contrasts(usudi$dep)   <- contr.treatment(levels(usudit$dep), base=which(levels(usudit$dep) == "01-08"))
## Maternal age
## create factor for mage
age <- function(dob, age.day, units = "years", floor = TRUE) {
     calc.age = interval(dob, age.day) / duration(num = 1, units = units)
     if (floor) return(as.integer(floor(calc.age)))
     return(calc.age)
}
usudi$mage <- age(dob = usudi$mdob, age.day = usudi$dob)
addmargins(table(usudi$mage, usudi$sudi, exclude = NULL))
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
age.cat <- function(x, lower = 0, upper, by = 5,
                    sep = "-", above.char = "+") {

     labs <- c(paste(seq(lower, upper - by, by = by),
                     seq(lower + by - 1, upper - 1, by = by),
                     sep = sep),
               paste(upper, above.char, sep = ""))

     cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
         right = FALSE, labels = labs)
}
addmargins(table(age.cat(usudi$mage, lower = 10,upper = 80), usudi$sudi, exclude = NULL))
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
usudi$magecat <- age.cat(usudi$mage, lower = 10,upper = 80)
levels(usudi$magecat)
#[1] "10-14" "15-19" "20-24" "25-29" "30-34" "35-39" "40-44" "45-49" "50-54" "55-59" "60-64" "65-69" "70-74" "75-79" "80+"
levels(usudi$magecat)  <- list(">20"   = c("10-14","15-19"),
                              "20-24" = "20-24",
                              "25-29" = "25-29",
                              "30-34" = "30-34",
                              "35-39" = "35-39",
                              "<40" = c("40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
sudi <- droplevels(usudi)
levels(usudi$magecat)
addmargins(table(usudi$magecat, usudi$sudi, exclude = NULL))
#           0      1   <NA>    Sum
#>20    53824    110      0  53934
#20-24 140062    193      0 140255
#25-29 194931    128      0 195059
#30-34 229717     83      0 229800
#35-39 136124     53      0 136177
#<40    30901      8      0  30909
#<NA>       8    158      0    166
#Sum   785567    733      0 786300

s1 <- glm(sudi ~ yod, data = usudi, family = binomial(link = "log"))
s2 <- glm(sudi ~ bw, data = usudi, family = binomial(link = "log"))
s3 <- glm(sudi ~ eth, data = usudi, family = binomial(link = "log"))
s4 <- glm(sudi ~ sex, data = usudi, family = binomial(link = "log"))
s5 <- glm(sudi ~ dhb, data = usudi, family = binomial(link = "log"))
s6 <- glm(sudi ~ dep, data = usudi, family = binomial(link = "log"))
s7 <- glm(sudi ~ magecat, data = usudi, family = binomial(link = "log"))
s8 <- glm(sudi ~ bwr, data = usudi, family = binomial(link = "log"))
s9 <- glm(sudi ~ mage, data = usudi, family = binomial(link = "log"))
library(doBy)
orderBy(~ AIC, AIC(s1,s2,s3,s4,s5,s6,s7))

m <- s2
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)

t.test(usudi$mage[usudi$sudi != 1], sudi$mage[usudi$sudi == 1])
#Welch Two Sample t-test
#
#data:  usudi$mage[usudi$sudi != 1] and sudi$mage[usudi$sudi == 1]
#t = 15.01, df = 574.84, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  3.357628 4.368594
#sample estimates:
#  mean of x mean of y 
#29.22485  25.36174 

