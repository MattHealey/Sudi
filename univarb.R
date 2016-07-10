options(scipen = 999)
#source("1 - clean.R")
library(multcomp)
library(effects)
library(lubridate)
library(margins)
#row.has.na <- apply(usudi, 1, function(x){any(is.na(x))}); sum(row.has.na);magesudi <- usudi[!row.has.na,]; rm(row.has.na)
usudi$bwr[usudi$bwr==0] <- NA
contrasts(usudi$yod)  <- contr.treatment(levels(usudi$yod), base=which(levels(usudi$yod) == "2014"))
contrasts(usudi$dhb)  <- contr.treatment(levels(usudi$dhb), base=which(levels(usudi$dhb) == "Southern"))
contrasts(usudi$eth)  <- contr.treatment(levels(usudi$eth), base=which(levels(usudi$eth) == "European_or_other"))
contrasts(usudi$sex)  <- contr.treatment(levels(usudi$sex), base=which(levels(usudi$sex) == "F"))
contrasts(usudi$dep)   <- contr.treatment(levels(usudi$dep), base=which(levels(usudi$dep) == "01_08"))

## Maternal age
## create factor for mage
age <- function(dob, age.day, units = "years", floor = TRUE) {
     calc.age = interval(dob, age.day) / duration(num = 1, units = units)
     if (floor) return(as.integer(floor(calc.age)))
     return(calc.age)
}
usudi$mage     <- age(dob = usudi$mdob, age.day = usudi$dob)
#magesudi$mage  <- age(dob = magesudi$mdob, age.day = magesudi$dob)
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
levels(usudi$magecat)  <- list("under20"   = c("10-14","15-19"),
                              "20-24" = "20-24",
                              "25-29" = "25-29",
                              "30-34" = "30-34",
                              "35-39" = "35-39",
                              "over40" = c("40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
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
contrasts(usudi$magecat)   <- contr.treatment(levels(usudi$magecat), base=which(levels(usudi$magecat) == "over40"))
rm(age,age.cat)

s1 <- glm(sudi ~ yod, data = usudi, family = binomial(link = "log"))
s2 <- glm(sudi ~ bw,  data = usudi, family = binomial(link = "log"))
s3 <- glm(sudi ~ eth, data = usudi, family = binomial(link = "log"))
s4 <- glm(sudi ~ sex, data = usudi, family = binomial(link = "log"))
s5 <- glm(sudi ~ dhb, data = usudi, family = binomial(link = "log"))
s6 <- glm(sudi ~ dep, data = usudi, family = binomial(link = "log"))
s7 <- glm(sudi ~ magecat, data = usudi, family = binomial(link = "log"))
s8 <- glm(sudi ~ bwr, data = usudi, family = binomial(link = "log"))
s9 <- glm(sudi ~ mage, data = usudi, family = binomial(link = "log"))
library(doBy)
orderBy(~ AIC, AIC(s1,s2,s3,s4,s5,s6,s7))


###
## Year of Death
#
addmargins(table(usudi$yod, usudi$sudi, exclude = NULL))
summary(s1 <- glm(sudi ~ yod, data = usudi, family = binomial(link = "log")))
s1b <- glm(sudi ~ 0 + yod, data = usudi, family = binomial(link = "log"))
anova(s1, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev Pr(>Chi)
#NULL                786299      11695
#yod  12   20.823    786287      11674  0.05304 .
year.ht <- glht(s1, linfct = mcp(yod = "Tukey"))
summary(year.ht)
plot(year.ht)
plot(allEffects(s1))
plot(margins(s1b, type = "link")[[1]])
plot(margins(s1, type = "link")[[1]])
m <- s1
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#Est           LL           UL
#(Intercept) 0.0006884563 0.0005050495 0.0009384666
#yod2002     1.4519065953 0.9648031975 2.1849355049
#yod2003     1.6042566267 1.0782473193 2.3868729171
#yod2004     1.5407978031 1.0355920731 2.2924643127
#yod2005     1.2651144223 0.8364064852 1.9135606069
#yod2006     1.7167325587 1.1654455220 2.5287931717
#yod2007     1.4548079293 0.9813302382 2.1567317799
#yod2008     1.4497370441 0.9779094532 2.1492148277
#yod2009     1.4499473158 0.9757329670 2.1546337881
#yod2010     1.3965287039 0.9386195084 2.0778306900
#yod2011     1.2659600051 0.8412309169 1.9051305681
#yod2012     0.9392033882 0.6060188657 1.4555702047
#yod2013     1.0737210826 0.6998363295 1.6473522659

###
## Birth Weight
#
addmargins(table(usudi$bw, usudi$sudi, exclude = NULL))
foo <- usudi[usudi$bw != "Unknown",]; foo <- droplevels(foo)
contrasts(foo$bw)   <- contr.treatment(levels(foo$bw), base=which(levels(foo$bw) == "over4500"))
summary(s2 <- glm(sudi ~ bw,  data = foo, family = binomial(link = "log")))
s2b <- glm(sudi ~ 0 + bw,  data = foo, family = binomial(link = "log"))
anova(s2, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                785565      11596
#bw    8   279.75    785557      11316 < 0.00000000000000022 ***
bw.ht <- glht(s2, linfct = mcp(bw = "Tukey"))
summary(bw.ht)
plot(bw.ht)
plot(allEffects(s2))
plot(margins(s2b, type = "link")[[1]])
plot(margins(s2, type = "link")[[1]])
m <- s2
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                     Est           LL             UL
#(Intercept)  0.000095225 0.0000238706   0.0003798732
#bwunder1000 16.842732491 3.4073282582  83.2551536765
#bw1000-1499 31.475104241 7.1709458784 138.1522331592
#bw1500-1999 35.643423701 8.5624717940 148.3746380380
#bw2000-2499 28.418252763 7.0007755510 115.3582319861
#bw2500-2999 17.313116111 4.3058396852  69.6133649608
#bw3000-3499  9.880542360 2.4628593436  39.6389333340
#bw3500-3999  5.525348281 1.3707497435  22.2720987346
#bw4000-4499  4.277268915 1.0360875570  17.6578024183




###
## Ethnicity
#
addmargins(table(usudi$eth, usudi$sudi, exclude = NULL))
foo <- usudi[usudi$eth != "Unknown",]; foo <- droplevels(foo)
contrasts(foo$eth)  <- contr.treatment(levels(foo$eth), base=which(levels(foo$eth) == "European_or_other"))
summary(s3 <- glm(sudi ~ eth, data = foo, family = binomial(link = "log")))
s3b <- glm(sudi ~ 0 + eth, data = foo, family = binomial(link = "log"))
anova(s3, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                785716      11680
#eth   2   541.64    785714      11138 < 0.00000000000000022 ***
eth.ht <- glht(s3, linfct = mcp(eth = "Tukey"))
summary(eth.ht)
plot(eth.ht)
plot(allEffects(s3))
plot(margins(s3b)[[1]])
plot(margins(s3)[[1]])
m <- s3
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                     Est           LL          UL
#(Intercept) 0.0003089326 0.0002626802 0.000363329
#ethMaori    7.0387997870 5.8527777958 8.465160334
#ethPacific  3.4457815638 2.6524246786 4.476436478

###
## Sex
#
addmargins(table(usudi$sex, usudi$sudi, exclude = NULL))
summary(s4 <- glm(sudi ~ sex, data = usudi, family = binomial(link = "log")))
s4b <- glm(sudi ~ 0 + sex, data = usudi, family = binomial(link = "log"))
anova(s4, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev      Pr(>Chi)
#NULL                786299      11695
#sex   1   11.028    786298      11684 0.0008975 ***
sex.ht <- glht(s4, linfct = mcp(sex = "Tukey"))
summary(sex.ht)
plot(sex.ht)
plot(allEffects(s4))
plot(margins(s4b)[[1]])
m <- s4
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                     Est           LL           UL
#(Intercept) 0.0008151365 0.0007295571 0.0009107545
#sexM        1.2798616806 1.1056158159 1.4815688216


###
## Location
#
addmargins(table(usudi$dhb, usudi$sudi, exclude = NULL))
foo <- usudi[usudi$dhb != "Unknown",]; foo <- droplevels(foo)
summary(s5 <- glm(sudi ~ dhb, data = foo, family = binomial(link = "log")))
s5b <- glm(sudi ~ 0 + dhb, data = foo, family = binomial(link = "log"))
anova(s5, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev      Pr(>Chi)
#NULL                785343      11693
#dhb   3   40.704    785340      11652 0.000000007555 ***
dhb.ht <- glht(s5, linfct = mcp(dhb = "Tukey"))
summary(dhb.ht)
plot(dhb.ht)
plot(allEffects(s5))
plot(margins(s5)[[1]])
plot(margins(s5b)[[1]])
m <- s5
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                     Est           LL          UL
#(Intercept) 0.0009417619 0.0008408861 0.001054739
#dhbMidland  1.3098096206 1.0927743020 1.569950208
#dhbCentral  1.0529486731 0.8658995271 1.280403642
#dhbSouthern 0.6041930829 0.4778598717 0.763925374


###
## Deprivation
#
addmargins(table(usudi$dep, usudi$sudi, exclude = NULL))
foo <- usudi[usudi$dep != "Unknown",]; foo <- droplevels(foo)
summary(s6 <- glm(sudi ~ dep, data = foo, family = binomial(link = "log")))
s6b <- glm(sudi ~ 0 + dep, data = foo, family = binomial(link = "log"))
anova(s6, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                785292      11679
#dep   1   206.68    785291      11472 < 0.00000000000000022 ***
dep.ht <- glht(s6, linfct = mcp(dep = "Tukey"))
summary(dep.ht)
plot(dep.ht)
plot(allEffects(s6))
m <- s6
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                     Est           LL           UL
#(Intercept) 0.0006116709 0.0005509314 0.0006791068
#dep09-10    2.9457046786 2.5482970779 3.4050880992

###
## Maternal age - categorical
addmargins(table(usudi$magecat, usudi$sudi, exclude = NULL))
summary(s7 <- glm(sudi ~ magecat, data = usudi, family = binomial(link = "log")))
s7b <- glm(sudi ~ 0 + magecat, data = usudi, family = binomial(link = "log"))
anova(s7, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                   786133     9453.2
#magecat  5   241.35    786128     9211.8 < 0.00000000000000022 ***
magec.ht <- glht(s7, linfct = mcp(magecat = "Tukey"))
summary(magec.ht)
plot(magec.ht)
plot(allEffects(s7))
plot(margins(s7)[[1]])
plot(margins(s7b)[[1]])
m <- s7
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
#                        Est           LL            UL
#(Intercept)    0.0002588243 0.0001294476  0.0005175067
#magecatunder20 7.8799783067 3.8448787357 16.1498092353
#magecat20-24   5.3165992300 2.6215406609 10.7822959962
#magecat25-29   2.5353559692 1.2412824514  5.1785392462
#magecat30-34   1.3954781331 0.6755309491  2.8827091087
#magecat35-39   1.5037203419 0.7150663625  3.1621888332


###
## Birth weight - numeric
#
summary(s8 <- glm(sudi ~ bwr, data = usudi, family = binomial(link = "log")))
anova(s8, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                785565      11596
#bwr   1   239.59    785564      11356 < 0.00000000000000022 ***
plot(allEffects(s8))
m <- s8
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
plot(usudi$bwr,usudi$sudi,xlab="Maternal Age",ylab="Probability of survival") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(sudi~bwr,family=binomial,usudi) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
curve(predict(g,data.frame(bwr=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
points(usudi$bwr,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.

###
## Maternal age - numeric
#
summary(s9 <- glm(sudi ~ mage, data = usudi, family = binomial(link = "log")))
anova(s9, test = "Chisq")
#Analysis of Deviance Table
#Model: binomial, link: log
#Response: sudi
#Terms added sequentially (first to last)
#Df Deviance Resid. Df Resid. Dev              Pr(>Chi)
#NULL                786133     9453.2
#mage  1   226.93    786132     9226.2 < 0.00000000000000022 ***
plot(allEffects(s9))
m <- s9
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = coef(m), LL = coef(m) - 1.96 * se, UL = coef(m) + 1.96 *se))
exp(tab)
plot(usudi$mage,usudi$sudi,xlab="Maternal Age",ylab="Probability of survival") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(sudi~mage,family=binomial,usudi) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
curve(predict(g,data.frame(mage=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
points(usudi$mage,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.

library(doBy)
orderBy(~ AIC, AIC(s1,s2,s3,s4,s5,s6,s7,s8,s9))


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


plot(predict(s1),residuals(s1),col=c("blue","red")[1+sudi])
lines(lowess(predict(s1)[sudi==0],residuals(s1)[Y==0]),col="blue")
lines(lowess(predict(s1)[sudi==1],residuals(s1)[Y==1]),col="red")
lines(lowess(predict(s1),residuals(s1)),col="black",lwd=2)
abline(h=0,lty=2,col="grey")

k		<- 20 			##20% of the dataset as testdata
N		<- 100			##100 Permutations
permu		<- paste("Permut_",1:N,sep="")
AUC_Results 	<- matrix(NA, 1, N, dimnames=list("AUC",permu))
n 		<- ncol(usudi)
numrows 	<- nrow(usudi)
learnDataSize	<- round(numrows*(1-0.01*k))
testDataSize	<- numrows-learnDataSize
##loop
for (j in 1:N){
  cat("calculating",((j/N)*100),"% \n")
  learnIndex	<-sample(nrow(Dataset))[1:learnDataSize]
  learnData	<-Dataset[learnIndex,]
  testData	<-Dataset[-learnIndex,]
  mg		<-glm(formula =yourFormula,
            family = binomial(link = "logit"),data=learnData)
  bestmod_cv	<-step(mg,direction="backward",trace=0)
  predicted_cv	<-predict(bestmod_cv, newdata=testData, type="response")
  observed_cv		<-testData[,"Y"]
  AUC_result		<-roc.auc(observed_k, predicted_k)
  AUC_Results[1,j]	<-AUC_result$A
}



set.seed(0)
head(tab <- data.frame(Y=as.numeric(runif(100)>0.5), X=rnorm(100)))
subs <- sample(c(1:nrow(tab)), round(nrow(tab)*0.66), replace=F)  #the 66% of data you want in one sample
tab1 <- tab[subs, ] #the one sample
tab2 <- tab[!c(1:nrow(tab)) %in% subs, ] #the other sample, which are the data that do not fall in the first sample

rlog1 <- glm(Y~X,family=binomial,data=tab1)
summary(rlog1)
tab2$pred <-predict(rlog1, newdata=tab2, type="response")
hist(tab2$pred)

library(ROCR) #allows you to make easily ROC's which allows the assessment of your prediction
pred <- prediction(tab2$pred, tab2$Y)
perf <- performance(pred,"tpr","fpr")
plot(perf); abline(0, 1, col="red")

slrm <- lrm(sudi ~ bw, data = usudi)
