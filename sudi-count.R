blank <- expand.grid(sex = c("F","M"),
                     eth = c("Maori", "European_or_other","Pacific"  ),
                     dep= c("01_08", "09_10"),
                     yod= c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"),
                     dhb = c("Nothern" ,"Midland" , "Central" , "Southern"),
                     bw = c("under1000" , "1000_1499" , "1500_1999" , "2000_2499" , "2500_2999",
                            "3000_3499" , "3500_3999" , "4000_4499", "over4500")
                     )

sudit.0 <- subset(sudit, sudi == "0")
sudit.0$sudi <- 1
sudit.1 <- subset(sudit, sudi == "1")

foo  <- aggregate(sudi ~ yod + bw + dhb + dep + sex + eth, data = sudit.1, FUN = sum)
foo$deaths <- foo$sudi; foo$sudi <- NULL
foo$pop <- 0
foo2 <- aggregate(sudi ~ yod + bw + dhb + dep + sex + eth, data = sudit.0, FUN = sum)
foo2$pop <- foo2$sudi;foo2$sudi <- NULL
foo2$deaths <- 0
foo3 <- rbind(foo,foo2)
foo4 <- aggregate(cbind(deaths, pop) ~ yod + bw + dhb + dep + sex + eth, data = foo3, FUN = sum)
foo5 <- merge(foo4, blank, by = c("yod","sex","dep","dhb","bw","eth"), all = T)
foo5$deaths[is.na(foo5$deaths)] <- 0
foo5$pop[is.na(foo5$pop)] <- 0
x <- which(foo5$pop < foo5$deaths)
foo5$deaths[x] <- 0
sapply(foo5,function(x) sum(is.na(x)))
count.sudi <- foo5
rm(foo,foo2,foo3,foo4,foo5,x, sudit.0,sudit.1,blank)

library(ggplot2)
library(MASS)
library(fitdistrplus)
library(vcd)
descdist(count.sudi$deaths,boot=50000,discrete=T,obs.col="blue", obs.pch = 15, boot.col="red")
descdist(count.sudi$pop,boot=5000,discrete=T,obs.col="blue", obs.pch = 15, boot.col="red")
summary(nb <- fitdist(count.sudi$deaths, "nbinom"))
plot(nb)
plot(foo<-goodfit(count.sudi$deaths,type = "nbinomial"), shade=T)
print(foo);summary(foo)
gofstat(nb,chisqbreaks = 0:3, discrete=T)
observed <- table(count.sudi$deaths)
foo  <- dnbinom(as.numeric(names(observed)), size = unname(nb$estimate[1]), mu = unname(nb$estimate[2])) * sum(observed)
rootogram(observed, foo)
(foo<-distplot(observed, type = "nbinomial"))
(foo<-Ord_plot(observed, type = "nbinomial"))

fit1 <- glm.nb(deaths ~ offset(log(1+pop)) + yod + bw + sex + dhb  + eth, data = count.sudi)
library(texreg)
fit1e <- fit1
fit1e$coefficients <- exp(coef(fit1))
screenreg(fit1e, single.row = T)
library(sjPlot)
sjp.glm(fit1, type = "ma")

library(rms)
f   <- Glm(deaths ~ offset(log(1+pop)) + yod + bw + sex + dhb  + eth, family = negative.binomial(fit1$theta), data = count.sudi)
print(names(coef(f)), quote=FALSE)
specs(f)
anova(f)
an <- anova(f)
options(digits=3)
print(an, 'subscripts')
print(an, 'dots')
