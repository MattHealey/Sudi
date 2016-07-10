library(ggplot2)
library(gridExtra)
library(MASS)
library(fitdistrplus)
library(sjPlot)
library(sjmisc)
library(texreg)
library(AICcmodavg)
library(lme4)
require(countreg)
require(boot)
## set contrasts. keep as treatment by default for models. Use sum for post-hoc.
## set ref group for each factor to a priori lowest risk group (from lit!)

sjp.setTheme(theme = science_theme)
sjp.frq(count.sudi$deaths,
        geom.colors = "lightblue",
        title = NULL, type = 'hist')

screenreg(fit1, single = T)
sjt.glm(fit1, stringPredictors = "Predictors",
        stringDependentVariables = "Models",
        stringIntercept = "",
        labelPredictors = c("", "", "","",""),
        labelDependentVariables = c("Deaths","Deaths inc. pop"),
        separateConfColumn = T, showHeaderStrings = TRUE,
        CSS = list(css.table = "border: 2px solid;",
                   css.tdata = "border: 1px solid;",
                   css.depvarhead = "color:#003399;"),
        showLogLik = T, showAIC = T, showAICc = T,
        showDeviance = T, showChi2 = T, showHosLem = T, showFamily = T)
mort <- count.sudi

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.15,1), legend.justification = "top")

s.mod <- glm.nb(deaths ~ offset(log(1+pop)) + yod + bw + eth + sex + dep + dhb, data = mort)
x <- mp(mort,s.mod,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = yod), alpha = .5) +
  geom_line(aes(colour = yod), size = 2)
nbyod <- g1 + lk + coord_cartesian(ylim = c(0,3))
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .5) +
  geom_line(aes(colour = eth), size = .8)
nbeth <- g1 + lk + coord_cartesian(ylim = c(0,3))
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = sex), alpha = .5) +
  geom_line(aes(colour = sex), size = .8)
nbsex <- g1 + lk+ coord_cartesian(ylim = c(0,4))
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dep), alpha = .5) +
  geom_line(aes(colour = dep), size = .8)
nbdep <- g1 + lk+ coord_cartesian(ylim = c(0,3))
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .5) +
  geom_line(aes(colour = dhb), size = .8)
nbdhb <- g1 + lk + coord_cartesian(ylim = c(0,3))
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = bw), alpha = .5) +
  geom_line(aes(colour = bw), size = .8)
nbbw <- g1 + lk + coord_cartesian(ylim = c(0,3))
grid.arrange(nbyod, nbeth, nbsex, nbdep, nbdhb, nbbw, nrow=2)

s.null1 <- glm.nb(deaths ~ offset(log(1+pop)), data = mort)
x <-mp(mort,s.null1,95)
g1 <- ggplot(x, aes(pop, deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .5) +
     geom_line(aes(pop), size = .8)
lnull1 <- g1 + lk+ coord_cartesian(ylim = c(0,3))
s.null2 <- glm.nb(deaths ~ offset(log(1+pop)) + pop, data = mort)
x <-mp(mort,s.null2,95)
g1 <- ggplot(x, aes(pop, deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .5) +
     geom_line(aes(pop), size = .8)
lnull2 <- g1 + lk+ coord_cartesian(ylim = c(0,1))
grid.arrange(lnull1,lnull2)

x <-mp(mort,s.null1,95)
g1 <- ggplot(x, aes(pop,deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .5) +
     geom_line(aes(pop), size = .8)
     null1 <- g1 + lk+ coord_cartesian(ylim = c(0,3))
x <-mp(mort,s.null2,95)
g1 <- ggplot(x, aes(pop, deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .5) +
     geom_line(aes(pop), size = .8)
null2 <- g1 + lk+ coord_cartesian(ylim = c(0,.6))
grid.arrange(null1,null2)


lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.15,1), legend.justification = "top")
s.yod <- glm.nb(deaths ~ offset(log(1+pop)) + yod, data = mort)
x <- mp(mort,s.yod,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = yod), alpha = .6) +
  geom_line(aes(colour = yod), size = .8)
syod <- g1 + lk + coord_cartesian(ylim = c(0,5)) + facet_grid(. ~ sex)
s.yod <- glm.nb(deaths ~ offset(log(1+pop)) + yod + pop, data = mort)
x <- mp(mort,s.yod,95)
g1 <- ggplot(x, aes(pop, deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL, fill = yod), alpha = .6) +
     geom_line(aes(colour = yod), size = .8)
syod2 <- g1 + lk + coord_cartesian(ylim = c(0,1)) + facet_grid(. ~ sex)
x <- mp(mort,s.mod,95)
g1 <- ggplot(x, aes(pop, deaths)) +
     geom_ribbon(aes(ymin = LL, ymax = UL, fill = yod), alpha = .6) +
     geom_line(aes(colour = yod), size = .8)
syod3 <- g1 + lk+ coord_cartesian(ylim = c(0,3)) + facet_grid(. ~ sex)
grid.arrange(syod,syod2,syod3)

s.eth <- glm.nb(deaths ~ offset(log(1+pop)) + eth + pop, data = mort)
x <-mp(mort,s.eth,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth), size = .8)
seth <- g1 + lk + coord_cartesian(ylim = c(0,1.25)) + facet_grid(. ~ bw)
x <- mp(mort,s.mod,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  #geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth), size = .8)
seth2 <- g1 + lk+ coord_cartesian(ylim = c(0,2)) + facet_grid(. ~ bw)
grid.arrange(seth,seth2)

x <-mp(mort,glm.quin2,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = quin), alpha = .6) +
  geom_line(aes(colour = quin), size = .8)
nbquin <- g1 + lk+ coord_cartesian(ylim = c(0,1))
x <-mp(mort,glm.dhb2,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb), size = .8)
nbdhb <- g1 + lk + coord_cartesian(ylim = c(0,1))
x <-mp(mort,glm.year2,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = year), alpha = .6) +
  geom_line(aes(colour = year), size = .8)
nbyear <- g1 + lk + coord_cartesian(ylim = c(0,1))
grid.arrange(nbage, nbeth, nbgen, nbquin, nbdhb, nbyear, nrow=2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.019,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(log(pop)) + age + gen, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ gen)
nbag <- g1 + lk + coord_cartesian(ylim = c(0,16), xlim = c(0,17000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ gen)
nbag2 <- g1 + lk + coord_cartesian(ylim = c(0,10), xlim = c(4,10))
grid.arrange(nbag,nbag2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.039,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + gen, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .6) +
  geom_line(aes(colour = gen)) + facet_grid(. ~ age)
nbga1 <- g1 + lk + coord_cartesian(ylim = c(0,14), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .6) +
  geom_line(aes(colour = gen)) + facet_grid(. ~ age)
nbga2 <- g1 + lk + coord_cartesian(ylim = c(0,14), xlim = c(3,10))
grid.arrange(nbga1,nbga2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbad1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbad2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbad1,nbad2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ age)
nbda1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ age)
nbda2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbda1,nbda2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ eth)
nbed1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,15000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ eth)
nbed2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbed1,nbed2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.04,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbde1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,16000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbde2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbde1,nbde2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.019,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbay1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbay2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbay1,nbay2)

fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbey1 <- g1 + lk + coord_cartesian(ylim = c(0,7), xlim = c(0,18000))
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbey2 <- g1 + lk + coord_cartesian(ylim = c(0,7), xlim = c(5,10))
grid.arrange(nbey1,nbey2)

