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
x <- which(foo5$pop < foo5$deaths)
foo5$deaths[x] <- 0
count.sudi <- foo5
rm(foo,foo2,foo3,foo4,foo5,x, sudit.0,sudit.1)


