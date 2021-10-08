##
## Stock & Watson chapter 14.1 to 14.5
##

## load .xlsx data
library(readxl)
dbase <- read_excel("../StockWatson/macro_3e.xlsx")
View(dbase)

## save variables in memory
library(tseries)
library(xts)
ffr         <- ts(dbase[,"fyff"], frequency=4, start=c(1957,1),end = c(2005,1))
unempl      <- ts(dbase[,"lhur"], frequency=4, start=c(1957,1),end = c(2005,1))
exr         <- ts(dbase[,"exruk"], frequency=4, start=c(1957,1),end = c(2005,1))
Ln.gdp.jp   <- ts(log(dbase[,"gdpjp"]), frequency=4, start=c(1957,1),end = c(2005,1))
cpi         <- ts(dbase[,"punew"], frequency=4, start=c(1957,1),end = c(2005,1))
Ln.cpi      <- log(cpi)
infl        <- 400*diff.xts(Ln.cpi,differences = 1)

## create transformations
D.infl   <- diff.xts(infl,differences = 1)
infl.1   <- lag.xts(infl,k = 1)
D.infl.1 <- lag.xts(D.infl,k = 1)
D.infl.2 <- lag.xts(D.infl,k = 2)
D.infl.3 <- lag.xts(D.infl,k = 3)
D.infl.4 <- lag.xts(D.infl,k = 4)
unempl.1 <- lag.xts(unempl,k = 1)
unempl.2 <- lag.xts(unempl,k = 2)
unempl.3 <- lag.xts(unempl,k = 3)
unempl.4 <- lag.xts(unempl,k = 4)

## container for QLR test
Q <- ts(rep(0,length(ffr)),frequency=4, start=c(1957,1),end = c(2005,1)) 

## create time series object for all variables
macrodb     <- ts(
                  cbind(ffr, unempl, exr, Ln.gdp.jp, cpi, Ln.cpi, infl, Q,
                        infl.1, D.infl, D.infl.1, D.infl.2, D.infl.3, D.infl.4,
                        unempl.1, unempl.2, unempl.3, unempl.4),
                  frequency = 4, start=c(1957,1),end = c(2005,1)
                 ) ## 1957Q1 - 2005Q1
macrodb.sub <- window(macrodb, start=c(1962,1),end = c(2004,4)) ## 1962Q1 - 2004Q4


## Section 14.2
## Figure 14.2
plot.ts(
        macrodb.sub[,c("ffr","exr","Ln.gdp.jp","cpi","Ln.cpi","infl","D.infl","unempl")],
        main = "Macro Data 1962-2004",
        nc = 2
       )
## Table 14.2
acf(
    macrodb.sub[,c("infl")],
    lag.max = 4,
    type = c("correlation"),
    plot = TRUE,
    main = "Inflation Rate"
   )
acf(
    macrodb.sub[,c("D.infl")],
    lag.max = 4,
    type = c("correlation"),
    plot = TRUE,
    main = "Change of Inflation Rate"
   )


## Section 14.3 Autoregression
## eq 14.7
library(lmtest)
library(sandwich)
ar1 <- lm(D.infl ~ D.infl.1, macrodb.sub)
coeftest(ar1, vcov. = vcovHC(ar1, type="HC1"))

## eq 14.13
ar4<-lm(D.infl ~ D.infl.1 + D.infl.2 + D.infl.3 + D.infl.4, macrodb.sub)
coeftest(ar4, vcov. = vcovHC(ar4, type="HC1"))


## Section 14.4 ARX
## eq 14.16
adl.41 <- lm(D.infl ~ D.infl.1 + D.infl.2 + D.infl.3 + D.infl.4 + unempl.1, macrodb.sub)
coeftest(adl.41, vcov = vcovHC(adl.41, type="HC1"))

## eq 14.17
adl.44 <- lm(D.infl ~ D.infl.1 + D.infl.2 + D.infl.3 + D.infl.4 + unempl.1 + unempl.2 + unempl.3 + unempl.4, macrodb.sub)
coeftest(adl.44, vcov = vcovHC(adl.44, type="HC1"))
