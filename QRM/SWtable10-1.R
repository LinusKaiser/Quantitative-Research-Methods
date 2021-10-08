##
## Stock & Watson Chapter 10, Table 10.1 using the plm package
##

## load .xlsx data
library(readxl)
Panel <- read_excel("../StockWatson/fatality.xlsx")
View(Panel)

## library(calibrate)
library(lmtest)


## panel dimensions
N   <-length(unique(Panel$state))
T   <-max(Panel$year)-min(Panel$year)+1

## (re-) define variables
Panel$FatalityRate <- Panel$mrall*10000
Panel$Avm          <- Panel$vmiles/1000
Panel$Drink.18     <- 0
Panel$Drink.18[Panel$mlda >= 18 & Panel$mlda < 19] <- 1
Panel$Drink.19     <- 0
Panel$Drink.19[Panel$mlda >= 19 & Panel$mlda < 20] <- 1
Panel$Drink.20     <- 0
Panel$Drink.20[Panel$mlda >= 20 & Panel$mlda < 21] <- 1
Panel$Mandatory    <- 0
Panel$Mandatory[Panel$jaild == 1 | Panel$comserd == 1] <- 1
Panel$Ln.inc       <- log(Panel$perinc)
## note that the dimension of these variables is only 48, so they are not saved easily in Panel
D.FatalityRate  <- Panel$FatalityRate[Panel$year==1988] - Panel$FatalityRate[Panel$year==1982]
D.beertax       <- Panel$beertax[Panel$year==1988] - Panel$beertax[Panel$year==1982] 

## estimate models
library(plm)
model.1 <- plm(FatalityRate ~ beertax, data = Panel, model = "pooling")
model.2 <- plm(FatalityRate ~ beertax, data = Panel, model = "within")
model.3 <- plm(FatalityRate ~ beertax, data = Panel, effect = "twoways", model = "within")

## without plm package: least squares dummy variable (LSDV) regression
## LSDV    <- lm(FatalityRate ~ beertax + factor(year) + factor(state), data=Panel) 

model.4 <- plm(FatalityRate ~ beertax + Drink.18 + Drink.19 + Drink.20 + Mandatory + Avm + unrate + Ln.inc,
               data = Panel, effect = "twoways", model = "within")
model.5 <- plm(FatalityRate ~ beertax + Drink.18 + Drink.19 + Drink.20 + Mandatory + Avm,
               data = Panel, effect = "twoways", model = "within")
model.6 <- plm(FatalityRate ~ beertax + mlda + Mandatory + Avm + unrate + Ln.inc,
               data = Panel, effect = "twoways", model = "within")
model.7 <- plm(FatalityRate ~ beertax + Drink.18 + Drink.19 + Drink.20 + Mandatory + Avm + unrate + Ln.inc,
               data = subset(Panel, year == "1982" | year == "1988"), effect = "twoways", model = "within")

## compute standard errors
library(sandwich)
df.cor <- c(N/(N-1))
se.1<-sqrt(diag(vcovHC(model.1,method = c("white2"),type="HC1")))
se.2<-sqrt(df.cor*vcovHC(model.2,method = c("arellano"),type="HC0"))
se.3<-sqrt(df.cor*vcovHC(model.3,method = c("arellano"),type="HC0"))
se.4<-sqrt(df.cor*diag(vcovHC(model.4,method = c("arellano"),type="HC0")))
se.5<-sqrt(df.cor*diag(vcovHC(model.5,method = c("arellano"),type="HC0")))
se.6<-sqrt(df.cor*diag(vcovHC(model.6,method = c("arellano"),type="HC0")))
se.7<-sqrt(df.cor*diag(vcovHC(model.7,method = c("arellano"),type="HC0")))

## display results; note that the R^2 are not correctly computed
library(stargazer)
stargazer(model.1,model.2,model.3,model.4,
          se=list(se.1,se.2,se.3,se.4),
          title = "Regression Results",
          no.space=TRUE,
          omit.stat=c("f"),
          covariate.labels=c("BeerTax", "Drinking age 18", "Drinking age 19", "Drinking age 20",
                             "Mandatory jail or community service?", "Average vehicle miles per hour", "Unempl.rate", "Real income per capita (log)"),
          type = "text"
          )


stargazer(model.5,model.6,model.7,
          se=list(se.5,se.6,se.7),
          title = "Regression Results (Robustness Checks)",
          omit.stat=c("f"),
          covariate.labels=c("BeerTax", "Drinking age 18", "Drinking age 19", "Drinking age 20","Drinking age",
                             "Mandatory jail or community service?", "Average vehicle miles per hour", "Unempl.rate", "Real income per capita (log)"),
          no.space=TRUE,
          type = "text"
          )
