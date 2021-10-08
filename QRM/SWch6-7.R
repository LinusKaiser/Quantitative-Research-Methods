#
# Stock & Watson Chapter 6 and 7
#

# load .xlsx data; you may have to change the path to "../StockWatson/caschool.xlsx"
library(readxl)
CAschools <- read_excel("../StockWatson/caschool.xlsx")
View(CAschools)

# load variables in memory
testscr <- CAschools$testscr
str <- CAschools$str
el_pct <- CAschools$el_pct

# estimate multivariate regression
library(sandwich)
library(lmtest)
model2 <- lm(testscr ~ el_pct + str)
coeftest(model2, vcov=vcovHC(model2, type="HC1"))

# correlation between regressors
cor(el_pct, str)

# standard errors for iid and heteroscedastic disturbances
se <- sqrt(diag(vcovHC(model2, type = "const")))
se
robse <- sqrt(diag(vcovHC(model2, type = "HC1")))
robse

# quantiles from chi^2 and chi^2/q distributions
qchisq(c(.95, .99), df=2)
qchisq(.95, df=c(1, 2, 3, 4, 5))/c(1, 2, 3, 4, 5)

# plot chi^2(3) and chi^2(3)/3 densities for illustration
x <- seq(0, 10, length=1000)
fx <- dchisq(x, 3)
plot(x, fx, type="l", lwd=2, lty=1, xlab="x value", ylab="Density", col="blue")
lines(x/3, fx, lwd=2, col="red")

# plot chi^2(3), chi^2(5), chi^2(5) densities for illustration
plot(x, dchisq(x, 3), type="l", lty=1, xlab="x value", ylab="Density", col="blue", lwd=2)
lines(x, dchisq(x,4), lwd=2, col="red")
lines(x, dchisq(x,5), lwd=2, col="yellow")

# scale variable so as to make coefficient interpretable
exp <- CAschools$expn_stu/1000

# estimate unrestricted and restricted model; note that R^2 are part of summary() not coeftest()
model3 <- lm(testscr ~ el_pct + exp + str)
summary(model3)
model4 <- lm(testscr ~ el_pct)
summary(model4)

# test hypothesis under homoscedasticity and heterscedasticity
library(car)
linearHypothesis(model3, c("exp = 0", "str = 0"), vcov=vcovHC(model3, type = "const"))
linearHypothesis(model3, c("exp = 0", "str = 0"), vcov=vcovHC(model3, type="HC1"))

