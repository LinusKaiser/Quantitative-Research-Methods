#
# Stock & Watson Chapter 8
#

# load .xlsx data
library(readxl)
CAschools <- read_excel("../StockWatson/caschool.xlsx")
View(CAschools)

# load variables in memory
str <- CAschools$str
testscr <- CAschools$testscr
avginc <-CAschools$avginc
el_pct <- CAschools$avginc

# load packages for for HCSE's and custom summary tables
library(lmtest)
library(sandwich)

# estimate original model
model0 <- lm(testscr ~ str)
coeftest(model0, vcov = vcovHC(model0, type = "HC1"))

# plot sample regression
plot(str, testscr)
abline(model0)

# estimate new model
model00 <- lm(testscr ~ avginc)
coeftest(model00, vcov = vcovHC(model00, type = "HC1"))

# plot relationship
plot(avginc, testscr)
abline(model00, lwd=2)


# create transformed variables: squares
avginc2 <- avginc^2

# estimate polynomial model
model1 <- lm(testscr ~ avginc + avginc2)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

# plot quadratic function between min and max value of x variable
avginc_min = min(avginc, na.rm = FALSE)
avginc_max = max(avginc, na.rm = FALSE)
fit1 = function(x){607.3018 + 3.851*x - 0.0423*x^2}
curve(fit1, xlim = c(avginc_min, avginc_max), col = "blue", lwd=2, add = TRUE)


# create transformed variables: logs
# NB log() is the natural log, log10() is the log with base 10
lnavginc <- log(avginc)

# estimate linear-log regression and print robust SEs
model2 <- lm(testscr ~ lnavginc)
coeftest(model2, vcov = vcovHC(model2, type = "HC1"))

# plot log function between min and max value of x variable
fit2 = function(x){557.8323 + 36.4197*log(x)}
curve(fit2, xlim = c(avginc_min, avginc_max), col = "red", lwd=2, add = TRUE)


# for log-linear and log-log regression, create log of testscr
lntestscr <- log(testscr)

# plot scatter with vertical log-axis
plot(avginc, lntestscr)


# create dummy and interaction variables
HiStr <- ifelse(str>=20,1,0)
HiEL <- ifelse(el_pct>=10,1,0)
HiStrxHiEL <- HiStr*HiEL

# estimate linear model with dummy variables
model3 <- lm(testscr ~ HiEL + HiStr + HiStrxHiEL)
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))


# create interaction variable
strxpct <- str*el_pct

# estimate linear model with interaction variable
model4 <- lm(testscr~el_pct+str+strxpct)
coeftest(model4, vcov = vcovHC(model4, type = "HC"))
