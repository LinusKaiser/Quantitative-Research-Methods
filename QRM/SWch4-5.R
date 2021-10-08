#
# Stock & Watson Chapter 4 and 5
#

# load .xlsx data
library(readxl)
CAschools <- read_excel("../StockWatson/caschool.xlsx")
View(CAschools)

# load variables in memory
testscr <- CAschools$testscr
str <- CAschools$str

# scatter plot via menu
plot(str, testscr)

# OLS estimation of linear regression
model1 <- lm(testscr ~ str)
summary(model1)

# add estimated regression line to plot
abline(model1)

# create fitted values and add residuals to plot
fit <- predict(model1)
segments(str, testscr, str, fit, col="red",lty=2)

# standard errors for iid disturbances
library(sandwich)
se <- sqrt(diag(vcovHC(model1, type = "const")))
se

# robust se's for heteroscedastic disturbances
robse <- sqrt(diag(vcovHC(model1, type = "HC1")))
robse

# load lmtest package for coeftest()
library(lmtest)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

# robust confindence intervals
coef(model1)-1.96*robse
coef(model1)+1.96*robse

# t tests
tstat <- coef(model1)/robse
tstat


# create dummy variable
D <- ifelse(str<20,1,0)

# OLS estimation of testscr by D
model2 <- lm(testscr~D)
summary(model2)

# select values of testscr if D==1 and compute summary statistics
testscrD <- ifelse(D==1,testscr,NA)
averageD <- mean(testscrD, na.rm=TRUE)
stddevD <- sd(testscrD, na.rm=TRUE)
quantsD <- quantile(testscrD, probs = seq(0, 1, 0.25), na.rm = TRUE)

# and similarly for when D==0         
testscrNotD <- ifelse(D==0,testscr,NA)
