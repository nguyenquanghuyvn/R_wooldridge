library(wooldridge)
library(lmtest)
library(stargazer)

orig <- lm(price ~ lotsize + sqrft + bdrms, 
           data = hprice1)
summary(orig)
reset <- lm(price ~ lotsize + sqrft + bdrms + 
                I(fitted(orig)^2) + I(fitted(orig)^3), 
            data = hprice1)

summary(reset)

est_lol3s <- lm(lprice ~ llotsize + lsqrft + bdrms, 
               data = hprice1)
summary(est_lol3s)

linearHypothesis(reset, matchCoefs(reset, "fitted"),
                 white.adjust = "hc1")

resettest(orig, power = 2:4)
resettest(orig, power = 2:4, vcov = hccm)
resettest(est_lol3s)
resettest(est_lol3s, vcov = hccm)

# Breush-Pagan test ====
bp_resids <- (residuals(orig))^2
bp_ols <- lm(bp_resids ~ lotsize + sqrft + bdrms, 
             data = hprice1)
summary(bp_ols)

bptest(orig)
bptest(orig, ~ I(fitted(orig)) + I(fitted(orig))^2) # White test

# WLS ====
wls_resids <- residuals(orig)
wls_lresids <- log(wls_resids^2)
wls_est <- lm(wls_lresids ~ lotsize + sqrft + bdrms, 
              data = hprice1)
wls_g <- exp(fitted(wls_est))
fgls <- 1 / wls_g

est_wls <- orig <- lm(price ~ lotsize + sqrft + bdrms, 
                      data = hprice1,
                      weights = fgls)
summary(est_wls)

coeftest(est_wls, vcov. = hccm)

# Proxy for variable ====
data("wage2", package = "wooldridge")
head(wage2)

est_noIQ <- lm(lwage ~ educ + exper + tenure + married + south +
                   urban + black, 
               data = wage2)
summary(est_noIQ)

est_IQ <- update(est_noIQ, . ~ . + IQ)
summary(est_IQ)

est_inter <- update(est_noIQ, . ~ . + IQ + IQ:educ)
summary(est_inter)

stargazer(est_noIQ, est_IQ, est_inter,
          type = "text",
          dep.var.labels = "log(wage)",
          align = TRUE)

est_KWW <- update(est_noIQ, . ~ . + KWW)
summary(est_KWW)
est_bothproxy <- update(est_IQ, . ~ .  + KWW)
stargazer(est_noIQ, est_IQ, est_KWW, est_bothproxy,
          type = "text",
          dep.var.labels = "log(wage)",
          column.labels = c("No proxy", "IQ", "KWW", "IQ and KWW"),
          align = TRUE,
          header = FALSE)

# Lag variables ====
data("crime2", package = "wooldridge")
View(crime2)

lcrime82 <- crime2[crime2$year == 82, "lcrmrte"]
est_data <- subset(crime2, year == 87,
                   select = c(lcrmrte, unem , llawexpc))
est_data <- cbind(est_data, lcrime82)

est_noLag <- lm(lcrmrte ~ unem + llawexpc,
                data = est_data)

est_Lag <- lm(lcrmrte ~ unem + llawexpc + lcrime82,
              data = est_data)

stargazer(est_noLag, est_Lag,
          align = TRUE,
          type = "text",
          covariate.labels = c("Constant", "LogLawExp", "Unemployment", "Log Lag"))

# Measurement error ====
## Measurment errors in dependent variable
set.seed(123)
b0 <- 1
b1 <- 0.5
N <- 10000
n <- 1000
b1hat <- vector(length = N, mode = "numeric")
b1hat_me <- vector(length = N, mode = "numeric")
x <- rnorm(n, mean = 4, sd = 1)

for (i in 1:N) {
    u <- rnorm(n)
    ystar <- b0 + b1 * x + u
    b1hat[i] <- coef(lm(ystar ~ x))["x"]
    
    ### Measurement errors:
    e <- rnorm(n)
    y <- ystar + e
    b1hat_me[i] <- coef(lm(y ~ x))["x"]
}

c(mean(b1hat), mean(b1hat_me))
c(var(b1hat), var(b1hat_me))

plot(density(b1hat), 
     main = c(expression("Density of estimated b"[1])),
     xlab = bquote(hat(b)[1]))

lines(density(b1hat_me), 
     lty = 2,
     col = 2)
legend("topleft",
       legend = c("No me", "With me"),
       lty = c(1,2),
       col = c(1,2))

## Measurement errors in explanatory variables
### CLV assumption

xstar <- rnorm(n, mean = 4, sd = 1)

for (i in 1:N) {
    u <- rnorm(n)
    y <- b0 + b1 * xstar + u
    b1hat[i] <- coef(lm(y ~ xstar))["xstar"]
    
    ### Measurement errors:
    e <- rnorm(n)
    x <- xstar + e
    b1hat_me[i] <- coef(lm(y ~ x))["x"]
}

c(mean(b1hat), mean(b1hat_me))
c(var(b1hat), var(b1hat_me))

plot(density(b1hat), 
     main = c(expression("Density of estimated b"[1])),
     xlab = bquote(hat(b)[1]),
     xlim = c(0,0.6), 
     axes = F)
abline(v = 0.5, lty = 3, col = 3, lwd = 2)
lines(density(b1hat_me), 
      lty = 2,
      col = 2)
legend("topleft",
       legend = c("No me", "With me"),
       lty = c(1,2),
       col = c(1,2))

# Missing data ====
data("lawsch85", package = "wooldridge")

View(lawsch85)
str(lawsch85)
summary(lawsch85)

missLAT <- is.na(lawsch85$LSAT)
sum(missLAT)
table(missLAT)
prop.table(table(missLAT))

colSums(is.na(lawsch85))
table(complete.cases(lawsch85))

mean(lawsch85$LSAT)
mean(lawsch85$LSAT, na.rm = TRUE)

summary(lm(log(salary)~LSAT+cost+age, data=lawsch85))

# Outlying Obs ====
data("rdchem", package = "wooldridge")

table(complete.cases(rdchem)) ## No missing data

reg <- lm(rdintens~sales+profmarg, data=rdchem)
inlm <- influence.measures(reg)
summary(inlm)

studres <- rstudent(reg)
hist(studres, freq = FALSE)
lines(density(studres), lwd = 2)


