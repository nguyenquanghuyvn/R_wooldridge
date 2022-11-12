library(AER)
library(wooldridge)
library(stargazer)
library(scales)

data("gpa3", package = "wooldridge")
data("crime1", package = "wooldridge")
data("hprice1", package = "wooldridge")

View(gpa3)
summary(gpa3)
plot(density(gpa3$cumgpa))
plot(density(gpa3$sat))
cor(gpa3)

est_ols <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white,
              data = gpa3)

est_sum <- summary(est_ols) 
est_sum$coefficients[,2]

se <- coeftest(est_ols, vcov. = hccm(est_ols, type = "hc1"))[,2]

cbind(se = est_sum$coefficients[,2], se_robust = se)

myH0 <- c("black", "white")
linearHypothesis(est_ols, myH0)
linearHypothesis(est_ols, myH0, vcov = hccm)
linearHypothesis(est_ols, myH0, white.adjust = "hc1", test = "Chisq")

#LM test

est_ur <- lm(narr86 ~ pcnv + avgsen + I(avgsen^2) + ptime86 + qemp86 +
                  inc86 + black + hispan, 
              data = crime1)
est_rm <- lm(narr86 ~ pcnv + ptime86 + qemp86 +
                 inc86 + black + hispan, 
             data = crime1)

resid <- residuals(est_rm)

est_avgsen <- lm(avgsen ~ pcnv + ptime86 + qemp86 +
                               inc86 + black + hispan, 
                           data = crime1)
resid_avgsen <- residuals(est_avgsen)

avgsensq <- crime1$avgsen^2
st_avgsensq <- lm(avgsensq ~ pcnv + ptime86 + qemp86 +
                              inc86 + black + hispan, 
                          data = crime1)
resid_avgsensq <- residuals(st_avgsensq)

multiResid_avgsen <- resid_avgsen * resid
multiResid_avgsensq <- resid_avgsensq * resid

vec1 <- rep(1, length(multiResid_avgsen))
est_lm <- lm(vec1 ~ multiResid_avgsen + multiResid_avgsensq + 0)
summary(est_lm)

LM <- length(multiResid_avgsen) - sum(residuals(est_lm)^2)
2 * pnorm(-abs(LM))

p_value <- 1 - pchisq(LM, 2)

# Test for heteroskedasticity
head(hprice1)
str(hprice1)
summary(hprice1)

mod <- lm(hprice1$price ~ hprice1$sqrft)
plot(hprice1$price ~ hprice1$sqrft,
     cex = 0.5,
     pch = 19)

abline(mod, col = "darkred")

mod_normal <- lm(price ~ lotsize + sqrft + bdrms, 
                 data = hprice1)
summary(mod_normal)

resids_sq <- residuals(mod_normal)^2
hetero_mod <- lm(resids_sq ~ lotsize + sqrft + bdrms,
                 data = hprice1,
                 x = TRUE, y = TRUE)
(hetero_sum <- summary(hetero_mod))
1 - pf(5.33, 3,84) > 0.05

bptest(mod_normal)

fit_price <- fitted(mod_normal)
bptest(mod_normal, ~ fit_price + I(fit_price)^2)

mod_log <- lm(log(price) ~ I(log(lotsize)) + I(log(sqrft)) + I(log(bdrms)),
              data = hprice1)

summary(mod_log)
bptest(mod_log)
