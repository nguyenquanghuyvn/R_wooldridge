library(plm)
library(stargazer)
library(wooldridge)
library(AER)
library(Formula)
#===================#

data("jtrain", package = "wooldridge")

View(jtrain)
head(jtrain)
summary(jtrain)

jtrain_pdata <- pdata.frame(jtrain, index = c("fcode", "year"),
                            drop.index = TRUE, row.names =TRUE )

summary(jtrain_pdata$sales)

est_pool <-  plm(lscrap ~ d88 + d89 + grant + cgrant_1,
                 data = jtrain_pdata, 
                 model = "pool")
est_fe <- plm(lscrap ~ d88 + d89 + grant + cgrant_1,
              data = jtrain_pdata, 
              model = "within")
est_re <- plm(lscrap ~ d88 + d89 + grant + cgrant_1,
              data = jtrain_pdata, 
              model = "random")
est_fd <- plm(lscrap ~ d88 + d89 + grant + cgrant_1,
              data = jtrain_pdata, 
              model = "fd")

summary(est_fe)
summary(est_fd)
summary(est_re)
stargazer(est_pool, est_fe, est_fd, est_re,  type = "text")

#Practice from "plm" package
data("Grunfeld", package = "plm")

grun_pool <- plm(inv ~ value + capital, data = Grunfeld,
                 model = "pool")
grun_fe <- plm(inv ~ value + capital, data = Grunfeld,
               model = "within")
grun_re <- plm(inv ~ value + capital, data = Grunfeld,
               model = "random")
grun_twowayFe <- plm(inv ~ value + capital, data = Grunfeld,
                   effect = "twoways",
                   model = "within")
grun_twowayRe <- plm(inv ~ value + capital, data = Grunfeld,
                   effect = "twoways",
                   model = "random")

summary(grun_pool)
summary(grun_fe)
summary(grun_re)
summary(grun_twowayFe)
summary(grun_twowayRe)
summary(fixef(grun_fe,type = "dmean"))
summary(fixef(grun_twoway, effect = "individual"))

se <- list(robust_pool = sqrt(diag(vcovHC(grun_pool))),
           robust_fe = sqrt(diag(vcovHC(grun_fe))),
           robust_re = sqrt(diag(vcovHC(grun_re))))

stargazer(grun_pool, grun_fe, grun_re,
          title = "Regression results", 
          align = TRUE, dep.var.labels = c("Pool OLS", "FE", "RE"),
          covariate.labels = c("Value", "Capital", "Constant"),
          no.space = TRUE, omit.summary.stat = NULL,
          type = "text")

stargazer(grun_pool, grun_fe, grun_re,
          title = "Regression results", 
          align = TRUE, dep.var.labels = c("Pool OLS", "FE", "RE"),
          covariate.labels = c("Value", "Capital", "Constant"),
          no.space = TRUE, omit.summary.stat = NULL,
          ci = TRUE, ci.level = 0.95, single.row = FALSE,
          type = "text")

stargazer(grun_pool, grun_fe, grun_re,
          title = "Regression results", 
          align = TRUE, dep.var.labels = c("Pool OLS", "FE", "RE"),
          order = c("Constant", "Capital", "Value"),
          no.space = TRUE, omit.summary.stat = NULL,
          ci = TRUE, ci.level = 0.95, single.row = FALSE,
          keep.stat = "n", type = "text")

stargazer(grun_pool, grun_fe, grun_re,
          title = "Regression results", 
          align = TRUE, dep.var.labels = c("Investment"),
          order = c("Constant", "Capital", "Value"),
          no.space = FALSE, omit.summary.stat = NULL,
          ci = FALSE, ci.level = 0.95, single.row = FALSE,
          se = se,
          keep.stat = "n", type = "text")

stargazer(grun_pool, grun_fe, grun_re,
          title = "Regression results", 
          align = TRUE, 
          dep.var.labels = c("Investment"),
          column.labels = c("OLS", "FE", "RE"),
          order = c("Constant", "Capital", "Value"),
          no.space = FALSE, omit.summary.stat = NULL,
          ci = FALSE, ci.level = 0.95, single.row = FALSE,
          se = se,
          keep.stat = "n", type = "text")

## Other random methods
grun_amem <- plm(inv ~ value + capital, data = Grunfeld,
                 model = "random",
                 random.method = "amemiya")
grun_walhus <- plm(inv ~ value + capital, data = Grunfeld,
                 model = "random",
                 random.method = "walhus")
grun_nerlove <- plm(inv ~ value + capital, data = Grunfeld,
                   model = "random",
                   random.method = "nerlove")

summary(grun_amem)
summary(grun_nerlove)
summary(grun_walhus)

stargazer(grun_re, grun_amem, grun_walhus, grun_nerlove,
          type = "text",
          title = "Regression results on RE with different methods",
          column.labels = c("Swamy and Arora","Amemiya", "Walhus", "Nerlove"),
          align = TRUE)

## Example 11.6 from Wooldrige 2010, page 310
data("prison", package = "wooldridge")

View(prison)
str(prison)
summary(prison)

prison_pool <- plm(lcriv ~ lpris + polpc + incpc + unem + black + metro +
                       ag0_14 + ag15_17 + ag18_24 + ag25_34,
                   data = prison,
                   model = "fd",)
summary(prison_pool)


formula <- formula(mv~crim+zn+indus+chas+nox+rm+age+
                       dis+rad+tax+ptratio+blacks+lstat)
class(formula)

## Instrument variables
data("Crime", package = "plm")
data("Wages", package = "plm")
cr <- plm(log(crmrte) ~ log(prbarr) + log(polpc) + log(prbconv) +
              log(prbpris) + log(avgsen) + log(density) + log(wcon) +
              log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) +
              log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) +
              region + smsa + factor(year) | . - log(prbarr) - log(polpc) +
              log(taxpc) + log(mix), data = Crime,
          model = "random")
summary(cr)

ht <- pht(lwage~wks+south+smsa+married+exp+I(exp^2)+
               bluecol+ind+union+sex+black+ed | sex+black+bluecol+south+smsa+ind,
          data=Wages,index=595)

summary(ht)

## Practice matrix
N <- 5
T <- 3

I_t <- diag(T)
i_t <- rep(1, T)
Z_t <- kronecker(i_t, I_t)
kronecker(I_t, i_t)
Z_t %*% solve((t(Z_t) %*% Z_t)) %*% t(Z_t) 
J_t <- i_t %*% t(i_t)
J_tBar <- 1/5 * i_t %*% t(i_t)

i_n <- rep(1,N)
J_n <- 1/N * i_n %*% t(i_n)

E_n <- diag(n) - J_n
E_t <- I_t - J_t
Q <- kronecker(E_n, E_t)

kronecker(diag(5), I_t)

5*kronecker(I_n, I_t)
