#Library =====
library(wooldridge)
library(AER)
library(plm)

library(stargazer)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

library(boot)
library(multiwayvcov)
library(clusterSEs)
#================#
rm(list = ls())

data("kielmc", package = "wooldridge")
View(kielmc)
summary(kielmc)

dat <- kielmc

fac_var <- c("y81", "nearinc")
for (i in fac_var) {
    dat[,i] <- factor(dat[,i])
}

summary(dat)

p <- ggplot(dat)
p + geom_boxplot(aes(x = nearinc, y = lrprice))

mod_base <- lm(
    lrprice ~ nearinc*y81,
    data = dat
    )

mod_age <- update(mod_base, .~. + age + agesq)
mod_full <- update(mod_age, .~. + lintst + lland + rooms + baths)

stargazer(mod_base, mod_age, mod_full,
          type = "text")

# Simple panel data
data("crime2", package = "wooldridge")

dat <- crime2

View(dat)

summary(dat)

dat$d87 <- factor(dat$d87)

mod_base <- lm(
    crmrte ~ d87 + unem,
    data = dat
    )

summary(mod_base)

crime2_p <- pdata.frame(crime2, index = 46)

# From book on R ====
data("crime4", package = "wooldridge")
View(crime4); help(crime4)

crime4_p <- pdata.frame(crime4, 
                        index = c("county", "year"))

mod_base <- plm(diff(log(crmrte)) ~ d83+d84+d85+d86+d87+diff(lprbarr)+
                    diff(lprbconv)+ diff(lprbpris)+diff(lavgsen)+diff(lpolpc),
                data=crime4_p, 
                model="pooling")

summary(mod_base)

mod_fd <- plm(log(crmrte)~d83+d84+d85+d86+d87+lprbarr+lprbconv+
                  lprbpris+lavgsen+lpolpc,data=crime4_p, model = "fd")

summary(mod_fd)

# C1 ====
rm(list = ls())
data("fertil1")
View(fertil1)


X_var <- c( "educ", "age", "agesq",
    "black", "east", "northcen", "west", "farm", "othrural", "town"
    ,"smcity", "y74", "y76", "y78", "y80", "y82", "y84")

X_var <- paste(X_var, "+")

form <- as.formula(kids ~ X_var)

mod_base <- lm(kids ~ educ + age + agesq +
                   black + east + northcen + west +
                   othrural + town + smcity + 
                   y74 + y76 + y78 + y80 + y82 + y84,
               data = fertil1)

summary(mod_base)
linearHypothesis(mod_base, 
                 c("othrural = 0", "town = 0", "smcity = 0"),
                 white.adjust = "hc1")

linearHypothesis(mod_base,
                 c("northcen = 0", "west = 0", "east = 0"))

usq <- residuals(mod_base)^2

mod_usq <- lm(usq ~ y74 + y76 + y78 + y80 + y82 +y84, 
              data = fertil1)

summary(mod_usq)

mod_interaction <- update(mod_base,
                          .~. + y74educ + y76educ + y78educ + y80educ + y82educ +
                              y84educ)

summary(mod_interaction)

linearHypothesis(mod_interaction,
                 matchCoefs(mod_interaction, "y(..)educ"),
                 white.adjust = "hc1")

# C2 ====
data("cps78_85", package = "wooldridge")

View(cps78_85)
summary(cps78_85)

p <- ggplot(data = cps78_85)
p + geom_density(mapping = aes(lwage)) +
    geom_function(fun = dnorm, args  = list(mean = 2 , sd = 0.5), color = "red") +
    theme_classic()

mod_base <- lm(lwage ~ y85 + educ + y85:educ + exper + expersq + union +
                   female + y85:female, 
               data = cps78_85)

summary(mod_base)
reset(mod_base)
bptest(mod_base, ~ fitted(mod_base) + fitted(mod_base)^2)
vif(mod_base)

var(cps78_85$y85fem)
var(cps78_85$y85educ)

coeftest(mod_base, vcov. = vcovHC(mod_base, type = "HC3"))

mod_educ12 <- update(mod_base, .~. - y85:educ + y85:I(educ -12))

summary(mod_educ12)

confint(mod_educ12)

stargazer(mod_base, mod_educ12, 
          align = TRUE,
          type = "text",
          ci = TRUE, ci.level = 0.99,
          model.names = FALSE,
          column.labels = c("Base", "Educ at 12"),
          digits = 3,
          initial.zero = FALSE,
          intercept.bottom = FALSE)

cps78_85$lrwage <- ifelse(test = cps78_85$y85 == 0, 
                          yes = cps78_85$lwage, 
                          no = cps78_85$lwage - log(1.65))

summary(cps78_85)

mod_baserw <- update(mod_base, lrwage ~.)

summary(mod_baserw)
plot(mod_baserw)

stargazer(mod_base, mod_baserw, type = "text",
          intercept.bottom = FALSE)

mod_union <- update(mod_base, .~. + y85:union)
summary(mod_union)
vif(mod_union)

mod_unionrw <- update(mod_baserw, .~. + y85:union)
summary(mod_unionrw)
vif(mod_unionrw)

resids <- residuals(mod_unionrw)
mean(resids)
residsq <- resids^2

summary(
    lm(residsq ~ y85 + educ + y85:educ + exper + expersq + union +
           female + y85:female,
       data = cps78_85)
)

w <- 1 / sqrt(cps78_85$educ)
mod_wls <- update(mod_baserw, weights = w)

resids <- residuals(mod_wls)
mean(resids)
residsq <- resids^2

#C3 ====

rm(list = ls())
data("kielmc")
View(kielmc)

mod_base <- lm(lprice ~ y81*I(ldist),
               data = kielmc)

summary(mod_base)

mod_all <- update(mod_base, 
                  formula = .~. + age + agesq + rooms + baths +
                      lintst + lland + larea)

stargazer(mod_base, mod_all,type = "text")

summary(kielmc$ldist)

# C4 ====
data("injury")
View(injury)
summary(injury)

dat <- tibble(ldurat = injury$ldurat,
              aftch = factor(injury$afchnge),
              high = injury$highearn)

dat %>% filter(high == 0) %>%
ggplot() +
    geom_boxplot(aes(aftch, ldurat))

ggplot(dat) +
    geom_boxplot(aes(aftch, ldurat)) +
    facet_grid(~high)

mod_base <- lm(ldurat ~ afchnge + highearn + afchnge:highearn,
               data = injury)

summary(mod_base)
plot(mod_base)

sum(residuals(mod_base)^2)

mod_all <- update(object = mod_base, 
                  formula = .~. + male + married + age +
                      head + neck + upextr + trunk + lowback + lowextr +
                      occdis + manuf + construc)
 summary(mod_all)
 
 stargazer(mod_base, mod_all, 
           type = "text")
 
 mod_michigan <- update(mod_all, .~.,
                        data = injury,
                        subset = mi == 1)
summary(mod_michigan) 

mod_kentucky <- update(mod_all, .~.,
                       data = injury,
                       subset = ky == 1)

summary(mod_kentucky)

stargazer(mod_all, mod_michigan, mod_kentucky,
          type = "text")

mod_miDum <- update(mod_all, .~. + ky + ky:highlpre)

summary(mod_miDum)

# C5 =====
rm(list = ls())
data("rental")

View(rental)
summary(rental)

p <- ggplot(data = rental) + theme_classic()

p + geom_density(aes(x = rent, fill = factor(rental$year))) +
    labs(fill = "year") +
    # no legend
    #guides(fill = FALSE)
    theme(legend.position = "none")

p + geom_point(aes(x = pctstu, y = rent, color = factor(rental$year)), show.legend = TRUE)

p + geom_density(aes(x = rent, fill = factor(rental$year), 
                     color = factor(rental$year))) +
    labs(fill = "year") +
    guides(color = FALSE) +
    theme(legend.title = element_text(face = "bold"))

mod_base <- lm(lrent ~ y90 + lpop + lavginc + pctstu,
               data = rental)

summary(mod_base)
coeftest(mod_base, vcov. = vcovHC(mod_base, type = "HC1"))

mod_fd <- plm(lrent ~ y90 + lpop + lavginc + pctstu,
              data = rental,
              model = "fd")
 
summary(mod_fd)
coeftest(mod_fd, vcov. = vcovHC(mod_fd, type = "HC1"))

stargazer(mod_base, mod_fd,
          type = "text")

# C6 ====
rm(list = ls())
data(crime3) 

crime_pda <- pdata.frame(crime3, index = c("district", "year"))

mod_base <- lm(lcrime ~ d78 + clrprc1 + clrprc2,
               data = crime_pda)

summary(mod_base)

mod_fd <- plm(lcrime ~ d78 + clrprc1 + clrprc2,
              data = crime_pda,
              model = "fd")

summary(mod_fd)

stargazer(
    mod_base, mod_fd,
    type = "text",
    intercept.bottom = FALSE
)

linearHypothesis(mod_fd,
                 c("clrprc1 = clrprc2"),
                 white.ajust = hccm)

mod_fdDelta <- update(mod_fd, .~. - clrprc1  - clrprc2 + avgclr)
summary(mod_fdDelta)

# C7 ====
rm(list = ls())
data("gpa3")

View(gpa3)
gpa3_pdta <- pdata.frame(gpa3, index = c("id", "term"))
summary(gpa3_pdta)

p <- ggplot(gpa3) + theme_classic()
p + geom_density(aes(trmgpa, color = factor(gpa3_pdta$season),
                     linetype = factor(gpa3_pdta$season))) +
    labs(color = "In season") +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c("right", "top"),
          legend.title = element_text(face = "bold"))

p + geom_density(aes(trmgpa, color = factor(gpa3_pdta$football)))
p + geom_point(aes(x = sat, trmgpa, color = factor(gpa3_pdta$football)))
p + geom_boxplot(aes(x = factor(gpa3_pdta$season), y = trmgpa)) +
    facet_grid(~football)

mod_base <- lm(trmgpa ~ spring + sat + hsperc + female + black + white +
                   frstsem + tothrs + crsgpa + season,
               data = gpa3_pdta)

summary(mod_base)

mod_foot <- update(mod_base, .~. + football)
summary(mod_foot)

mod_nofoot <- update(mod_base, .~., 
                     subset = football == 0)

summary(mod_nofoot)

mod_wfoot <- update(mod_base, .~., 
                     subset = football == 1)
summary(mod_wfoot)

mod_fd <- plm(trmgpa ~ spring + sat + hsperc + female + black + white +
                  frstsem + tothrs + crsgpa + season,
              data = gpa3_pdta)


summary(mod_fd)

#C8 ====
rm(list = ls())

data("vote2")

View(vote2)
colnames(vote2)
dat <- vote2
dat1 <- pivot_longer(vote2,
                    c('vote88', 'vote90'), 
                    names_to = "year", 
                    names_prefix = "vote",
                    values_to = "vote") %>%
    select(year, vote) %>%
    mutate(id = rep(1:dim(vote2)[1], each = 2))

dat2 <- pivot_longer(vote2, c('inexp88', 'inexp90'), 
                    names_to = "year", 
                    names_prefix = "inexp",
                    values_to = "inexp") %>%
    select(year, inexp) %>%
    mutate(id = rep(1:dim(vote2)[1], each = 2))

dat3 <- pivot_longer(vote2, c('chexp88', 'chexp90'), 
                     names_to = "year", 
                     names_prefix = "chexp",
                     values_to = "chexp") %>%
    select(year, chexp) %>%
    mutate(id = rep(1:dim(vote2)[1], each = 2))

dat4 <- pivot_longer(vote2, c('win88', 'win90'), 
                     names_to = "year", 
                     names_prefix = "win",
                     values_to = "win") %>%
    select(year, win) %>%
    mutate(id = rep(1:dim(vote2)[1], each = 2))

dat <- left_join(dat1, dat2, by = c("id", "year")) %>%
    left_join(y = dat3, by = c("id", "year")) %>%
    left_join(y = dat4, by = c("id", "year"))

dat <- pdata.frame(dat, index = c("id", "year"))
rm(list = c("dat1", "dat2", "dat3", "dat4"))

summary(dat)

mod_base <- lm(vote ~ I(factor(year)) + I(log(inexp)) + I(log(chexp)),
               data = dat)

summary(mod_base)

mod_fd <- plm(vote ~ I(factor(year)) + I(log(inexp)) + I(log(chexp)),
              data = dat,
              model = "fd")

mod_pool <- plm(vote ~ I(factor(year)) + I(log(inexp)) + I(log(chexp)),
              data = dat,
              model = "pool")

mod_fe <- plm(vote ~ I(factor(year)) + I(log(inexp)) + I(log(chexp)),
              data = dat,
              model = "within")

stargazer(mod_base, mod_fd, mod_fe,
          type = "text")

pooltest(vote ~ I(log(inexp)) + I(log(chexp)),
         data = dat,
         model = "pool")

plmtest(mod_pool, effect = "individual")
pFtest(vote ~ I(factor(year)) + I(log(inexp)) + I(log(chexp)),
       data = dat,
       effect = "individual")

phtest(mod_pool, mod_fe)

#C9 ====
rm(list = ls())

data("crime4")

View(crime4) ; help(crime4)
summary(crime4)

dat <- crime4 %>% 
    select(starts_with("w"),-west)

dat_log <- data.frame(matrix(NA, nrow = dim(dat)[1], ncol = dim(dat)[2]))
name <- vector("character", 9)

for (i in 1:length(colnames(dat))) {
 
    dat_log[,i] <- log(dat[,i])
    name[i] <- paste("l", colnames(dat)[i], sep = "")
}

colnames(dat_log) <- name

dat[,col]
value(col)
colnames(dat[2])
paste("l", colnames(dat)[2], sep = "")

# C10 ====
rm(list = ls())
data("wagepan")
wagepan_pdata <- pdata.frame(wagepan, index = c("nr", "year"))

summary(wagepan)

p <- ggplot(data = wagepan) + theme_classic()

p + geom_boxplot(aes(x = factor(manuf), y = lwage))
p + geom_col(aes(x = educ, y = lwage))

mod_base <- lm(lwage ~ educ + black + hisp + married + union +
                   d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
               data = wagepan)

summary(mod_base)
coeftest(mod_base, vcov. = vcovHC(mod_base, type = "HC1"))
         
mod_fd <- plm(lwage ~ educ + black + hisp + married + union +
                            d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
              data = wagepan_pdata,
              model = "fd")

summary(mod_fd)        
resid_fd <- residuals(mod_fd)
lresid_fd <- lag(resid_fd,1)

summary(lm(resid_fd ~ lresid_fd))


## Test for unobserved effect:
pwtest(lwage ~ educ + black + hisp + married + union+
           d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
       data = wagepan_pdata)

## Test for serial correlation or RE:
pbsytest(lwage ~ educ + black + hisp + married + union+
             d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
         data = wagepan_pdata,
         test = "j")

## Test for AR(1) and MA(1):
pbsytest(lwage ~ educ + black + hisp + married + union+
             d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
         data = wagepan_pdata)

## Test for Re:
pbsytest(lwage ~ educ + black + hisp + married + union+
             d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
         data = wagepan_pdata,
         test = "re")

# Conditional LM test for AR(1) or MA(1) errors under random effects
pbltest(lwage ~ educ + black + hisp + married + union+
            d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
        data = wagepan_pdata,
        alternative = "onesided")

pbltest(lwage ~ educ + black + hisp + married + union+
            d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
        data = wagepan_pdata,
        alternative = "twosided")

## Test for serial correlation in idiosyncratic errors

mod_fe <- plm(lwage ~ educ + black + hisp + married + union +
                  d81 + d82 + d83 + d84 + d85 + d86 + d86 + d87,
              data = wagepan_pdata,
              model = "within")


pbgtest(mod_fe, order = 2)

# Fixed efect
data("jtrain")

View(jtrain)

dat <- pdata.frame(jtrain, index = c("fcode", "year"))

mod_pool <- plm(lscrap ~ d88 +d89 + grant + grant_1,
                data = dat,
                model = "pool")

summary(mod_pool)

mod_fe <- plm(lscrap ~ d88 +d89 + grant + grant_1,
                data = dat,
                model = "within")

summary(mod_fe)
cluster.bs.plm(mod_fe, dat = dat)

plmtest(lscrap ~ d88 +d89 + grant + grant_1,
        data = dat,
        effect = "individual")

plmtest(lscrap ~ d88 +d89 + grant + grant_1,
        data = dat,
        effect = "twoways", 
        type = "bp")

pdim(dat)
pvar(dat)

phtest(lscrap ~ d88 +d89 + grant + grant_1,
       data = dat,
       method = "aux")

rm(list = ls())
data("wagepan")
dat <- pdata.frame(wagepan)

pdim(dat)
pvar(dat)
 educ
form <- lwage ~ black + exper +hisp + married + educ + union

pbsytest(form, data = dat, test = "j")
pbsytest(form, data = dat, test = "ar")
pbsytest(form, data = dat, test = "re")

pbltest(form, data = dat)

mod_fe <- plm(form, data = dat, model = "within")
mod_re <- plm(form, data = dat, model = "random")
mod_cre <- update(mod_re, .~. + Between(married) + Between(union))

stargazer(mod_fe, mod_re, mod_cre,
          type = "text")

linearHypothesis(mod_cre, matchCoefs(mod_cre, "Between"),
                 vcov. = vcovHC(mod_cre, method ="arellano",
                                type="HC1", cluster ="group"))

phtest(mod_fe, mod_re)

# Given RE, testing for AR(1) or MA(1)
pbltest(form, data = dat, alternative = "onesided")

# Serial correlation for FE 
pbgtest(mod_fe, order = 2)

## using Wooldridge
pwartest(form, data = dat)

## Wooldridge test for serial correlation in FD and FE
pwfdtest(form, data = dat, h0 = "fd")
pwfdtest(form, data = dat, h0 = "fe")

pcdtest(form, data = dat)
pcdtest(form, data = dat, model = "within")
coef(mod_fe)

mod_lm <-  lm(form, data = dat)
cluster.boot(mod_lm, cluster = dat$nr)
