# Library ====
library(wooldridge)
library(AER)
library(moments)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(psych)
#================#

data("lawsch85", package = "wooldridge")
help("lawsch85")
View(lawsch85)
summary(lawsch85)
table(complete.cases(lawsch85))

salary_noNA <- !is.na(lawsch85$salary)
salary_noNA <- lawsch85$salary[salary_noNA]
plot(density((salary_noNA)))

# Hourly wage data
data("wage1", package = "wooldridge")
str(wage1)

res <- lm(log(wage)~married*female+educ+exper+I(exper^2)+tenure+I(tenure^2),
    data=wage1)

wage1$female <- as.logical(wage1$female)

wage1$female <- factor(wage1$female)
table(wage1$female)

car::Anova(res)

# Breaking a numeric variable into Categories
cutpts <- c(0, 10, 25, 40, 60, 100, 175)
lawsch85$rankcat <- cut(lawsch85$rank, cutpts)
table(lawsch85$rankcat)

lawsch85$rankcat <- relevel(lawsch85$rankcat, "(100,175]")
(res <- lm(log(salary) ~ rankcat+LSAT+GPA+log(libvol)+log(cost), 
           data = lawsch85))

# Computer exercises ====
## C1 ====
data("gpa1", package = "wooldridge")
str(gpa1)
summary(gpa1)

mod <- lm(colGPA ~ PC + hsGPA + ACT, 
          data = gpa1)

summary(mod)

gpa1$PC <- factor(gpa1$PC, labels = c("No", "Yes"))
summary(gpa1$PC)

gpa1$fathcoll <- factor(gpa1$fathcoll)
gpa1$mothcoll <- factor(gpa1$mothcoll)

mod_add <- lm(colGPA ~ PC + hsGPA + ACT + fathcoll + mothcoll, 
                        data = gpa1)

stargazer(mod, mod_add,
          type = "text",
          align = TRUE)

linearHypothesis(mod_add, c("fathcoll1 = 0", "mothcoll1 = 0"),
                 white.adjust = "hc1")

mod_sq <- lm(colGPA ~ PC + hsGPA + I(hsGPA^2)+ ACT + fathcoll + mothcoll, 
              data = gpa1)
summary(mod_sq)

coeftest(mod_sq, vcov. = vcovHC(mod_sq, type = "HC1"))
linearHypothesis(mod_sq, matchCoefs(mod_sq,"hsGPA"))

rm(list = ls())

## C2 ====
data("wage2", package = "wooldridge")

View(wage2)
summary(wage2)

colSums(is.na(wage2))
table(complete.cases(wage2))

cor(wage2, use = "complete.obs")

skewness(wage2, na.rm = TRUE)
kurtosis(wage2, na.rm = TRUE)


plot(density(wage2$wage))

wage2$black <- factor(wage2$black, labels = c("No", "Yes"))

(mod_normal <- lm(wage ~ educ + exper + tenure + married + black + south + urban,
                 data = wage2, 
                 x = TRUE))

summary(mod_normal)

vif(mod_normal)
vif(mod_normal, type = "predictor")
vif(mod_sq)
vif(mod_sq, type = "predictor")
bptest(mod_normal,  ~  + fitted(mod_normal) + fitted(mod_normal)^2)

coeftest(mod_normal, vcov. = vcovHC(mod_normal, "HC1"))

mod_sq <- lm(wage ~ educ + exper + tenure + married + black + south + urban +
                 I(exper^2) + I(tenure^2),
             data = wage2)

stargazer(mod_normal, mod_sq,
          align = TRUE,
          column.labels = c("Normal", "With squared vars"),
          type = "text")

plot(wage2$wage ~ wage2$exper)
abline(mod_normal)

ord <- order(wage2$exper)
lines(wage2$exper[ord], fitted(mod_sq)[ord])

mod_eduRace <- lm(wage ~ educ*black + exper + tenure + married + south + urban,
                  data = wage2)

summary(mod_eduRace)
linearHypothesis(mod_eduRace, matchCoefs(mod_eduRace, "edu|black"),
                 white.adjust = "hc1")

wage2$married <- factor(wage2$married)
mod_eduIntn <- lm(lwage ~ educ*(black:married) + exper + tenure + south + urban,
                  data = wage2 )
mod_sum <- summary(mod_eduIntn)
str(mod_sum$coefficients)
class(mod_sum$coefficients)

rownames(mod_sum$coefficients) <- c("Intercept", "educ", "exper",
                                    "tenure", "south", "urban",
                                    "white.single", "black.single",
                                    "white.married",
                                    "educ.white.single",
                                    "educ.black.single",
                                    "educ.white.married")
mod_sum$coefficients

## C3 ====
rm(list = ls())
data("mlb1", package = "wooldridge")

View(mlb1)
summary(mlb1)

p <- ggplot(data = mlb1)

p + geom_freqpoly(aes(x = salary), stat = "density") +
    theme_classic()

mod_normal <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr + rbisyr +
                     runsyr + fldperc + allstar + frstbase + scndbase + 
                     thrdbase + shrtstop + catcher,
                 data = mlb1, 
                 x = TRUE,
                 y = TRUE)

summary(mod_normal)

coeftest(mod_normal, vcov. = hccm)
vif(mod_normal)

X <- mod_normal$x[,-1]
Y <- mod_normal$y
resids <- residuals(mod_normal)

summary(lm(I(resids)^2 ~ X))

H0 <- c("fldperc = allstar",
        "allstar = frstbase",
        "frstbase = scndbase", 
        "scndbase = thrdbase",
        "thrdbase = shrtstop")

linearHypothesis(mod_normal, H0, white.adjust = "hc1",
                 test = "Chisq")

## C3 ====
rm(list = ls())
data("gpa2", package = "wooldridge")

View(gpa2)
summary(gpa2)

plot(density(gpa2$colgpa))
kurtosis(gpa2)
skewness(gpa2)

athele_id <- as.logical(gpa2$athlete)
black_id <- as.logical(gpa2$black)

rank_topid <- gpa2$hsrank < 10
table(rank_topid)


boxplot(gpa2$colgpa ~ gpa2$female)
boxplot(gpa2$colgpa ~ gpa2$white)
boxplot(gpa2$colgpa ~ gpa2$black)
boxplot(gpa2$colgpa ~ gpa2$athlete)
boxplot(gpa2$colgpa ~ rank_topid)

plot(gpa2$colgpa ~ gpa2$sat,
     cex = 0.8,
     pch = 20, 
     col = "gray")

points(gpa2$sat[athele_id], gpa2$colgpa[athele_id],
       cex = 0.8, 
       pch = 20,
       col = "red")

points(gpa2$sat[black_id ], gpa2$colgpa[black_id ],
       cex = 0.8, 
       pch = 20,
       col = "red")

points(gpa2$sat[rank_topid], gpa2$colgpa[rank_topid],
       cex = 0.8, 
       pch = 20,
       col = "red")

plot(gpa2$colgpa ~ gpa2$tothrs,
     cex = 0.8,
     pch = 20, 
     col = "gray")

points(gpa2$tothrs[athele_id], gpa2$colgpa[athele_id],
       cex = 0.8, 
       pch = 20,
       col = "red")

points(gpa2$tothrs[black_id ], gpa2$colgpa[black_id ],
       cex = 0.8, 
       pch = 20,
       col = "red")

plot(gpa2$colgpa ~ gpa2$verbmath,
     cex = 0.8,
     pch = 20, 
     col = "gray")

plot(gpa2$colgpa ~ gpa2$hsperc,
     cex = 0.8,
     pch = 20, 
     col = "gray")

points(gpa2$hsperc[athele_id], gpa2$colgpa[athele_id],
       cex = 0.8, 
       pch = 20,
       col = "red")

plot(gpa2$colgpa ~ gpa2$hsrank)

gpa2$female <- factor(gpa2$female, levels = c(0,1), labels = c("No","Yes"))
summary(gpa2$female, )
str(gpa2$female)

mod_base <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athele,
               data = gpa2,
               x = TRUE)

summary(mod_base)
coeftest(mod_base, vcov. = vcovHC(mod_base, 'HC1'))

mod_dropSat <- update(mod_base, . ~ . -sat)
summary(mod_dropSat)

stargazer(mod_base, mod_dropSat,
          type = "text")

vif(mod_base, type = "predictor")

summary(lm(sat ~  hsize + I(hsize^2) + hsperc + female + athele,
           data = gpa2))

mod_inter <- update(mod_base, .~. + female:athele)
summary(mod_inter)

linearHypothesis(mod_inter, 
                 c("atheleTRUE = 0", "femaleYes:atheleTRUE = 0"),
                 white.adjust = "hc1")
mod_satGen <- update(mod_base, .~.  + sat:female) 
summary(mod_satGen)

coeftest(mod_satGen, vcov. = hccm)

linearHypothesis(mod_satGen,
                 c("femaleYes = 0", "sat:femaleYes = 0"),
                 white.adjust = "hc1")

### LM test

resids_r <- residuals(update(mod_base, .~. - female))
lm_mod <- lm(resids_r ~  hsize + I(hsize^2) + hsperc + sat*female + athele,
             data = gpa2)
summary(lm_mod)


stargazer(mod_base, mod_dropSat, mod_inter, mod_satGen,
          type = "text")

## C5 ====
rm(list = ls())
data("ceosal1", package = "wooldridge")

View(ceosal1)
summary(ceosal1)
describe(ceosal1)

plot(density(ceosal1$salary))
plot(density(ceosal1$pcsalary))
plot(density(ceosal1$sales))
plot(density(ceosal1$roe))
plot(density(ceosal1$pcroe))
plot(density(ceosal1$finance))
plot(density(ceosal1$consprod))
plot(density(ceosal1$lsalary))

ceosal1$rosneg <- ceosal1$ros < 0
ceosal1$indus <- as.factor(ceosal1$indus)
ceosal1$finance <- as.factor(ceosal1$finance)
ceosal1$consprod <- as.factor(ceosal1$consprod)

mod_base <- lm(lsalary ~ lsales + roe + rosneg,
               data = ceosal1)

summary(mod_base)

mod_ex <- lm(lsalary ~ lsales + roe + rosneg + indus + finance + consprod,
             data = ceosal1)

summary(mod_ex)
bptest(mod_ex,  ~ + fitted(mod_ex) + I(fitted(mod_ex)^2))
coeftest(mod_ex, vcov. = hccm)

## C6 =====
rm(list = ls())
data("fertil2", package = "wooldridge")

View(fertil2)
summary(fertil2)
describe(fertil2)

plot(density(fertil2$educ))
hist(fertil2$agefbrth)

p <- ggplot(data = fertil2)
p + geom_count(aes(x = agefbrth, y = children))

fertil2$urban <- factor(fertil2$urban, labels = c("Not urban", "Urban"))
fertil2$electric <- factor(fertil2$electric, labels = c("No elec", "Elec"))
fertil2$tv <- factor(fertil2$tv, labels = c("No TV", "TV"))

mod_base <- lm(children ~ educ + age + I(age^2) + urban + electric + tv,
               data = fertil2)

xtabs(children ~ educ + age + I(age^2) + urban + electric + tv,
      data = fertil2)

summary(mod_base)

vif(mod_base)
bptest(mod_base)
coeftest(mod_base, vcov. = hccm)

linearHypothesis(mod_base, matchCoefs(mod_base, "age"))

urban_id <- fertil2$urban == 1
table(urban_id)

mod_nonUrban <- update(mod_base, .~. - urban, 
                       data = fertil2[!urban_id,])

mod_Urban <- update(mod_base, .~. - urban, 
                       data = fertil2[urban_id,])


se_rb <- list(se_b = coeftest(mod_base, vcov. = hccm)[,2],
              se_nU = coeftest(mod_nonUrban, vcov. = hccm)[,2],
              se_U = coeftest(mod_Urban, vcov. = hccm)[,2])
stargazer(mod_base, mod_nonUrban, mod_Urban,
          type = "text", align = TRUE,
          column.labels = c("All sample","No urban", "Urban"),
          se = se_rb)
se_rb
coeftest(mod_base, vcov. = hccm)[,2]

prop.table(table(fertil2$electric, fertil2$urban))
prop.table(table(fertil2$tv, fertil2$urban),2)

ggplot(data = fertil2[urban_id,]) +
    geom_boxplot(aes(x = tv, y = children))

ggplot(data = fertil2[!urban_id,]) +
    geom_boxplot(aes(x = tv, y = children))

### Chow test
SSR_noU <- sum(residuals(mod_nonUrban)^2)
SSR_U <- sum(residuals(mod_Urban)^2)

SSR_p <- sum(residuals(mod_base)^2)

F <- (SSR_p - (SSR_noU + SSR_U)) / (SSR_noU + SSR_U) * 
    ((4361 - 2 * (5 + 1)) / 6)

## C7 ====
rm(list = ls())

data("wage1", package = "wooldridge")

View(wage1)

mod_base <- lm(lwage ~ female*educ + exper + expersq + tenure + tenursq,
               data = wage1)

summary(mod_base)

vif(mod_base, type = "predictor")
coeftest(mod_base, vcov. = hccm)

mod_atMean <- lm(lwage ~ female + I(educ - 12.5) + female:I(educ - 12.5) + 
                         exper + expersq + tenure + tenursq,
               data = wage1)

summary(mod_atMean)

## C8 ====

data("loanapp", package = "wooldridge")

View(loanapp)
describe(loanapp)
summary(loanapp)


unique(loanapp$white)
loanapp$white <- factor(loanapp$white, levels = c("0", "1"))
head(loanapp$white)


loanapp %>%
    count(white,approve) %>%
    group_by(white) %>%
    mutate(prop = n / sum(n))


ggplot(data = loanapp) +
    geom_boxplot(mapping = aes(x = white, y = appinc))

mod_sim <- lm(approve ~ white, data = loanapp)
summary(mod_sim)

mod_all <- lm(approve ~ . - action - reject - msa - mortlat1 - mortlat2
              - black - hispan,
              data = loanapp)
summary(mod_all)

mod_inter <- update(mod_all, .~. + white:obrat)
summary(mod_inter)

stargazer(mod_sim, mod_all, mod_inter,
          type = "text")

loanapp$obratMean <- loanapp$obrat - 32
mod_interMean <- update(mod_all, .~. - obrat + obratMean +
                            white:obratMean)



stargazer(mod_all, mod_inter, mod_interMean,
          type = "text")

plot(density(loanapp$obrat))
hist(loanapp$obrat)

loanapp$approve <- factor(loanapp$approve)

obrat_sum <- loanapp %>%
    select(white, approve, obrat) %>%
    group_by(white, approve) %>%
    summarise(obrat_mean = mean(obrat),
              n = n())


    ggplot() +
    geom_col(mapping = aes(x = white, y = prop, fill = approve),
             position = "dodge") +
    labs(y = "Proportion") +
    theme_classic()


p <- ggplot(data = loanapp)
p + geom_col(mapping = aes(x = white, y = obrat, fill = approve),
             position = "dodge")

## C9 ====
rm(list = ls())

data("k401ksubs", package = "wooldridge")

View(k401ksubs)
summary(k401ksubs)

k401ksubs$e401k <- factor(k401ksubs$e401k)

p <- ggplot(k401ksubs)

p + geom_boxplot(aes(x = e401k, y = inc))

mod_base <- lm(e401k ~ inc + age, data = k401ksubs)
summary(mod_base)

mod_gender <- update(mod_base, .~. + male)
summary(mod_gender)

linearHypothesis(mod_gender, 
                 c("age = 0", "male = 0"),
                 white.adjust = "hc1")

fit_gender <- fitted(mod_gender)

sum(fit_gender > 1 | fit_gender < 0)

resp_predict <- ifelse(fit_gender >= 0.5, 1, 0)

table(predict = resp_predict, true = k401ksubs$e401k)
t <- prop.table(table(predict = resp_predict, true = k401ksubs$e401k), 2)

mod_pira <- update(mod_gender, .~. + pira)
summary(mod_pira)

fit_pira <- fitted(mod_pira)
sum(fit_pira < 0 | fit_pira > 1)

resp_predictPi <- ifelse(fit_pira >= 0.5, 1, 0)

table(predict = resp_predictPi, true = k401ksubs$e401k)
prop.table(table(predict = resp_predictPi, true = k401ksubs$e401k),2)

## C10 ====
rm(list = ls())

data("affairs", package = "wooldridge")

View(affairs)
p <- ggplot(affairs)

summary(affairs)

affairs$kids <- factor(affairs$kids)
affairs$relig <- factor(affairs$relig)
affairs$ratemarr <- factor(affairs$ratemarr)

table(affairs$affair, affairs$ratemarr)
ftable(xtabs(affair ~ ratemarr + kids,
      data = affairs))

summary(xtabs(affair ~ ratemarr + kids,
              data = affairs))

mod_base <- lm(affair ~ yrsmarr + age + educ,
               data = affairs)

summary(mod_base)

linearHypothesis(mod_base,
                 c("age = 0", "educ = 0"),
                 white.adjust = "hc1")

mod_kids <- update(mod_base, .~. + kids)
summary(mod_kids)
coeftest(mod_kids, vcov. = hccm)

vif(mod_kids)

mod_relig <- update(mod_kids, .~. + relig)
summary(mod_relig)
vif(mod_relig)

affairs$relig <- relevel(affairs$relig, "2")

## C11 ====
rm(list = ls())

data("k401ksubs", package = "wooldridge")
summary(k401ksubs)

plot(density(k401ksubs$nettfa))

mod_base <- lm(nettfa ~ age + inc + e401k, 
               data = k401ksubs)

summary(mod_base)

mod_sq <- update(mod_base, .~. + I(age^2) + I(inc^2))
mod_interat0 <- update(mod_sq, .~. + e401k:I(age) + e401k:I((age)^2))
mod_interaction <- update(mod_sq, .~. + e401k:I(age - 41) + e401k:I((age - 41)^2))

stargazer(mod_base, mod_sq, mod_interaction, 
          type = "text")

linearHypothesis(mod_interaction, matchCoefs(mod_interaction, "e401k"))

summary(k401ksubs$fsize)

cutpts <- c(min(k401ksubs$fsize) - 1,1,2,3,4, 
            max(k401ksubs$fsize))
k401ksubs$sizerank <- cut(k401ksubs$fsize, cutpts,
                          labels = c("f1","f2", "f3", "f4", "f5"))
summary(k401ksubs$sizerank,)

mod_fsize <- update(mod_sq, .~. + sizerank)
summary(mod_fsize)
