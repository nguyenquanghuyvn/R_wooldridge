library(wooldridge)
library(AER)
library(quantmod)
library(dynlm)

data("barium", package = "wooldridge")
data("fertil3", package = "wooldridge")
data("hseinv", package = "wooldridge")

str(barium)
summary(barium)

impts <- ts(barium, start = c(1978,2), frequency = 12)
plot(impts)

getSymbols("F", auto.assign = TRUE)
head(F)

last(F)
summary(F)
dim(F)

plot(F$F.Adjusted, las = 1)


tsdata <- ts(fertil3, start = 1913)

res <- dynlm(gfr ~ pe + L(pe) + L(pe,2) +
                 ww2 + pill,
             data = tsdata)
summary(res)
coeftest(res, vcov. = vcovHC(res, "HC1"))

linearHypothesis(res, matchCoefs(res, "pe"))

b <- coef(res)
b["pe"] + b["L(pe)"] + b["L(pe, 2)"]
linearHypothesis(res, "pe + L(pe) + L(pe, 2) = 0 ")

res <- dynlm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
             data = impts)
summary(res)

tsdata <- ts(hseinv, start = 1947)

head(tsdata)

res1 <- dynlm(linvpc ~ lprice, data = tsdata)
res2 <- dynlm()

