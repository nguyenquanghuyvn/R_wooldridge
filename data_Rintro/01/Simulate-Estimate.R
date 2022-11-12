# Set the random seed
set.seed(123456)

# Draw a sample given the population parameters
sample <- rnorm(100,10,2)

# Estimate the population mean with the sample average
mean(sample)

# Draw a different sample and estimate again:
sample <- rnorm(100,10,2)
mean(sample)

# Draw a third sample and estimate again:
sample <- rnorm(100,10,2)
mean(sample)

#Simulation

r <- 10000
ybar <- numeric(r)
n <- 100

for (i in 1:r) {
    sample <- rnorm(n, mean = 10, sd = 2)
    ybar[i] <- mean(sample)
}


mean(ybar)
sd_theory <- sqrt(2^2 / n)
sd(ybar)

plot(density(ybar))
curve(dnorm(x, 10, sd_theory), add = TRUE, lty = 2)

# Asymptotic property

size <- c(10,50,100,1000)

ybar_diffsize <- matrix(NA, nrow = r, ncol = length(size))

for (s in 1:length(size)) {
    for (i in 1:r) {
        sample <- rnorm(size[s], mean = 10, sd = 2)
        ybar_diffsize[i,s] <- mean(sample)
    }
        
}

colnames(ybar_diffsize) <- paste("n=",size, sep = "")

par(mfrow = c(2,2))

for (s in 1:length(size)) {
    plot(density(ybar_diffsize[,s]), 
         xlim = c(8.5,11.5))
    curve(dnorm(x, 10, sqrt(4/size[s])), add = TRUE, lty = 2)
    legend("topright",
           legend = c("sample density", "theorital density"),
           lty = c(1,2),
           cex = 0.8,
           xjust = 1, yjust = 1
           )
}

par(mfrow = c(1,1))
curve(dchisq(x,1))
