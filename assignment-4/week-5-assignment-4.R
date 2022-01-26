setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 1/Life Insurance Mathematics/Week 5 - Premiums/Assignment 4")

getwd()

rm(list = ls())

##1

A = 0.00022; B = 2.7*10^-6; c = 1.124

t_p_x <- function(t, x) {
   s <- exp(-A)
   g <- exp(-B/(log(c)))
   return(s^t * g^(c^x * (c^t - 1)))
}

px <- t_p_x(1, 0:100)

qx <- 1 - px

age <- 0:100

life_table <- data.frame(age, px, qx); head(life_table)

i <- 0.05

##2

###1
term_insurance <- function(age, n, i, life_table) {
   qx <- life_table$qx
   px <- 1 - qx
   kpx <- c(1, cumprod(px[(age+1):(age+n-1)]))
   kqx <- kpx * qx[(age+1):(age+n)]
   discount_factors <- (1 + i) ^ - (1:length(kqx))
   sum(discount_factors * kqx)
}

p1 <- 500000*term_insurance(50, 20, i, life_table); p1

###2
life_annuity_due <- function(age, n, i, life_table) {
   px <- 1 - life_table$qx #replace life table with tpx codes
   kpx <- c(1, cumprod(px[(age+1):(age+n-1)]))
   discount_factors <- (1+i)^ - (0:(n-1))
   sum(discount_factors * kpx)
}

a1 <- life_annuity_due(50, 20, i, life_table)

p2 <- p1/a1; p2

###3

EPV_benefits <- NULL; EPV_annuities <- NULL

for (k in 1:20) {
   EPV_benefits <- c(EPV_benefits, 500000*term_insurance(50+k, 21-k, i, life_table))
   EPV_annuities <- c(EPV_annuities, p2*life_annuity_due(50+k, 21-k, i, life_table))
}

plot(EPV_annuities - EPV_benefits)

barplot(EPV_annuities - EPV_benefits)

###4

plot(1:20, pch = 1:20)

###3-2
TI <- function(age, n, i) {
   kpx <- c(1, cumprod(px[(age + 1):(age + n - 1)]))
   kqx <- kpx * qx[(age + 1):(age + n)]
   discount_factors <- (1 + i) ^ - (1:length(kqx))
   sum(discount_factors * kqx)
}

TA <- function(age, n, i) {
   kpx <- c(1, cumprod(px[(age + 1):(age + n - 1)]))
   discount_factors <- (1 + i) ^ - (0:(n - 1))
   sum(discount_factors * kpx)
}

L <- c(); s <- 0

for (i in c(50:70)) {
   for (j in seq(20, 1, -1)) {
      L[s] <- (-500000 * TI(i, j, 0.05)) + (p2 * TA(i, j, 0.05))
   }
   s <- s + 1
}

barplot(L, ylim = c(-10000,5000),
        main = "Excess of premium over benefit for 20-year term insurance issued to (50)",
        xlab = "Duration (years)", ylab = "EPV")

excess <- c()

v = (1+0.05)^-1

qx <- life_table$qx

for(i in 0:19){
   excess[i+1] <- p2 - 500000*v*qx[50+1+i]
}

barplot(excess, ylim = c(-3000, 1000),
        main = "Excess premium over benefit for term insurance (n=20) for (50)",
        xlab = "Duration (years)", ylab = "EPV")

excess
