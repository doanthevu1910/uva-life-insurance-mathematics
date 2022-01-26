setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 1/Life Insurance Mathematics/Week 4 - Annuities/Assignment 3")

getwd()

rm(list = ls())

library(pracma)

##2

A = 0.00022; B = 2.7*10^-6; c = 1.124

mu_x <- function(x) {
   A + B*c^x
}

t_p_x <- function(t, x) {
   a <- integral(mu_x, x, x+t)
   exp(-1 * a)
}

t_p_x_2 <- function(t, x) {
   s <- exp(-A)
   g <- exp(-B/(log(c)))
   return(s^t * g^(c^x * (c^t - 1)))
}

t_p_x_2(1, 20)

#check if equal

t_p_x(6, 10) == t_p_x_2(6, 10)

isTRUE(all.equal(t_p_x, t_p_x_2))

all.equal(t_p_x, t_p_x_2)

identical(t_p_x, t_p_x_2)

#plot to check

plot(0:100, t_p_x_2(1, 0:100), type = "l")

a <- NULL

for (i in 0:100) {
   a[i+1] <- t_p_x(1, i)
}

lines(0:100, a, col = "red")

#b
plot(0:100, t_p_x_2(1, 0:100), type = "l")

#c
qx <- 1 - t_p_x_2(1, 1:100)

plot(qx)

##3

##4

