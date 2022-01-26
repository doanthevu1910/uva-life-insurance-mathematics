setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 1/Life Insurance Mathematics/Week 1/Assignment 1")

getwd()

rm(list = ls())

#2 
interest <- c(rep(0.06, 5), rep(0.07, 5), rep(0.08, 13)); interest

cash_flow_loan <- c(10000, 20000, 30000)

cash_flow_vector <- c(1)

for (i in 1:19) {
  cash_flow_vector[i+1] <- cash_flow_vector[i]*(1-0.05) 
}

discount <- function(s, t, i) {(1 + i) ^ - (t - s)}

PV_loan <- sum(cash_flow_loan * discount(0, 0:2, interest[1:3]))

PV_payments <- sum(cash_flow_vector * discount(0, 3:22, interest[4:23]))

k <- PV_loan/PV_payments

yearly_payments <- cash_flow_vector*k*discount(0, 3:22, interest[4:23])

yearly_payments

#test
discount(0, 3:5, interest[4:6])

## new solution (the correct one)

cash_flow_loan <- c(10000, 20000, 30000)

cash_flow_vector <- c(1)

for (i in 1:19) {
  cash_flow_vector[i+1] <- cash_flow_vector[i]*(1-0.05) 
}

cash_flow_vector

d_factors <- c(1.06^-3, rep(NULL, 19))

for (i in 1:2) {
  d_factors[i+1] <- d_factors[i]*1.06^-1
}

d_factors[4] <- 1.06^-5*1.07^-1

for (i in 4:7) {
  d_factors[i+1] <- d_factors[i]*1.07^-1
}

d_factors[9] <- 1.06^-5*1.07^-5*1.08^-1

for (i in 9:19) {
  d_factors[i+1] <- d_factors[i]*1.08^-1
}

d_factors

discount <- function(s, t, i) {(1 + i) ^ - (t - s)}

PV_loan <- sum(cash_flow_loan * discount(0, 0:2, i = 0.06))

PV_payments <- sum(cash_flow_vector * d_factors)

k1 <- PV_loan/PV_payments; k1

yearly_payments <- cash_flow_vector*d_factors*k1; yearly_payments

sum(yearly_payments)

#3

i <- 5.5/100

##i_monthly <- (1+i)^(1/12) - 1

discount2 <- function(s, t, i = i_monthly) {(1 + i) ^ - (t - s)}

cash_flow <- c(rep(1, 180))

PV <- sum(cash_flow * discount2(0, 1:180))

150000/PV

#OR

i <- 5.5/100

# i_monthly <- (1+i)^(1/12) - 1

i_monthly <- i/12

discount_factors <- (1 + i_monthly) ^ - (1:180)

cash_flow <- rep(1, 180)

PV <- sum(cash_flow * discount_factors)

k <- 150000/PV; k

#Bonus Q

p <- cash_flow*discount_factors*k; sum(p)

p <- rev(p)

max <- 12
y <- seq_along(p)
chunks <- split(p, ceiling(y/max))

a <- c(150000)

for (i in 1:15) {
  a[i+1] <- a[i] - sum(chunks[[i]])
}

round(a, 2)