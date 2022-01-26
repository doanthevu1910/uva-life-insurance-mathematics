setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 1/Life Insurance Mathematics/Week 1/Assignment 1")

getwd()

rm(list = ls())

#2 

cash_flow_loan <- c(10000, 20000, 30000)

cash_flow_vector <- c(1)

for (i in 1:19) {
  cash_flow_vector[i+1] <- cash_flow_vector[i]*(1-0.05) 
}

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

discount <- function(s, t, i) {(1 + i) ^ - (t - s)}

PV_loan <- sum(cash_flow_loan * discount(0, 0:2, i = 0.06)); PV_loan

PV_payments <- sum(cash_flow_vector * d_factors)

k1 <- PV_loan/PV_payments

yearly_payments <- cash_flow_vector*k1; yearly_payments

yearly_payments*d_factors

#3

i <- 5.5/100

# i_monthly <- (1+i)^(1/12) - 1

i_monthly <- i/12

discount_factors <- (1 + i_monthly) ^ - (1:180)

cash_flow <- rep(1, 180)

PV <- sum(cash_flow * discount_factors)

k2 <- 150000/PV; k2

#Bonus Q

p <- cash_flow*discount_factors*k2

p <- rev(p)

max <- 12
y <- seq_along(p)
chunks <- split(p, ceiling(y/max))

a <- c(150000)

for (i in 1:15) {
  a[i+1] <- a[i] - sum(chunks[[i]])
}

round(a, 2)

#OR

p2 <- cash_flow*discount_factors*k2

b <- c(150000)

for (i in 1:180) {
  b[i+1] <- b[i] - rev(p2)[i]
}

round(b[c(seq(5, 180, by=12), 181)], 2)

seq(5, 180, by=12)