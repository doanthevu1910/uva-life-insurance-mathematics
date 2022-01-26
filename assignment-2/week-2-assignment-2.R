setwd("D:/UvA/BSc Actuarial Science/Year 2/Block 1/Life Insurance Mathematics/Week 2 - Life tables and selection, Valuation of life insurance benefits/Assignment 2")

getwd()

rm(list = ls())

#2

life_table <- read.table("Belgium.txt", header = TRUE)

head(life_table)

#a. µx,t in year t, versus x, for different t (t = 1841, 1900, 1950, 2000, 2016)

#year 1841

life_table_1841 <- subset(life_table, Year == 1841)

qx_1841 <- as.numeric(life_table_1841$qx)

age_1841 <- as.numeric(life_table_1841$Age)

px_1841 <- 1 - qx_1841

mx_1841 <- -log(px_1841)

plot(age_1841, mx_1841, type = "l", 
     xlab = "Age", ylab = "Force of mortality", 
     main = "µx,t versus x, for year 1841, 1900, 1950, 2000, 2015",
     lwd = 2)

#year 1900

life_table_1900 <- subset(life_table, Year == 1900)

qx_1900 <- as.numeric(life_table_1900$qx)

age_1900 <- as.numeric(life_table_1900$Age)

px_1900 <- 1 - qx_1900

mx_1900 <- -log(px_1900)

lines(age_1900, mx_1900, type = "l", col = "red", lwd = 2)

#year 1950

life_table_1950 <- subset(life_table, Year == 1950)

qx_1950 <- as.numeric(life_table_1950$qx)

age_1950 <- as.numeric(life_table_1950$Age)

px_1950 <- 1 - qx_1950

mx_1950 <- -log(px_1950)

lines(age_1950, mx_1950, type = "l", col = "green", lwd = 2)

#year 2000

life_table_2000 <- subset(life_table, Year == 2000)

qx_2000 <- as.numeric(life_table_2000$qx)

age_2000 <- as.numeric(life_table_2000$Age)

px_2000 <- 1 - qx_2000

mx_2000 <- -log(px_2000)

lines(age_2000, mx_2000, type = "l", col = "blue", lwd = 2)

#year 2015

life_table_2015 <- subset(life_table, Year == 2015)

qx_2015 <- as.numeric(life_table_2015$qx)

age_2015 <- as.numeric(life_table_2015$Age)

px_2015 <- 1 - qx_2015

mx_2015 <- -log(px_2015)

lines(age_2015, mx_2015, type = "l", col = "yellow", lwd = 2)

#b. µx,t versus t, for different selected ages x

#age 0

life_table_0 <- subset(life_table, Age == 0)

qx_0 <- as.numeric(life_table_0$qx)

mx_0 <- -log(1 - qx_0)

plot(life_table_0$Year, mx_0, type = "l", 
     xlab = "Year", ylab = "Force of mortality",
     ylim = c(0, 0.8), lwd = 2, 
     main = "µx,t versus t, for age 0, 20, 40, 60, 80, 100")

#age 20

life_table_20 <- subset(life_table, Age == 20)

qx_20 <- as.numeric(life_table_20$qx)

mx_20 <- -log(1 - qx_20)

lines(life_table_20$Year, mx_20, type = "l", col = "red", lwd = 2)

#age 40

life_table_40 <- subset(life_table, Age == 40)

qx_40 <- as.numeric(life_table_40$qx)

mx_40 <- -log(1 - qx_40)

lines(life_table_40$Year, mx_40, type = "l", col = "green", lwd = 2)

#age 60

life_table_60 <- subset(life_table, Age == 60)

qx_60 <- as.numeric(life_table_60$qx)

mx_60 <- -log(1 - qx_60)

lines(life_table_60$Year, mx_60, type = "l", col = "blue", lwd = 2)

#age 80

life_table_80 <- subset(life_table, Age == 80)

qx_80 <- as.numeric(life_table_80$qx)

mx_80 <- -log(1 - qx_80)

lines(life_table_80$Year, mx_80, type = "l", col = "yellow", lwd = 2)

#age 100

life_table_100 <- subset(life_table, Age == 100)

qx_100 <- as.numeric(life_table_100$qx)

mx_100 <- -log(1 - qx_100)

lines(life_table_100$Year, mx_100, type = "l", col = "orange", lwd = 2)

#c. S0,t(x)

#year 1841

#xp0 for x = 1:110

cumprod(px_1841[(0+1):(109+1)])

plot(1:110, cumprod(px_1841[(0+1):(109+1)]), 
     type = "l", ylim = 0:1,
     xlab = "Age", ylab = expression(paste("S"["0, t"](x))), lwd = 2,
     main = expression(paste("S"["0, t"](x), " for 1841, 1900, 1950, 2000, 2015"))) 

#year 1900

cumprod(px_1900[(0+1):(109+1)])

lines(1:110, cumprod(px_1900[(0+1):(109+1)]), 
      type = "l", col = "red", lwd = 2)

#year 1950

cumprod(px_1950[(0+1):(109+1)])

lines(1:110, cumprod(px_1950[(0+1):(109+1)]), 
      type = "l", col = "green", lwd = 2)

#year 2000

cumprod(px_2000[(0+1):(109+1)])

lines(1:110, cumprod(px_2000[(0+1):(109+1)]), 
      type = "l", col = "blue", lwd = 2)

#year 2015

cumprod(px_2015[(0+1):(109+1)])

lines(1:110, cumprod(px_2015[(0+1):(109+1)]), 
      type = "l", col = "yellow", lwd = 2)

#d.

sum(cumprod(px_1841[(0+1):(109+1)]))

sum(cumprod(px_2015[(0+1):(109+1)]))

e_0 <- NULL

for (i in seq(1841, 2015)) {
   
   life_table_i <- subset(life_table, Year == i)

   px_i <- 1 - as.numeric(life_table_i$qx)
   
   add <- sum(cumprod(px_i[(0+1):(109+1)]))
   
   e_0 <- c(e_0, add)

}

plot(1841:2015, e_0, type = "l", lwd = 2,
     xlab = "Year", ylab = expression(paste("e"[0])),
     main = "Life expectancy for newborns (1841-2015)")

e_20 <- NULL

for (i in seq(1841, 2015)) {
   
   life_table_i <- subset(life_table, Year == i)
   
   px_i <- 1 - as.numeric(life_table_i$qx)
   
   add <- sum(cumprod(px_i[(20+1):(109+1)]))
   
   e_20 <- c(e_20, add)
   
}

lines(1841:2015, 20+e_20, type = "l", lwd = 2, col = "red")

e_40 <- NULL

for (i in seq(1841, 2015)) {
   
   life_table_i <- subset(life_table, Year == i)
   
   px_i <- 1 - as.numeric(life_table_i$qx)
   
   add <- sum(cumprod(px_i[(40+1):(109+1)]))
   
   e_40 <- c(e_40, add)
   
}

lines(1841:2015, 40+e_40, type = "l", lwd = 2, col = "green")

plot(1:20, 1:20, ylab = expression(paste("S"["0, t"](x))))