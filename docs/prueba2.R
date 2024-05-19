vector= c(1973.1,1961.53,1993.23,1937.27,2044.57,2054.27,2028.18,2082.2,2079.99,2094.86,2111.94,2099.29,2094.14,2039.87,1944.41,2024.81,2080.62,2054.08,1918.6,1904.42,2021.95,2075.54,2065.55,2083.89,2148.9,2170.95,2157.69,2143.02,2164.99,2246.63,2275.12,2329.91,2366.82,2359.31,2395.35,2433.99)
data=ts(vector,start=c(2014,7),frequency=12)
datacomp <- decompose(data)
plot(datacomp)

library(aTSA)

data1 <- rnorm(50)
data_ts <- ts(data1,start=c(2014,7),frequency=12)
library(aTSA)
data.adf <- adf.test(data_ts)


require(graphics)
plot(airmiles, main = "airmiles data",
     xlab = "Passenger-miles flown by U.S. commercial airlines", col = 4)
