library(deSolve)

priceInit = c(price1 = 100)
times = seq(1, 20, by = 1)

de = function(t, priceInit, pars){
  with(as.list(c(pars, priceInit)),{
    #cumulative distribution function
    if (currentPrice - price1 < minDiscount){
      cumulativePrice = 0.0
    }else if(currentPrice - price1 >= maxDiscount){
      cumulativePrice = 1.0
    }else{
      cumulativePrice = (currentPrice - price1 - minDiscount)/(maxDiscount - minDiscount)
    }
    #density function
    if (currentPrice - price1 < minDiscount){
      densityPrice = 0.0
    }else if(currentPrice - price1 >= maxDiscount){
      densityPrice = 0.0
    }else{
      densityPrice = 1/(maxDiscount - minDiscount)
    }
    
    leftPart = cumulativePrice*arrivalTime*exp(-arrivalTime*t)
    upp = arrivalTime*(currentPrice - price1 - minDiscount)*exp(-arrivalTime*t)
    down = 1 - exp(-arrivalTime*t)
    dp1 = leftPart/(down*densityPrice)
    dp = upp/down
    
    upp1 = arrivalTime*(currentPrice - price1)*exp(-arrivalTime*t)
    down1 = 1 - exp(-arrivalTime*t)
    dp2 = upp1/down1
    
    return(list(dp))
  })
}

pars = c(currentPrice = 100.0, 
         minDiscount = 80.0,
         maxDiscount = 80.0,
         arrivalTime = 0.5)
out = ode(y = priceInit, times = times, func = de, parms = pars)
plot(out, times)


