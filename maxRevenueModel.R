library(deSolve)

priceInit = c(price1 = 100)
times = seq(1, 10, by = 1)

de = function(t, priceInit, pars){
  with(as.list(c(pars, priceInit)),{
    
    denominator = 1/(maxDiscount - minDiscount)
    expdistr = 1 - exp(-arrivalTime*t)
    undist = (currentPrice - price1 - minDiscount)
    rightPart = undist*denominator*expdistr - price1*denominator*expdistr
    
    leftPart = fixedCosts - price1*undist*denominator*arrivalTime*exp(-arrivalTime*t)
      
    dp = leftPart/rightPart
    
    return(list(dp))
  })
}

pars = c(currentPrice = 100.0, 
         minDiscount = 0.0,
         maxDiscount = 50.0,
         arrivalTime = 0.4,
         fixedCosts = 5)
out = ode(y = priceInit, times = times, func = de, parms = pars)
plot(out, times)


