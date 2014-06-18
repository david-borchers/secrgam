
lognormal.CI.Nhat = function(n, Nhat, cv){

  varNhat = (Nhat * cv)^2
  cfactor = exp(1.96 * sqrt(log(1 + varNhat / (Nhat-n)^2)))
  lower = n + (Nhat-n) / cfactor
  upper = n + (Nhat-n) * cfactor
  
  return(list(lower = lower, est = Nhat, upper = upper))

}
