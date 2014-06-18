
# rm(list = ls()

suppressPackageStartupMessages(require(splines))

## *****************************************************************************
## *****************************************************************************
##
##                                  function
##
## *****************************************************************************
## *****************************************************************************

delta.method.density.variance = function(formula, data, beta, Sigma, type = c("individual", "mean"), weights = NULL){
    
    type = match.arg(type)
    
    n = nrow(data) ; n
    
    # if 'weights' is NULL or a scalar, then convert to a vector of equal weights
    # if 'weights' is a vector, then make sure it has the right length and that it sums to one
    if(type == "mean"){
        if(is.null(weights) || length(weights) == 1){
            weights = rep(1, n)
        }else{
            if(length(weights) != n) 
                stop("'weights' must be a scalar or a numeric vector with length equal to nrow(data)")
        }
        weights = weights / sum(weights)
    } ; weights
    
    # remove LHS from formula
    if(length(formula) == 3) formula[2] = NULL ; formula
    
    # make the design matrix
    X = as.matrix(model.frame(formula, data)) ; head(X)
        
    # define the individual beta values
    for(i in seq_along(beta)) eval(parse(text = paste0(colnames(X)[i]," = beta[",i,"]")))
        
    # make a function to calculate either:
    # the individual predictions (if type = "individual"), or
    # a weighted mean of the indivdual predictions (if type = "mean") 
    f.args = paste(colnames(X), collapse = ", ") ; f.args
    f.call = paste0("f(",f.args,")") ; f.call
    f.full = switch(
        type,
        "individual" = paste0("function(",f.args,") as.numeric(X %*% c(",f.args,"))"),
        "mean"       = paste0("function(",f.args,") as.numeric(weights %*% (X %*% c(",f.args,")))")
    ) ; f.full
    f = eval(parse(text = f.full)) ; f ; eval(parse(text = f.call))
    
    # calculate gradient matrix
    grad = attr(numericDeriv(quote(eval(parse(text = f.call))), colnames(X)), "gradient") ; grad
    
    # for each row of the grad matrix, calulate the variance of f(beta)
    # each row corresponds to a row in the design matrix
    sapply(1:nrow(grad), function(i) t(grad[i, ]) %*% Sigma %*% grad[i, ])
    
}

delta.method = function(X, Dbeta, Cov, type = c("individual", "mean"), link = c("identity", "log"), weights = NULL){
  
  type = match.arg(type)
  
  link = match.arg(link)

  # calculate weights
  # if 'weights' is NULL or a scalar, then convert to a vector of equal weights
  # if 'weights' is a vector, then make sure it has the right length and that it sums to one
  n = nrow(X) ; n
  if(type == "mean"){
    if(is.null(weights) || length(weights) == 1){
      weights = rep(1, n)
    }else{
      if(length(weights) != n) 
        stop("'weights' must be a scalar or a numeric vector with length equal to nrow(X)")
    }
    weights = weights / sum(weights)
  } # weights
  
  # check colnames X match names Dbeta
  if(is.null(colnames(X)) || is.null(names(Dbeta)) || !all(colnames(X) == names(Dbeta)))
    stop("colnames(X) must match names(Dbeta)", call. = FALSE)
  
  # define the individual beta values
  for(i in seq_along(Dbeta)) eval(parse(text = paste0(colnames(X)[i]," = beta[",i,"]")))
  
  # make a function to calculate either:
  # the individual predictions (if type = "individual"), or
  # a weighted mean of the indivdual predictions (if type = "mean") 
  f.args = paste(colnames(X), collapse = ", ") # f.args
  f.call = paste0("f(",f.args,")") # f.call
  f.body = paste0(
    "as.numeric(",
    if(type == "mean") "weights %*% ",
    if(link == "log") "exp(", 
    "X %*% c(",f.args,")",
    if(link == "log") ")", 
    ")"
  ) # f.body
  f = paste0("function(",f.args,"){",f.body,"}")
  f = eval(parse(text = f)) # f
  
  # calculate the gradient matrix
  grad = attr(numericDeriv(quote(eval(parse(text = f.call))), colnames(X)), "gradient") ; grad
  
  # for each row of the grad matrix, calulate the variance of f(beta)
  # each row corresponds to a row in the design matrix
  sapply(1:nrow(grad), function(i) t(grad[i, ]) %*% Cov %*% grad[i, ])
  
}


lnci.nmin = function(n, Nhat, cv){

  varNhat = (Nhat * cv)^2
  cfactor = exp(1.96 * sqrt(log(1 + varNhat / (Nhat-n)^2)))
  lower = n + (Nhat-n) / cfactor
  upper = n + (Nhat-n) * cfactor
  
  return(list(lower = lower, est = Nhat, upper = upper))

}

## *****************************************************************************
## *****************************************************************************
##
##                        test: 1D regression spline
##
## *****************************************************************************
## *****************************************************************************

#-------------------------------------------------------------------------------
# make some data

# number of data points
n = 100 

# limits of the x-axis
# need to explicitly spcify this to make sure the bs function uses the same boundary knots each time
xlim = c(0,6) 

# generate some x values - e.g. ID mask values
x = seq(xlim[1], xlim[2], length = n) # regular

# generate some observations
y = rnorm(n, mean = sin(x), sd = 0.5)

# plot the data
par(mfrow = c(1,3))
plot(x, y, pch = 19, col = "grey", main = "Example data", cex = 0.5)
lines(x, sin(x), cex = 0.5, pch = 19, lwd = 2)
legend('topright', "true model", lty = 1)

#-------------------------------------------------------------------------------
# lm model

# make a data frame of regression spline basis values
data = as.data.frame(bs(x, df = 5, degree = 3, intercept = TRUE, Boundary.knots = xlim)) ; colnames(data) = paste0("X", 1:ncol(data)) ; head(data)

# use an explicit formula
formula = y ~ -1 + X1 + X2 + X3 + X4 + X5

# fit model with lm and save fitted betas and vcov matrix
model = lm(formula, data)
beta = coef(model)
Sigma = vcov(model)

#-------------------------------------------------------------------------------
# predictions

# make a 1D prediction grid
n = 20
newx = seq(xlim[1], xlim[2], length = n) # regular

# width of intervals
w = newx[2] - newx[1] ; w

# data frame of covariate values to use for prediction
newdata = as.data.frame(bs(newx, df = 5, degree = 3, intercept = TRUE, Boundary.knots = xlim)) ; colnames(newdata) = paste0("X", 1:ncol(newdata)) ; head(newdata)

# get predictions from lm model
preds = predict(model, newdata, se = TRUE)

# plot the predications and the 95CLs for the predictions
plot(x, y, pch = 19, col = "grey", main = "95% CI for predictions", cex = 0.5)
# lines(newx, preds$fit, col = 2, lwd = 2)
lines(newx, preds$fit - 1.96 * preds$se.fit, col = 2, lwd = 2)
lines(newx, preds$fit + 1.96 * preds$se.fit, col = 2, lwd = 2)

#-------------------------------------------------------------------------------
# use the delta method function as an alternative way to get the variance of each prediction

var1 = delta.method.density.variance(formula, newdata, beta, Sigma) ; var1

# check if same as estimated variances from lm
all.equal(sqrt(var1), preds$se.fit, tolerance = 1e-7, check.attributes = FALSE)

lines(newx, preds$fit - 1.96 * sqrt(var1), col = 4, cex = 0.5, lty = 2, lwd = 2)
lines(newx, preds$fit + 1.96 * sqrt(var1), col = 4, cex = 0.5, lty = 2, lwd = 2)
legend('topright', c("delta method", "lm"), lty = 1, col = c(4,2))

#-------------------------------------------------------------------------------
# variance of the mean of the predictions, weighted by interval width, assuming independence

# calulate weighted mean
w = rep(w, n)
weights = w / sum(w)
weighted.mean = as.numeric(weights %*% preds$fit) ; weighted.mean

# variance of weighted mean assuming independence
var.weighted.mean = as.numeric(weights^2 %*% preds$se.fit^2) ; var.weighted.mean
sum(preds$se.fit^2)/n^2 # should equal this if all weights are equal


plot(x, y, pch = 19, col = "grey", main = "95% CI for mean of predictions", cex = 0.5)
# abline(h = weighted.mean, col = 2)
abline(h = weighted.mean - 1.96 * sqrt(var.weighted.mean), col = 2, lwd = 2)
abline(h = weighted.mean + 1.96 * sqrt(var.weighted.mean), col = 2, lwd = 2)
legend('topright', c("delta method", "independence"), lty = 1, col = c(4,2))

#-------------------------------------------------------------------------------
# delta method variance of the weighted mean of the predictions, weighted by interval width, not assuming independence

var2 = delta.method.density.variance(formula, newdata, beta, Sigma, "mean", weights) ; var2
abline(h = weighted.mean - 1.96 * sqrt(var2), col = 4, lty = 2, lwd = 2)
abline(h = weighted.mean + 1.96 * sqrt(var2), col = 4, lty = 2, lwd = 2)


## *****************************************************************************
## *****************************************************************************
## *****************************************************************************
## *****************************************************************************


