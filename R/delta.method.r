
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
  for(i in seq_along(Dbeta)) eval(parse(text = paste0(colnames(X)[i]," = Dbeta[",i,"]")))
  
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
