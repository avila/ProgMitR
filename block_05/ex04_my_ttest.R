myt.test <- function(x, y) {
  # two sample t.test
  
  # x : 
  xbar <- mean(x)
  ybar <- mean(y)
  n <- length(x)
  m <- length(y)
  sigma_xsq <- var(x)
  sigma_ysq <- var(y)
  df <- n + m - 2
  
  t <- (xbar - ybar) / sqrt((1/n + 1/m) * ((n - 1) * sigma_xsq + (m - 1) * sigma_ysq) / (df))
  pvalue <- pt(t,df)
  
  return(list(t_stat = t,
              df = df,
              pvalue = pvalue))
}

x <- sleep$extra[sleep$group == 1]
y <- sleep$extra[sleep$group == 2]

myt.test(x, y)
#with(sleep, t.test(extra[group == 1], extra[group == 2]))
a <- t.test(x,y)

