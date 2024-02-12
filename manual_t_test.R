oneSidedT = function(x, mu0 = 0, alt = "two-sided", lev = 0.95) {
  #initializing values for computation based on input
  xBar = mean(x)
  variance = var(x)
  n = length(x)
  tStar = (xBar - mu0) / sqrt(variance / n)
  
  #determining the p value based on "alt" argument
  if (alt == "less") {
    pValue = pt(tStar, df = n - 1)
  } else if (alt == "greater") {
    pValue = pt(tStar, df = n - 1, lower.tail = FALSE)
  } else {
    pValue = 2 * pt(abs(tStar), df = n - 1, lower.tail = FALSE)
  }
  #computing the confidence interval
  CI = xBar + c(-1, 1) * qt((1 - lev) / 2, df = n - 1, lower.tail = FALSE) * sqrt(variance / n)
  returnVals = list(tStar, pValue, CI)
  #naming levels for accessing later
  names(returnVals) = c("Test Statistic", "p-value", "Confidence Interval")
  return (returnVals)
}

#f.
datasets = matrix(rnorm(100000), ncol = 20, byrow = TRUE) #generating the data

#conducting the six tests using the generated data
test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 0, alt = "two-sided", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)

test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 1, alt = "two-sided", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)

test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 0, alt = "less", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)

test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 1, alt = "less", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)

test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 0, alt = "greater", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)

test = apply(datasets, 1, function(x) {
  vals = oneSidedT(x, mu = 1, alt = "greater", lev = 0.95)
  return (vals[["p-value"]])})
(proportion = length(test[test < 0.05]) / 5000)