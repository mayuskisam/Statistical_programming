#question 1
#1f. (manual chi-square test)
chiSquareFun = function(dat, res.type = "pearson") {
  #setting variables for use in calculations later
  colTotals = colSums(dat)
  rowTotals = rowSums(dat)
  n = sum(dat)
  expected = (rowTotals %*% t(colTotals)) / n
  observed = dat
  
  if (length(expected[expected < 5]) > 0) { #if any expected counts are less than 5
    warning("Approximation may be inaccurate since at least one expected count < 5")
  }
  #calculating the appropriate residuals based on function input
  if (res.type == "pearson") {
    residuals = (observed - expected) / sqrt(expected)
  } else if (res.type == "std") {
    residuals = (observed - expected) / sqrt(expected * ((1 - rowTotals) / n) * ((1 - colTotals) / n))
  }
  #computing the test statistic and p-value
  Xval = sum((observed - expected) ^ 2 / expected)
  pVal = pchisq(Xval, df = (nrow(dat) - 1) * (ncol(dat) - 1), lower.tail = FALSE)
  
  returnVals = list(Xval, pVal, expected, res.type, residuals)
  #naming the returned list so it becomes easy to read
  names(returnVals) = c("Test Statistic", "p-value", "Expected Counts", "Residual Type", "Residuals")
  return(returnVals)
}

#1g.
#creating the data as a matrix and naming the rows/columns
rawData = matrix(c(6, 18, 13, 115, 256, 136, 256, 442, 155), nrow = 3, byrow = TRUE)
colnames(rawData) = c("Single/Widowed", "Married", "Divorced")
rownames(rawData) = c("Elementary", "Secondary", "College")

#converting the data to a table and using the previously written function on it
testData = as.table(rawData)
chiSquareFun(testData, res.type = "std")

#question 2 (manual F test)
#2e.
FTestFun = function(x1, x2, alt = "two-sided", lev = 0.95) {
  #defining values used in calculations later
  s1 = sd(x1)
  s2 = sd(x2)
  df.1 = length(x1) - 1
  df.2 = length(x2) - 1
  #computing the test statistic and running shapiro-wilk tests on each dataset
  Fvalue = s1 ^ 2 / s2 ^ 2
  shapiro1 = shapiro.test(x1)
  shapiro2 = shapiro.test(x2)
  #outputting warning messages if either of the p-values returned is < 0.05
  if (shapiro1[[2]] < 0.05 || (shapiro2[[2]] < 0.05)) {
    warning("p-value for one or both Shapiro-Wilk tests is < 0.05. The data deviates from a normal distribution.")
  }
  
  #calculating the p-value based on the given function input
  if (alt == "two-sided") {
    pValue = 2 * min(pf(Fvalue, df.1, df.2), pf(Fvalue, df.1, df.2, lower.tail = FALSE))
  } else if (alt == "less") {
    pValue = pf(Fvalue, df.1, df.2)
  } else if (alt == "greater") {
    pValue = pf(Fvalue, df.1, df.2, lower.tail = FALSE)
  }
  #calculating the confidence interval
  lower = s1 ^ 2 / (s2 ^ 2 * qf((1 - lev) / 2, df.1, df.2, lower.tail = FALSE))
  upper = s1 ^ 2 / (s2 ^ 2 * qf((1 - lev) / 2, df.1, df.2))
  CI = c(lower, upper)
  
  returnVals = list(Fvalue, pValue, 100 * lev, CI)
  #adding names to returned list for ease of reading
  names(returnVals) = c("Test Statistic", "p-value", "Confidence Level", "Confidence Interval")
  return(returnVals)
}
#2f.
#creating vectors to hold the given data and running the previously defined function on the samples
company1 = c(136, 129, 143, 110, 122, 128, 137, 140, 92, 107)
company2 = c(182, 245, 138, 142, 119, 131, 116, 142)
FTestFun(company1, company2, alt = "two-sided", lev = 0.99)