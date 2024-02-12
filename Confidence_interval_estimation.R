#question 1
V = matrix(0.8, 5, 5) #fills initial matrix with 0.8
#i and j matrices for matrix operation
i = matrix(rep(c(1, 2, 3, 4, 5), times = 5), nrow = 5)
j = matrix(rep(c(1, 2, 3, 4, 5), times = 5), nrow = 5, byrow = TRUE)
#creating three desired matrices
V = V ^ abs(i - j)
VTranspose = t(V)
VInverse = solve(V)
#placing matrices into 5x5x3 array
(arr = round(array(c(V, VTranspose, VInverse), dim = c(5, 5, 3), 
                   dimnames = list(NULL, NULL, c("V Matrix", "V Transpose", "V Inverse"))), 4))

#question 2
#a.
#reading the data using given code
dat.cig = read.table("cigarettes.txt", col.names = c("brand", "tar", "nicotine", "weight", "CO"))
head(dat.cig, 3) #displaying first and last 3 entries
tail(dat.cig, 3)
#b.
tarSD = sd(dat.cig$tar) #standard deviation for tar column
n = length(dat.cig$tar)
zVal = qnorm(0.015, lower.tail = FALSE) #0.015 = 0.03 / 2

XBar = (n ^ -1) * sum(dat.cig$tar)
#calculating upper and lower values of confidence interval
lower = XBar - (zVal * (tarSD / sqrt(n)))
upper = XBar + (zVal * (tarSD / sqrt(n)))
(CI = c(lower, upper)) #combining into one vector

#c.
library(Rmisc)
CI(dat.cig$tar, ci = 0.97)
#this CI does not exactly agree with the one calculated in part
#b, however the result is close. The interval calculated in part
#c is slightly wider.

#d.
tVal = qt(0.015, n - 1, lower.tail = FALSE)
newLower = XBar - (tVal * (tarSD / sqrt(n)))
newUpper = XBar + (tVal * (tarSD / sqrt(n)))
(newCI = c(newLower, newUpper))
#We should use this formula instead of the previous one in the case of a small sample size or
#we have an unknown standard deviation for the population.
#The values calculated by hand now match the CI() function. From this calculation,
#we are 97% certain the true population mean lies between 9.602 and 14.830

#e.
qqnorm(dat.cig$tar)
qqline(dat.cig$tar, lwd = 2) #plotting the straight line for reference
#since the points roughly fall on the line, the assumption of normality is satisfied

#f.
shapiro.test(dat.cig$tar)
#the p-value is 0.03622. As a result of p > 0.03, we fail to reject the null hypothesis
#that the data is normally distributed.

#g.
boxplot(dat.cig$tar, horizontal = TRUE, main = "Box and whisker plot for tar content", xlab = "Tar amount")
#There is one observation shown separately as a result of it being an outlier. This is the only tar value of 30