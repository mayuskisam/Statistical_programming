#question 1
#a.
(A = matrix(1, 3, 3)) #initializing matrix of all 1s
division = c(1, 2, 3)
(A = t(t(A) / c(1:9))) #calculating using transpose twice
#b.
(B = A[1:2, 1:2]) #getting first two rows/cols
#c.
(C = diag(diag(A)))
#d.
newCol = c(4, 4, 4) 
(D = cbind(C, newCol)) #binding column of 4s
newRow = c(5, 5, 5, 5)
(D = rbind(D, newRow)) #binding row of 5s

#question 2
#initializing matrices for calculation
a = matrix(c(2, 4, 6), nrow = 1)
b = t(matrix(c(3, 2, 5), nrow = 1))
C = matrix(c(3, 6, 2, 4), nrow = 2)
D = matrix(c(3, 8, 8, 0), nrow = 2)
E = matrix(c(3, 2, 4, 0, 1, 6, 0, 2), nrow = 2)
F = matrix(c(0, 1, 3, 2, 4, 0, 6, 0, 1), nrow = 3)
G = matrix(c(2, 6, 3, 6, 4, 1, 3, 1, 7), nrow = 3)

#performing matrix calculations
#a.
((2 * C) + (4 * D))
#b.
((4 * F) - G)
#c.
(t(a) + (2 * b))
#d.
(a + (2 * t(b)))
#e.
((F %*% t(a)) + b) 
#f.
(E %*% t(E))
#g.
(t(E) %*% E)
#h.
(a %*% t(a))


#question 3
#initializing given matrices for later calculation
y = matrix(c(1, 2, 2), nrow = 3)
x = matrix(c(1, 2, 3), nrow = 3)
X = matrix(c(1, 1, 1, 1, 2, 3), nrow = 3)
#a.
initial = t(X) %*% X
inverse = solve(initial) #calculating inverse of 2x2 result
(lineValues = (inverse %*% (t(X) %*% y))) #finding values for a and b
#b.
plot(x, y, ylim = range(0:5)) #plotting initial scatter plot

#adding line based on calculated matrix from part a
abline(a = lineValues[1, 1], b = lineValues[2, 1], lty = 2, col = 'blue')
pointValues = lineValues[2, 1] * x + lineValues[1, 1]
points(x, pointValues, pch = 17, col = "red") #adding points on scatter plot
#c.
#values of yHat used for correlation calculation
yHatBar = mean(pointValues)
yHatSquared = sum((pointValues - yHatBar) ^ 2)

#values of y used for correlation calculation
yBar = mean(y)
ySquared = sum((y - yBar) ^ 2)

corSum = sum((pointValues - yHatBar) * (y - yBar))
(correlation = corSum / sqrt(yHatSquared * ySquared)) #calculating correlation

#correlations using built in functions
(cor(pointValues, y))
(cor(x, y)) 
#the correlation values are the same between x and y and yHat and y

#question 4
#a.
U = matrix(runif(5000, 2, 8), nrow = 1000, ncol = 5) #generating uniform distribution
head(U)
(sampleMean = round(apply(U, 2, mean), 2)) #estimates mean of population based on sample columns
(sampleVar = round(apply(U, 2, var), 2)) #estimates variance of population based on sample columns
#b.
round(apply((sweep(U, 2, apply(U, 2, mean))) ^ 2, 2, mean) * (1000 / 999), 2) 
# * 1000/999 to account for n - 1 division