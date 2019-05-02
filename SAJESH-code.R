# Task 1.ii -> reading the data as a table and converting to matrix using matrix function
data = as.matrix(read.table('./Energy19.txt'))

# Task 1.iii -> subset of 300 data from all the data provided
subsetData = data[1:300,]

# Task 1.iv -> Scatter plots and histograms
plot(subsetData[,6],  subsetData[,1], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Temperature in kitchen (Celcius)", col = "blue", pch = 4 )

plot(subsetData[,6], subsetData[,2], type = "p", xlab = "Energy use of appliances in Wh",  ylab = "Humidity in kitchen", col = "blue", pch = 4 )

plot(subsetData[,6], subsetData[,3], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Temperature outside of kitchen (Celcius)", col = "blue", pch = 4)

plot(subsetData[,6], subsetData[,4], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Humidity outside", col = "blue", pch = 4 )

plot( subsetData[,6], subsetData[,5], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Visibility", col = "blue", pch = 4 )

# Histograms
hist(subsetData[,1], main = "Histogram", xlab = "Temperature in kitchen", col = "skyblue")
hist(subsetData[,2], main = "Histogram", xlab = "Humidity in kitchen", col = "skyblue")
hist(subsetData[,3], main = "Histogram", xlab = "Temperature outside", col = "skyblue")
hist(subsetData[,4], main = "Histogram", xlab = "Humidity outside", col = "skyblue")
hist(subsetData[,5], main = "Histogram", xlab = "Visibility", col = "skyblue")
hist(subsetData[,6], main = "Histogram", xlab = "Energy use", col = "skyblue")

# Task 2.i -> Data transformation
# Visibility from weather station seems like unreleated data as appliances works in or around the house that
# it belongs to. Since, task also does not specifies any criteria to choose the variables. I am ignoring
# 5th variable from this point onwards. Ofcourse correlations can be tested with Y so that variable with 
# least correlation can be ignored. But, I will just ignore 5th variable.
selectedData = subsetData[,-5]

# before transformation checking the skewness of each variables
library(e1071)
apply(selectedData, 2, skewness)
#        V1         V2         V3         V4         V6 
#   0.2841859  0.2500231 -0.1818761 -0.1063660  1.6146097 

scaleData = function (temp) {
  xScaled = (temp - min(temp)) / (max(temp) - min(temp))
  
  return (xScaled)
}

# Applying cube root transformation for all the variables
normalizedData = (selectedData) ^ (1/3)
# Nans were produed for 3rd variable as there might be values lower than 0 which wont work with log function
# Since, variable 3 is already not heavily skewed as we checked before with skewness function I will just keep
# the original value here
normalizedData[,3] = selectedData[,3]

apply(normalizedData, 2, skewness)
#   V1          V2          V3          V4          V6 
#0.15144969  0.05133773 -0.18187609 -0.27646129  0.76731481 
# Looks like we skewed fourth variable even more with cube root transformation so I will use original 
# value for this variable as well
normalizedData[,4] = selectedData[,4]

# Since Y is heavily skewed we can use log function for this variable to make it more normally distributed
# using log transformation for Y instead of cube root transformation
normalizedData[,5] = log(selectedData[,5])

skewness(normalizedData[,5]) #0.3336642

#Transforming all the data within 0-1 range
scaledData = apply(normalizedData, 2, scaleData)

write.table(scaledData, "SAJESH-transformed.txt", col.names = FALSE, append = FALSE, sep=",", row.names = FALSE)

# Task 3.i 
#
source(file = "./AggWaFit718.R")

#Task 3.ii  -> Models
# output directory is required in the project directory
fit.QAM(scaledData, "./output/output.txt", "./output/status.txt" )
fit.QAM(scaledData, "./output/output1.txt", "./output/status1.txt", g=PM05, g.inv = invPM05)
fit.QAM(scaledData, "./output/output2.txt", "./output/status2.txt", g=QM, g.inv = invQM)
fit.OWA(scaledData, "./output/output3.txt", "./output/status3.txt")
fit.choquet(scaledData, "./output/output4.txt", "./output/status4.txt")

# Task 4.i
input = c(18,44,4,74.8) ^ (1/3)
weights = read.table("weights")[,2]

Y = choquet(input, weights)
(Y) ^ 3 #16.27918

