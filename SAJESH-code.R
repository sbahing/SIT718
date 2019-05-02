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

cor(subsetData) # Viewing correlation of each variale with Y (V6)
#          V1          V2          V3          V4          V5        V6
# V1  1.00000000  0.04815762 -0.06176143  0.03683993  0.17281179 0.3422393
# V2  0.04815762  1.00000000  0.38211440 -0.10955013  0.07361037 0.1308550
# V3 -0.06176143  0.38211440  1.00000000 -0.48099781  0.18335703 0.4311620
# V4  0.03683993 -0.10955013 -0.48099781  1.00000000 -0.09163847 0.0743019
# V5  0.17281179  0.07361037  0.18335703 -0.09163847  1.00000000 0.2835517
# V6  0.34223927  0.13085504  0.43116199  0.07430190  0.28355171 1.0000000

# Since V4 i.e. Humidity outside kitchen seems to have way less correleation I will ignore that
# variable from this point onwards.
selectedData = subsetData[,-4]

# before transformation checking the skewness of each variables

library(e1071)
apply(selectedData, 2, skewness)
#        V1         V2         V3         V5         V6 
#   0.2841859  0.2500231 -0.1818761  0.7229380  1.6146097 

scaleData = function (temp) {
  xScaled = (temp - min(temp)) / (max(temp) - min(temp))
  
  return (xScaled)
}

# Applying cuberoot transformation on x1, x2 and x4 as they seems to be less skewed.
# x3 is not normalized as this variable is not very skewed originally and applying transformation
# such as log, square root, cuberoot only made the skewness worse.
# For x5, I used log transformation as it was heavely skewed.
v1 = selectedData [,1] ^ (1/3)
v2 = selectedData [,2] ^ (1/3)
v3 = selectedData [,3]
v4 = selectedData [,4] ^ (1/3)
v5 = log (selectedData[,5])

normalizedData = list('x1' = v1, 'x2' = v2, 'x3' = v3, 'x4' = v4, 'x5' = v5)

normalizedData = as.data.frame(normalizedData)

scaledData = apply(normalizedData, 2, scaleData)

apply(normalizedData, 2, skewness)
#     X1          X2         X3           X4           X5
# 0.15144969  0.05133773 -0.18187609  0.05201029  0.33366416

write.table(scaledData, "SAJESH-transformed.txt", col.names = FALSE, append = FALSE, sep=",", row.names = FALSE)

# Task 3.i 
#
source(file = "./AggWaFit718.R")

#Task 3.ii  -> Models
# output directory is required in the project directory
fit.QAM(normalizedData, "./output/output.txt", "./output/status.txt" )
fit.QAM(normalizedData, "./output/output1.txt", "./output/status1.txt", g=PM05, g.inv = invPM05)
fit.QAM(normalizedData, "./output/output2.txt", "./output/status2.txt", g=QM, g.inv = invQM)
fit.OWA(normalizedData, "./output/output3.txt", "./output/status3.txt")
fit.choquet(normalizedData, "./output/output4.txt", "./output/status4.txt")

#Task 4.i
input = scaleData(sqrt(c(18,44,4,74.8)))
weights = c(0.550409014516637,0.214286745589743,0.120432080374135,0.114872159519482)

cweights = c(0
             ,0
             ,0
             ,0.193637956140186
             ,0.403822529954561
             ,0.279041769700155
             ,0.725165889386119
             ,0.0212347951826433
             ,0.0212347951826433
             ,0.0212347951826433
             ,0.837194766749031
             ,0.420888145066684
             ,0.745010462552994
             ,0.420888145066693
             ,0.999999999999913)

QAM(input, weights)

choquet(input, cweights)
