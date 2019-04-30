# Task 1.ii -> reading the data as a table and converting to matrix using matrix function
data = as.matrix(read.table('./Energy19.txt'))

# Task 1.iii -> subset of 300 data from all the data provided
subsetData = data[1:300,]

# Task 1.iv -> Scatter plots and histograms
plot(subsetData[,6],  subsetData[,1], type = "p", ylab = "Energy use of appliances in Wh", xlab = "Temperature in kitchen (Celcius)", col = "blue", pch = 4 )

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
# Selecting only temperatures and humidity inside and outside the kitchen and variable of interest Y
# Visibility ignored from this point onwards

scaleData = function (temp) {
  xScaled = (temp - min(temp)) / (max(temp) - min(temp))
  
  return (xScaled)
}

selectedData = subsetData[,-5]
selectedData = log(selectedData + 1)
normalizedData = apply(selectedData, 2, scaleData)
summary(normalizedData)

write.table(normalizedData, "SAJESH-transformed.txt", col.names = FALSE, append = FALSE, sep=",", row.names = FALSE)

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
