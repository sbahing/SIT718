source(file = "./AggWaFit718.R")

# Task 1.ii -> reading the data as a table and converting to matrix using matrix function
data = as.matrix(read.table('./Energy19.txt'))

# Task 1.iii -> subset of 300 data from all the data provided
subsetData = data[sample(1:671, 300),c(1:6)]

# Task 1.iv -> Scatter plots and histograms
plot(subsetData[,6],  subsetData[,1], type = "p", ylab = "Energy use of appliances in Wh", xlab = "Temperature in kitchen (Celcius)", col = "blue", pch = 4 )

plot(subsetData[,6], subsetData[,2], type = "p", xlab = "Energy use of appliances in Wh",  ylab = "Humidity in kitchen", col = "blue", pch = 4 )

plot(subsetData[,6], subsetData[,3], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Temperature outside of kitchen (Celcius)", col = "blue", pch = 4)

plot(subsetData[,6], subsetData[,4], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Humidity outside", col = "blue", pch = 4 )

plot( subsetData[,6], subsetData[,5], type = "p", xlab = "Energy use of appliances in Wh", ylab = "Visibility", col = "blue", pch = 4 )

#Histograms
hist(subsetData[,1], main = "Histogram", xlab = "Temperature in kitchen", col = "skyblue")
hist(subsetData[,2], main = "Histogram", xlab = "Humidity in kitchen", col = "skyblue")
hist(subsetData[,3], main = "Histogram", xlab = "Temperature outside", col = "skyblue")
hist(subsetData[,4], main = "Histogram", xlab = "Humidity outside", col = "skyblue")
hist(subsetData[,5], main = "Histogram", xlab = "Visibility", col = "skyblue")
hist(subsetData[,6], main = "Histogram", xlab = "Energy use", col = "skyblue")

#Task 2.i -> Data transformation

