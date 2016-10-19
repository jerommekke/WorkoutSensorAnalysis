
#data <- extractExercise("sensor_data_2016_10_13_20_26_40.csv", "screen down")
data <- extractExercise("sensor_data_2016_10_12_18_56_02.csv", "squat")
acc <- data[["acc"]]
v <- cbind(acc$x, acc$y, acc$z) # make vector
t <- acc$t

filtered <- filterbank(v)

# get gravity and acceleration
g <- filtered[["slow"]]
a <- filtered[["fast"]]

# get angle
angle <- angleWithGravity(a, g)

# plot
plot(t, angle, type = 'l')

