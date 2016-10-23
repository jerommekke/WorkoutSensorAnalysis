
#data <- extractExercise("sensor_data_2016_10_13_20_26_40.csv", "screen down")
setwd('~/Development/personal/data/jamahl/')
listExercises("sensor_data_2016_10_12_19_55_07.csv")

data <- extractExercise("sensor_data_2016_10_12_19_55_07.csv", "side raises")
acc <- data[["acc"]]
v <- cbind(acc$x, acc$y, acc$z) # make vector
t <- acc$t

tmin <- 80
tmax <- 130 

plot(t[t >= tmin & t < tmax], v[t >= tmin & t < tmax,2], type = 'l')

filtered <- splitGravity(v, t, 0.4)

# get gravity and acceleration
g <- filtered[["slow"]]
a <- filtered[["fast"]] # is now the gravity-corrected acceleration

plot(t[t >= tmin & t < tmax], a[t >= tmin & t < tmax, 2], type = 'l', col = 'blue')

# smooth a
as <- a
for (dir in c(1,2,3))
  as[,dir] <- filter(a[,dir], rep(1,20)/20)

plot(t[t >= tmin & t < tmax], a[t >= tmin & t < tmax, 2], type = 'l')
lines(t[t >= tmin & t < tmax], as[t >= tmin & t < tmax, 2], type = 'l', col = 'blue')


# now rotate a to align with gravity
aa <- changeFrameOfReferenceTimeSeries(as, g)

# TODO: since the rotation along the z axis is arbitrary, it is easy for plotting and analysis purposes
# to rotate aa such that most motion in the non-z axis happen all in one (x) axis. 
#aadir <- apply(aa[t < 200 & !is.na(aa[,1]),], 2, sum)

plot(t[t >= tmin & t < tmax], aa[t >= tmin & t < tmax, 3], type = 'l', col = 'red')

# now assume dv/dt = a, thus v' - v = a*dt, v' = v + a*dt
# and do the most stupidest time-integration we can do
tt <- t[is.na(a[,1]) == FALSE]
aa <- a[is.na(a[,1]) == FALSE,]

v <- aa
v[1,] = c(0,0,0)
for (i in 2:nrow(aa))
  v[i,] <- v[i-1,] + aa[i-1,]*(t[i]-t[i-1])


# We can clearly see the integration drift from  here
plot(tt[tt < 200], v[tt < 200, 2], type = 'l')

# get angle
angle <- angleWithGravity(v, g)

# plot
plot(t, angle/pi*180.0, type = 'l')

plot(t[t < 200], angle[t < 200]/pi*180.0, type = 'l')
