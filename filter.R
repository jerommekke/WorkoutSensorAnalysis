
## The following code is the code as proposed by google on the android docs
## https://developer.android.com/reference/android/hardware/SensorEvent.html#values
## Note that it's dependent on dT, which in our case may even differ for different datasets

# // alpha is calculated as t / (t + dT)
# // with t, the low-pass filter's time-constant
# // and dT, the event delivery rate
# 
# final float alpha = 0.8;
# 
# gravity[0] = alpha * gravity[0] + (1 - alpha) * event.values[0];
# gravity[1] = alpha * gravity[1] + (1 - alpha) * event.values[1];
# gravity[2] = alpha * gravity[2] + (1 - alpha) * event.values[2];
# 
# linear_acceleration[0] = event.values[0] - gravity[0];
# linear_acceleration[1] = event.values[1] - gravity[1];
# linear_acceleration[2] = event.values[2] - gravity[2];

## Looking at the equations, basically it's saying:
## for any time-value a and gravity g, and the next time-step is g'
## thus (g'-g)/dt = dg/dt
## in the following we take c as the filter constant so as not to confuse it with time
## g' = alpha * g + (1-alpha)* a
## g' = c/(c+dT) * g + (1- alpha) * a
## g'(c+dT) = c*g + c*a
## c*(g'-g) + dT*g' = c*a
## c*(g'-g)/dT + g' = c*a/dt
## dg/dt = -g'/c + c*a/dt
## don't know that that makes sense

updateGravity <- function(v) {
  # TODO: 
  # i think the bottom loop should actually be made nice by simply using 
  # apply() with this function
  # i just don't know what the function is yet :D 
  # (in practice: how do I store something persistently or pass extra data through apply)
}

## example on running:
#data <- extractExercise("sensor_data_2016_10_13_20_26_40.csv", "screen up")
#acc <- test[["acc"]]
#v <- cbind(acc$x, acc$y, acc$z) # make vector

filterbank <- function(v) {
  # This should seperate out slow-moving stuff from fast moving stuff
  
  
  # Initial assumption of gravity is:
  g <- c(0, 0, 9.81)
  
  #g' = alpha *g + (1-alpha)*a
  alpha <- 0.8
  
  slow <- v;
  fast <- v;
  
  for (i in 1:nrow(v)) {
    slow[i,] <- alpha*g + (1-alpha)*v[i,]
    g <- slow[i,]
    
    fast[i,] <- v[i,] - slow[i,]
  }
    
  decomp <- list(slow = slow, fast = fast)
    
  return(decomp)
}