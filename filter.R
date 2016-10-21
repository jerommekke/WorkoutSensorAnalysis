
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

##TODO: Make dt-dependent
filterbank <- function(v, filterConst) {
  # This should seperate out slow-moving stuff from fast moving stuff
  
  # Initial assumption of gravity is:
  gravConst <- 9.81
  g <- c(0, 0, gravConst)
  
  #g' = alpha *g + (1-alpha)*a
  alpha <- filterConst
  
  slow <- v;
  fast <- v;
  
  for (i in 1:nrow(v)) {
    slow[i,] <- alpha*g + (1-alpha)*v[i,]
    g <- slow[i,]
  }
  
  # Now correct this approximation as we know that ||slow|| = 9.81
  # This only corrects magnitude though
  # TODO: is there a way to correct for angle, too?
  correctToGrav <- function(v){ 
      v <- v * gravConst/normOfVector(v)
    }
  
  slow <- apply(slow, 1, correctToGrav)
  # for some reason ,this transposed slow? wtf?
  slow <- t(slow)
  
  
  ## The above assumes that all acceleration that is slow enough to be
  # filtered out together with gravity must be acceleration in the direction of the
  # gravity vector. That is probably not the most accurate for use in a workout
  # detection algorithm, where we can have slow movements at an angle with gravity
  # A possibly better (i.e. less strict) assumption would be to only assume that the 
  # gravity vector lies in the plane that is spanned by the slow moving 
  # vector and the fast moving vector. Since we also require that a = a' + g
  # where a' is the corrected motion, we have (where f is fast and s is slow)
  #
  # g = (gamma)*f + (1-gamma)*s
  # where gamma in the range (0,1) and where we know the magnitude of ||g||, so that
  # we can use the law of cosines
  # ||g||^2 = ||f||^2*gamma^2 + (1-gamma)^2*||s||^2 - 2*||f||*||s||*cos(angle)
  # where the angle is the angle between f and s
  # that gives us
  # ||g||^2 = ||f||^2*gamma^2 + (1+gamma^2-2*gamma)*||s||^2 - 2*||f||*||s||*cos(angle)
  # or
  # 0 = gamma^2 ( ||f^2|| + ||s^2||) + gamma ( -2*||s||^2 ) + ||s||^2-||g||^2 - 
  #                       2*||f||*||s||*cos(angle)
  # which can be solved for gamma using the quadratic equation to give:
  # gamma = 2*||s||^2 +/- sqrt( 4*||s||^4 - 4*(||f||^2+||s||^2)*
  #                                         (||s||^2-||g||^2-2*||f||*||s||*cos(angle)) )
  #             / 2*( ||f^2|| + ||s^2||) 

  
  
  # now substract gravity from the signal to get the movement
  fast <- v - slow
    
  decomp <- list(slow = slow, fast = fast)
    
  return(decomp)
}