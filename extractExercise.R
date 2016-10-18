
listExercises <- function(filename) {
  
  file.data <- read.csv(filename, header = F)
  all.key <- data.matrix(file.data$V2)
  excercises  <- data.matrix(file.data$V3[all.key == "EXC" & data.matrix(file.data$V5) == "START"])
  weights     <- data.matrix(file.data$V4[all.key == "EXC" & data.matrix(file.data$V5) == "START"])
  excs <- unique(excercises)
  
  return(data.frame(excercises, weights))
}

extractExercise <- function(filename, exercisename) {
  
  file.data <- read.csv(filename, header = F)
  
  # First lets read the CSV into something i can work with
  all.x <- data.matrix(file.data$V3)
  all.y <- data.matrix(file.data$V4)
  all.z <- data.matrix(file.data$V5)
  
  all.key <- data.matrix(file.data$V2)
  
  # seperate out accelerometer and gyrometer data
  acc.x <- all.x[all.key == "ACC"]
  acc.y <- all.y[all.key == "ACC"]
  acc.z <- all.z[all.key == "ACC"]
  
  gyr.x <- all.x[all.key == "GYR"]
  gyr.y <- all.y[all.key == "GYR"]
  gyr.z <- all.z[all.key == "GYR"]
  
  # find the exercises
  excercises  <- data.matrix(file.data$V3[all.key == "EXC"])
  # action <- data.matrix(file.data$V5[all.key == "EXC"])
  
  # Convert time into a double value (number of seconds since some beginning, I don't care)
  t <- data.matrix(as.double( strptime(file.data$V1, format ="%Y_%m_%d_%H_%M_%OS" )))
  
  # This gets all unique exercises in the file
  excs <- unique(table(excercises))
  
  # First some hackery, all the screen_up data
  t_begin <- t[all.key == "EXC" & data.matrix(file.data$V5) == "START" & data.matrix(file.data$V3) == exercisename]
  t_end   <- t[all.key == "EXC" & data.matrix(file.data$V5) == "END" & data.matrix(file.data$V3) == exercisename]
  
  # Take off some margin
  t_begin <- min(t_begin) + 3
  t_end <- max(t_end) - 3
  
  t_acc <- t[all.key == "ACC"]
  t_gyr <- t[all.key == "GYR"]
  
  
  t <- t_acc[t_acc > t_begin & t_acc < t_end]
  x <- acc.x[t_acc > t_begin & t_acc < t_end]
  y <- acc.y[t_acc > t_begin & t_acc < t_end]
  z <- acc.z[t_acc > t_begin & t_acc < t_end]
  t <- t - t[1]
  
  acc <- data.frame(t,x,y,z);
  
  t <- t_gyr[t_gyr > t_begin & t_gyr < t_end]
  x <- gyr.x[t_gyr > t_begin & t_gyr < t_end]
  y <- gyr.y[t_gyr > t_begin & t_gyr < t_end]
  z <- gyr.z[t_gyr > t_begin & t_gyr < t_end]
  t <- t - t[1]
  
  gyr <- data.frame(t, x, y, z)
  
  return(acc)
}


