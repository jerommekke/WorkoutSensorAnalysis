
# Compute the vector cross product between u and v
# see https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
crossProduct3D <- function(u, v) {
  #Using column vectors, we can represent the same result as follows:
  return( c(u[2]*v[3] - u[3]*v[2], 
            u[3]*v[1] - u[1]*v[3], 
            u[1]*v[2] - u[2]*v[1]) )
}


changeFrameOfReference <- function(v, reference, dir = 3) {
  # returns a vector v' such that v' represents the motion in a rotated frame of reference where
  # reference is rotated to align with the the provided direction
  
  # Get the vector that defines the plane in which the old and new reference lie
  cref      <- c(0,0,0)
  cref[dir] <- 1
  
  # normalise reference dir
  reference <- reference / normOfVector(reference)
  
  # now get the rotation beteen reference and required direction
  angle <- angleBetweenVectors(cref, reference)
  
  # get the unit angle for the plane. Note that it's normalised by dividing cref with sin(angle)
  plane <- crossProduct3D(reference,cref/sin(angle))
  
  # establish rotation matrix 
  # see https://en.wikipedia.org/wiki/Rotation_matrix
  # "Rotation matrix from axis and angle"
  # R=\cos \theta \mathbf {I} +\sin \theta [\mathbf {u} ]_{\times }+(1-\cos \theta )\mathbf {u} \otimes \mathbf {u} ~,
  tensor <- plane %*% t(plane)
  crossProdMatrix <- reference %*% t(cref/sin(angle))
  crossProdMatrix <- crossProdMatrix - t(crossProdMatrix)
  
  R <- cos(-angle)*diag(3) + sin(-angle)*crossProdMatrix + (1-cos(-angle))*tensor
  
  # TODO: these checks can go away once we've build enough confidence
  # they're just testing we do what I expect
  if (abs(abs(det(R))  - 1) > 1e-3) {
    print("Warnig: det R != 1")
  }
  if ( abs((R %*% reference)[3] - 1) > 1e-3 )
  {
    print("Error in rotation")
  }
  
  # now rotate v
  v <- R %*% v
  
}

changeFrameOfReferenceTimeSeries <- function(v, reference, dir = 3) {
  # same as above, now with v and reference time series
  for (i in 1:nrow(v)) {
    v[i,] <- changeFrameOfReference(v[i,], reference[i,], dir)
  }
  return(v)
}

