
# magnitude of the vectors
normOfVector <- function(x) sqrt(sum(x^2))

## We know that the angle between two vectors is given by
## v1 . v2 = ||v1|| * ||v2|| cos(angle)
## where . the dot product
## so angle = arccos( v1.v2 / ||v1||*||v2||)
angleBetweenVectors <- function(v1, v2) {
  # requires v1 and v2 to both be an nx3 matrix
  
  v1_norm <- normOfVector(v1)
  v2_norm <- normOfVector(v2)
  
  # the inproduct is 
  in_prod <- sum(v1*v2)
  
  # angle
  angle <- acos( in_prod / (v1_norm*v2_norm) )
  
  return(angle)
}
