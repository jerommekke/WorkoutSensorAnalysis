
# magnitude of the vectors
normOfVector <- function(x) sqrt(sum(x^2))

## We know that the angle between two vectors is given by
## v1 . v2 = ||v1|| * ||v2|| cos(angle)
## where . the dot product
## so angle = arccos( v1.v2 / ||v1||*||v2||)
angleWithGravity <- function(v, g) {
  # requires v and g to both be an nx3 matrix
  
  v_norm <- apply(v, 1, normOfVector)
  g_norm <- apply(g, 1, normOfVector)
  
  # the inproduct is 
  in_prod <- apply(v*g, 1, sum)
  
  # angle
  angle <- acos( in_prod / (v_norm*g_norm) )
  
  return(angle)
}
