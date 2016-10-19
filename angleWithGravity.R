
## We know that the angle between two vectors is given by
## v1 . v2 = ||v1|| * ||v2|| cos(angle)
## where . the dot product
## so angle = arccos( v1.v2 / ||v1||*||v2||)
angleWithGravity <- function(v, g) {
  # requires v and g to both be an nx3 matrix
  
  # pre-allocate angle
  angle <- v[,1]
  
  # magnitude of the vectors
  norm_vec <- function(x) sqrt(sum(x^2))
  
  v_norm <- apply(v, 1, norm_vec)
  g_norm <- apply(g, 1, norm_vec)
  
  # the inproduct is 
  in_prod <- apply(v*g, 1, sum)
  
  # angle
  angle <- acos( in_prod / (v_norm*g_norm) )
  
  return(angle)
}
