## Programmin Assignment 2
## Assignment: Caching the Inverse of a Matrix
## There are two functions:
## 1.makeCacheMatrix 
## 2.cacheSolve

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
  inv <- matrix()
## Initally initializes inv to a empty matrix
  set_matrix <- function(y)
  { 
## This function assigns the value for the matrix whose inverse has to be determined.
    x<<-y
    inv <- matrix()
## Initally initializes inv to a empty matrix if the elements of the Matrix are changed. 
  }
  get_matrix <- function() x
## Gets the Matrix.
  set_inv <- function(invers) inv <<- invers
## Sets the Inv to invers.
  get_inv <- function() inv
## Prints the Inverse of a matrix.
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv)

}


cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache.
  inv <- x$get_inv() ## Gets the inverse.
  if(!is.na(inv[1,1])) ## If the Inverse is already computed or calculated then 
                       ## it retrive hte inverse form cache  
    {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix() ## Get the matrix
  inv <- solve(data, ...) ## Calculate its inverse
  x$set_inv(inv) ##Set the Inverse to inv
  inv ## returns the value
}
