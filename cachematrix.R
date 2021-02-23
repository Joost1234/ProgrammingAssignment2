## Functions to inverse invertible matrixes and cache the results

## The function creates a matrix object and can cache its inverse
makeCacheMatrix <- function(m=matrix()){
  inv <- NULL
  set <- function(y){
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  set_inv <- function(i) inv <<- i
  get_inv <- function() inv
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## The function will compute the inverse of the matrix, returned by the makeCacheMatrix function. 
## If the inverse of the matrix has already been calculated, the inverse of the cache is retrieved.
cacheSolve <- function(s_m,...){
 inv <- s_m$get_inv()
 if(!is.null(inv)){
   message("getting cached data")
   return(inv)
 }
 data <- s_m$get()
 inv <- solve(data,...)
 s_m$set_inv(inv)
 inv
}
