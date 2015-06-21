
# Below are the two functions(i.e., makeCacheMatrix, cacheSolve) that can
# be used to cache the inverse of a matrix. It is assumed that matrix 
# supplied is always invertible.

# "makeCacheMatrix", function creates a "matrix" object that 
# can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL                            # set inverse to NULL
  set <- function(y) {                       # set the matrix
  x <<- y
  inverse <<- NULL
   }
  get <- function() x                        # get the matrix    
  setinv <- function(inv) inverse <<- inv    # set inverse of the Matrix
  getinv <- function() inverse               # get inverse of the matrix
  list(set=set,
       get=get, 
       setinv=setinv, 
       getinv=getinv)
 }


# "cacheSolve", function calculates the inverse of the "matrix" 
# returned by "makeCacheMatrix" function above. 
# If the inverse has already been calculated then 
# it skips the calculation and retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  inverse <- x$getinv()

  if(!is.null(inverse)) {                 # if inverse is already calculated
  message("getting cached data")          # then return it. (cached data)         
  return(inverse)                          
  }
  data <- x$get()                         # if inverse is not already calculated
  inverse <- solve(data)                  # calculate the inverse
  x$setinv(inverse)                       # cache the computed inverse
  inverse                                 # return inverse
 }
