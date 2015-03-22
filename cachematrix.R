##The functions below are used to create a special 'matrix' object and cache the inverse of that matrix.

##The function makeCacheMatrix creates a 'matrix' object, which is a list of functions, and can cache the matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ##Initialize 'inv' (matrix inverse) to NULL
  set <- function(y) {                    ##Function to set matrix 'x'
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {                     ##Function to get matrix 'x'
    x
  }
  
  setInverse <- function(inverse) {       ##Function to set the inverse of matrix in the cache
    inv <<- inverse
  }
  
  getInverse <- function() {              ##Function to get the inverse of matrix
    inv
  }
  
  list(set = set, get = get,              ##List of functions defined in makeCacheMatrix function
       setInv = setInverse, 
       getInv = getInverse)
}


##The function cacheSolve calculates and returns the inverse of 'matrix' object created with the above makeCacheMatrix. 
##If the inverse has already been calculated, computation is skipped and returns the cached inverse matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()                     ##Gets the matrix inverse from makeCacheMatrix function
  if(!is.null(inv)) {                   ##If the inverse has already been calculated (!=NULL), returns cached inverse matrix
    message("getting cached data")
    return(inv)
  }
  else {
    mat <- x$get()                      ##If inverse not computed, assigns matrix 'x' from above to 'mat' 
    inv <- solve(mat, ...)              ##Calculates the inverse of matrix 
    x$setInv(inv)                       ##Sets the inverse of matrix in the cache
    inv                                 ##Returns matrix inverse
  }    
}

