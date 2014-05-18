## These two functions are calculating the inverse of a
## matrix. What is special here is that, if the inverse of the
## matrix is calculated once before, the functions directly
## read the result from cache instead of recalculating.

## The makeCacheMatrix() funtion creates an artificial list
## including a function to: 1) set the value of the matrix, 2)
## get the value of the matrix, 3) set the value of the inverse and
## 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Set an empty value to 'm', which is a special object that stores a matrix x and cache's its inverse.
  m <- NULL
  

  set <- function(y) {
    ## Assign the value 'y' to the R object 'x' 
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # calculate the inverse 
  setinverse <- function(inverse) m <<- inverse
  
  # get the value of the inverse
  getinverse <- function() m
  
  # a special "matrix", which is really a list containing above 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve() function calculates the inverse of the CacheMatrix object. 
## It first does some checking. If the inverse has been calculated,
## it copies the result from the cache memory.
## Otherwise, it calculates the inverse of the matrix and sets it 
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # first checks to see if the mean has already been calculated
  m <- x$getinverse()
  
  if(!is.null(m)) { ## if the cache memory is not NULL,
    message("getting cached data") 
    return(m) ## return the value stored in cache
  } 
  
  # Otherwise, calculates the inverse of the matrix and sets it in the cache.
  data <- x$get() 
  
  m <- solve(data, ...) 
  
  x$setinverse(m) 
  
  # Return the inverse of 'x'
  m 
}



