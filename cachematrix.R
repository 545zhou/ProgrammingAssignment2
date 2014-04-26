# the following two functions are used to cache the inverse of a matrix

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                    # i is the inverse
  set <- function(y) {         # to set the data
    x <<- y
    i <<- NULL
  }
  get <- function() x          # to get the data
  setinverse <- function(inverse) i <<- inverse   # to set the inverse
  getinverse <- function() i                      # to get the inverse
  list(set = set, get = get,                      # return a list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()               # get inverse
  if(!is.null(i)) {                 # check if there is cached inverse
    message("getting cached data")
    return(i)                       # if there is cached inverse, return the inverse
  }
  data <- x$get()                   # import the data
  i <- solve(data, ...)             # calculate the inverse
  x$setinverse(i)                   # set the inverse
  i                                 # return the inverse
}