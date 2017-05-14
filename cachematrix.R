## makeCacheMatrix creates a special matrix that can cache its inverse 
## and cacheSolve checks if the inverse has been cached, returns the 
## inverse if it has and calculates/returns it if not

## Builds a set of functions, set(), get(), setinverse() and getinverse()
## returns as a list in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates or retrieves (if cached) the inverse of an object of type
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  print(x)
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinverse(s)
  s

        ## Returns a matrix that is the inverse of 'x'
}
