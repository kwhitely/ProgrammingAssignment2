## These functions calculate the inverse of a square matrix, storing the 
##result in cache for faster access

## This function creates a matrix object that can cache its inverse
makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL                              # Sets initial value to null for cacheSolve
  }                                               #function to determine if inverse is in cache
  get <- function() x 
  setinverse <- function(solve) m <<- solve       # Inverse is calculated and stored in cache
  getinverse <- function() m
  list(set = set, get = get,                      # Sets names so values can be called
       setinverse = setinverse,                   
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.  If already computed,
##the function will retrieve the inverse from cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {                               # Displays message if inverse 
    message("getting cached data")          #is already stored in cache
    return(m)                               #and then displays inverse
  }
  data <- x$get()                                 # Gets matrix from get()
  m <- solve(data, ...)                           # Calculates inverse if not in cache
  x$setinverse(m)                                 # Calls setinverse function to store
  m                                               #inverse in cache
}
