## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Here the "special" matrix is built so can storage the calculated inverse as cache
makeCacheMatrix <- function(x = matrix()) {
  #Initialize with NULL where the inverse will be store
  s <- NULL
  #Set the original matrix to solve, and set S to NULL assumming is a new matrix
  #so there is no cache
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  #Get the original matrix
  get <- function() x
  #Set the result of the solve, and store it in cache
  setsolve <- function(solve) s <<- solve
  #Get the solve from cache
  getsolve <- function() s
  #Return the object
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## Will look for the result in the cache if it is empty we proceed with the calc
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Look the result in the cache
  i<-x$getsolve()
  # if is found is returned
  if(!is.null(i)){
    message("this is cache")
    return(i)
  }
  # get the data to solve the matrix
  data <- x$get()
  # here is where the matrix is inverse
  s <- solve(data, ...)
  # storage the result in the cache
  x$setsolve(s)
  #return the inverse
  s
}


