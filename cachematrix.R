## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" with a list. Steps -
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function calculates the inverse of the special
## "matrix" created with the above function. Steps:
## 1. First check to see if inverse is already calculated
## 2. If yes, get inverse from cache
## 3. Otherwise, calculate the inverse and set the value of the inverse in the ## cache

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setInv(m)
  m
}
