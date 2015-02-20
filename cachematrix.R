## These functions cache a matrix and its inverse in order for the inverse not
## to be calculated each time when it is used


## This function creates 4 functions that are called inside the next function:
## 1) set-> cache the matrix x
## 2) get-> to get x
## 3) setinverse-> determine the inverse of x
## 4) getinvers-> get the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the cached matrix (x in makeCacheMatrix) if the contents of x
## have not changed
## If the contents of x changed, the inverse of the new matrix is determined and returned

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("geting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   
}




