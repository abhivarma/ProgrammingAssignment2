## Put comments here that give an overall description of what your
## functions do

## The goal of these functions("makeCacheMatrix","cacheSolve") is to cache the inverse of a matrix.


## Write a short comment describing this function

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inver <<- inverse
  getinv <- function() inver
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## "cacheSolve" function computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" function written above. If the inverse for an unchanged matrix has already been calculated the cachesolve should be retrieving the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  if(!is.null(inver)) {
    message("getting cached result")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinv(inver)
  inver
}


##Sample Input:

## m <- matrix(rnorm(9),3,3)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

##Output:

##          [,1]       [,2]     [,3]
##[1,] -4.011860 -3.1399890 6.686528
##[2,] -2.643075 -1.9202345 3.482559
##[3,] -1.195063 -0.4296807 2.040335
