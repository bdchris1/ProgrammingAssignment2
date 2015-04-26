## These functions demonstrate the concept of lexical scoping of variables.
## This is useful when storing the result of one function in a variable to be used
## in other functions without having to duplicate processing steps.

##This function generates a matrix and uses the <<- operator to make the matrix available 
## outside the scope of the function in which it was declared (e.g., made available to the 'cacheSolve' function).

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function tests for the presence of a cached matrix.
## If a cached matrix exists (and it is invertible), the solve() function is called to create the inverse

  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }

## Code can be tested from the console as follows:

## 1) call: x <- makeCacheMatrix(matrix(sample.int(10, 10 * 10, TRUE), 10, 10)); generates a 10 x 10 matrix of random values between 1 and 10
## 2) call: cacheSolve(x)

