## Caching the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix", a list containing a function to:

## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse of the matrix
## 4. get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){ x }
  setinv <- function(inv){ i <<- inv}
  getinv <- function(){ i }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The next function, cacheSolve calculates the inverse of the special "matrix" from the above function.
## It first checks if the inverse has already been calculated.  If it has, it gets the inverse from the
## cache.  Otherwise, it calculates the inverse of the matrix and sets the values via the setinv function.

cacheSolve <- function(x, ...) {
  i <-x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
