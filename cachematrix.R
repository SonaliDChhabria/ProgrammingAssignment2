## Matrix inversion is a costly computation and hence it is beneficial to cache it's inverse 
## rather than compute it repeatedly

## makeCacheMatrix creates a special matrix object that can cache it's inverse
## It is a list containing a function to -
## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the Inverse
## 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse )
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix hasn't changed then 
## cacheSolve should get the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
