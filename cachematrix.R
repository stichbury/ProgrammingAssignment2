
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y ## store incoming matrix y as x
    inv <<- NULL ## reset inv
  }

  get <- function() x ## return matrix
  setinverse <- function(pInv) inv <<- pInv ## store incoming inverse as inv
  getinverse <- function() inv ## return inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() ## retrieve the data
  inverse <- solve(data) ## solve the inverse
  x$setinverse(inverse) ## cache it
  inverse
}
