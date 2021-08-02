#Computing an inverted matrix and cache it.


#Creates a matrix that caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
  }

###############################################################

# Calculates the inverse of matrix x or 
# returns the already computed inverse matrix if it has been already cached

cacheSolve <- function(x, ...) {
     
    inv <- x$getInv()
    if (!is.null(inv)) {
      message("Returning cached inverse matrix")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}

################################################################
# Run function with results

# x <- matrix(c(2, 1, 0, -1,2,-1,0,-2,1), nrow = 3, ncol = 3, byrow = FALSE)
# y <- makeCacheMatrix(x)



# First: cacheSolve(y)

# [,1] [,2] [,3]
# [1,] -1.110223e-16    1    2
# [2,] -1.000000e+00    2    4
# [3,] -1.000000e+00    2    5



# Second: cacheSolve(y)

# getting cached data
# [,1] [,2] [,3]
# [1,] -1.110223e-16    1    2
# [2,] -1.000000e+00    2    4
# [3,] -1.000000e+00    2    5
