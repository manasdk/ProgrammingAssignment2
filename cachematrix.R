## The two supplied functions used in conjunctions create an ability
## to cache the inverse of a matrix to avoid repeated computation.
## The general applicability of this mechanism is to reduce costly
## redundant computations.

## Usage -
## cached_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4, 5, 6),
##                                         nrow=3,
##                                         ncol=2))
## inv_matrix_1 <- cacheSolve(cached_matrix)
## inv_matrix_2 <- cacheSolve(cached_matrix)
##

## Wraps the supplied matrix in an object that is capable
## of storing inverse of the matrix.
##
## Object provides following methods -
## 1. get - retrieve the original matrix.
## 2. set - assign the matrix to be inverted.
## 3. getInverse - retrieve the cached inverse.
## 4. setInverse - set the inverse to be cached.
## as well as get and set the
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y) {
            x <<- y
            m_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m_inverse <<- inverse
    getInverse <- function() m_inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Attempts to retrieve the inverse from a previously cached
## computation. If no cached solution is found will compute
## inverse and save it in the supplied wrapper object.
cacheSolve <- function(m_wrapper, ...) {
      # First try to get the cached value and return that
      # if available.
      m_inverse <- m_wrapper$getInverse()
      if(!is.null(m_inverse)) {
              message("getting cached data")
              return(m_inverse)
      }

      # Since a previous computation is not found pull out
      # original matrix and compute inverse.
      m <- m_wrapper$get()
      m_inverse <- solve(m, ...)

      # Save inverse for future consumption.
      x$setInverse(m_inverse)
      m_inverse
}
