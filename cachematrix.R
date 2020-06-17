## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #Parent Function

  inv <- NULL ##Single arrow operator works in the current level
  set <- function(y) {  ##Setting the value of the Matrix
    x <<- y ##Double arrow operator modifies variiables at the parent levels
    inv <<- NULL
  }
  get <- function() x #Get the value of the Matrix
  seti <- function(solvem) inv <<- solvem ##Set the value of the Inverse
  geti <- function() inv ##Get the value of the Inverse
  list(set = set, get = get,
       seti = seti,
       geti = geti)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        
  inv <- x$geti () ##Returns matrix inverse of x
  if(!is.null(inv)){ ##If inverse has been calculated it can get it from the cache
      message("getting cached data")
      return(inv)
  }
  matrix <-x$get() #otherwise we will calculate the inverse
  inv <- solve(matrix,...)
  x$seti(inv)
  inv
  
}
