makeCachematrix <- function(x = matrix())-{
    inv <- NULL 
  set <- function (y) {
    x <<- y
    inv <<- NULL
    }
    get <- function() {x}
    setInverse ,- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }
    
cacheSOLVE <- FUNCTION(x, ...) (
     inv ,- x$getInverse()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
    }
    mat <- x$get()
    in <- solve(mat, ...)
    x$setInverse(inv)
    inv
    
}
