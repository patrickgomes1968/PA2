## These two functions act in conjunction to find the inverse of
## an inputted (square) matrix. 
## While the 'Solve' fucntion in R accomplishes this handily enough,
## it can have large computational overheads in doing so with 
## large matrices. The code below relies on the lexcical scoping
## feature of R, using  the <<- operator which
## can be used to assign a value to an object in an environment 
## that is different from the current environment.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    if(!(nrow(i)==ncol(i))) {
        message("The Matrix isn't a Square! Exiting...")
        stop()
    } else { ##go ahead and find the inverse
        data <- x$get()
        i <- Solve(data, ...)
        x$setinverse(i)
        i
    }
}
