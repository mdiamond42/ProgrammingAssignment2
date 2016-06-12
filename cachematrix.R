## These functions allow the inverse of a matrix to be cached
## 

## this function creates a list of functions and used cached variables to return
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                if(!is.matrix(y))return("Error: Argument is not a matrix")
                x <<- y
                m <<- NULL
                if(nrow(x)!=ncol(x)) "Warning: There is no Inverse for this Matrix"
        }
        get <- function() x
        setinverse <- function(inver) m <<- inver
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function returns the inverse matrix or calls the setinverse for the 
##make cachematrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
